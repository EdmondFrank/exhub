defmodule Exhub.MCP.Tools.I2I do
  @moduledoc """
  MCP Tool for image-to-image generation (i2i) via Gitee AI.

  Generates a new image guided by one or more existing images plus a text
  prompt. Mirrors the GenClaw `i2i` tool (`Exhub.Genclaw.Tools.I2I`) but is
  exposed as a standalone MCP tool on the image-gen server, so it shares the
  `image_gen` model set, tuning params and response shape.

  Reference images may be URLs (`https://…`), base64 `data:` URIs, or absolute /
  `~` local file paths — they are resolved by `Exhub.MCP.ImageSource`.

  ## Backend paths

    * `qwen-image-2.0` / `qwen-image-2.0-pro` — the reference images are inlined
      as base64 data URLs on `POST /v1/images/generations` (`images: [...]`),
      which supports multi-image guidance (e.g. a subject plus a clothing
      reference).
    * other models — single-image editing via the multipart
      `POST /v1/images/edits` endpoint.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.ImageSource
  alias Exhub.MCP.Tools.ImageGen

  use Anubis.Server.Component, type: :tool

  @generations_url "https://api.moark.com/v1/images/generations"
  @edits_url "https://api.moark.com/v1/images/edits"

  # Models that take the reference images as base64 data URLs on the generations
  # endpoint (multi-image guidance).
  @multi_image_models ~w(qwen-image-2.0 qwen-image-2.0-pro)

  # Multi-image models whose generations endpoint rejects the `size` param
  # (HTTP 400 "参数无效 'size'"); the server default is used instead.
  @no_size_models ~w(qwen-image-2.0-pro)

  @default_model "qwen-image-2.0"
  @default_size "1024x1024"

  @request_timeout_ms 180_000
  @boundary_random_max 1_000_000_000

  def name, do: "i2i"

  @impl true
  def description do
    """
    Generate a new image guided by one or more existing images plus a text prompt
    (image-to-image / image editing), using Gitee AI's image API.

    Use it to restyle an existing image, transfer clothing / colour / background
    from a reference, blend a subject with a reference, or iterate on a prior
    generation. Returns the generated image as a URL (or base64); display it with
    markdown: `![Generated Image](URL)`.

    **Inputs:**
    - `image_path` (required) — primary/source image.
    - `reference_image_paths` (optional) — extra reference images for multi-image
      guidance; do NOT repeat `image_path` here.
      Both accept a URL (`https://…`), a base64 `data:` URI, or an absolute / `~`
      local file path (local files over 2 MB are downscaled automatically).

    **Models:** `qwen-image-2.0` (default) and `qwen-image-2.0-pro` are
    multi-image capable. Any other `image_gen` model is also accepted but edits a
    single image (the primary one).

    **Tuning:** `size`, `negative_prompt`, `guidance_scale`,
    `num_inference_steps` and `seed` follow the same semantics as `image_gen`.
    """
  end

  schema do
    field(:image_path, {:required, :string},
      description:
        "Primary/source image. A URL (https://...), a base64 data URI, or an absolute / ~ local file path."
    )

    field(:prompt, {:required, :string},
      description:
        "Guidance for the resulting image — be specific about what should change and what should stay."
    )

    field(:reference_image_paths, {:list, :string},
      description:
        "Extra reference images for multi-image guidance (do NOT include image_path here). URLs, data URIs, or absolute / ~ local paths."
    )

    field(:model, :string,
      description:
        "Model to use. Default: qwen-image-2.0 (multi-image capable). Any other image_gen model edits the single primary image."
    )

    field(:size, :string, description: "Output image size. Default: 1024x1024.")

    field(:negative_prompt, :string, description: "Elements to avoid in the generated image.")

    field(:guidance_scale, {:either, {:integer, :float}},
      description: "How closely the model follows the prompt (float)."
    )

    field(:num_inference_steps, :integer,
      description: "Number of denoising steps. Higher = better quality but slower."
    )

    field(:seed, :integer, description: "Random seed for reproducible generation.")

    field(:quality, :string,
      description:
        "Reserved. Currently has no effect (kept for parity with the GenClaw i2i tool)."
    )
  end

  @impl true
  def execute(params, frame) do
    api_key = Application.get_env(:exhub, :giteeai_api_key, "")

    cond do
      api_key == "" ->
        error(
          frame,
          "Gitee AI API key not configured. Run: mix scr.insert dev giteeai_api_key \"your-key\""
        )

      true ->
        with {:ok, validated} <- validate_params(params),
             {:ok, sources} <- resolve_sources(validated) do
          generate(validated, sources, api_key, frame)
        else
          {:error, reason} -> error(frame, reason)
        end
    end
  end

  # ---------------------------------------------------------------------------
  # Public, pure helpers (unit-tested)
  # ---------------------------------------------------------------------------

  @doc """
  Validates and normalizes `i2i` params.

  Returns `{:ok, validated}` with keys `:image_path`, `:prompt`,
  `:reference_image_paths`, `:model`, `:size`, `:negative_prompt`,
  `:guidance_scale`, `:num_inference_steps`, `:seed`, or `{:error, message}`.
  """
  @spec validate_params(map()) :: {:ok, map()} | {:error, String.t()}
  def validate_params(params) when is_map(params) do
    image_path = Map.get(params, :image_path)
    prompt = Map.get(params, :prompt)
    model = normalize_string(Map.get(params, :model), @default_model)
    size = normalize_string(Map.get(params, :size), @default_size)

    cond do
      not present?(image_path) ->
        {:error,
         "`image_path` is required (a URL, a data URI, or an absolute / ~ local file path)."}

      not present?(prompt) ->
        {:error, "`prompt` is required."}

      model not in ImageGen.valid_models() ->
        {:error,
         "Invalid model: #{model}. Valid models: #{Enum.join(ImageGen.valid_models(), ", ")}"}

      true ->
        trimmed_path = String.trim(image_path)

        {:ok,
         %{
           image_path: trimmed_path,
           prompt: prompt,
           reference_image_paths:
             normalize_refs(Map.get(params, :reference_image_paths), trimmed_path),
           model: model,
           size: size,
           negative_prompt: Map.get(params, :negative_prompt),
           guidance_scale: Map.get(params, :guidance_scale),
           num_inference_steps: Map.get(params, :num_inference_steps),
           seed: Map.get(params, :seed)
         }}
    end
  end

  @doc """
  Resolves every reference image into a value the API can fetch.

  Returns `{:ok, sources}` where `sources` is a list of data URIs / URLs in order
  (`image_path` first, then `reference_image_paths`), or `{:error, message}` when
  a source cannot be resolved.
  """
  @spec resolve_sources(map()) :: {:ok, [String.t()]} | {:error, String.t()}
  def resolve_sources(%{image_path: image_path, reference_image_paths: refs}) do
    [image_path | refs]
    |> Enum.reduce_while({:ok, []}, fn source, {:ok, acc} ->
      case ImageSource.resolve(source) do
        {:ok, resolved} ->
          {:cont, {:ok, [resolved | acc]}}

        {:error, reason} ->
          {:halt, {:error, "Failed to resolve image source #{inspect(source)}: #{reason}"}}
      end
    end)
    |> case do
      {:ok, list} -> {:ok, Enum.reverse(list)}
      other -> other
    end
  end

  @doc """
  Builds the JSON body for the multi-image generations endpoint.
  """
  @spec build_generations_body(map(), [String.t()]) :: map()
  def build_generations_body(validated, sources) do
    %{
      "prompt" => validated.prompt,
      "model" => validated.model,
      "response_format" => "url",
      "images" => sources,
      "extra_body" => ImageGen.build_extra_body(validated.model, drop_nils(validated))
    }
    |> maybe_put("size", generations_size(validated))
    |> maybe_put("seed", validated.seed)
  end

  # Some multi-image models reject the `size` param on the generations endpoint.
  defp generations_size(%{model: model}) when model in @no_size_models, do: nil
  defp generations_size(%{model: model, size: size}), do: ImageGen.normalize_size(size, model)

  @doc """
  Builds the multipart fields for the single-image edits endpoint.

  `file_part` is a `{filename, bytes, content_type}` tuple (see
  `data_uri_file_part/1`).
  """
  @spec build_edit_fields(map(), {String.t(), binary(), String.t()}) :: [
          {String.t(), String.t() | {String.t(), binary(), String.t()}}
        ]
  def build_edit_fields(validated, {filename, bytes, content_type}) do
    size = ImageGen.normalize_size(validated.size, validated.model)
    extra_body = ImageGen.build_extra_body(validated.model, drop_nils(validated))

    [
      {"model", validated.model},
      {"prompt", validated.prompt},
      {"size", size},
      {"response_format", "url"}
    ]
    |> Kernel.++(extra_body_fields(extra_body))
    |> maybe_put_field("seed", validated.seed)
    |> Kernel.++([{"image", {filename, bytes, content_type}}])
  end

  @doc """
  Parses a base64 `data:` URI into a multipart file tuple.

  Returns `{:ok, {filename, bytes, mime}}` or `{:error, message}`.
  """
  @spec data_uri_file_part(String.t()) ::
          {:ok, {String.t(), binary(), String.t()}} | {:error, String.t()}
  def data_uri_file_part(data_uri) when is_binary(data_uri) do
    case String.split(data_uri, ",", parts: 2) do
      ["data:" <> meta, payload] ->
        mime =
          meta
          |> String.split(";")
          |> List.first()
          |> normalize_mime()

        case Base.decode64(payload) do
          {:ok, bytes} -> {:ok, {"image" <> mime_ext(mime), bytes, mime}}
          :error -> {:error, "Invalid base64 payload in data URI."}
        end

      _ ->
        {:error, "Not a base64 data URI."}
    end
  end

  def data_uri_file_part(_other), do: {:error, "Not a base64 data URI."}

  # ---------------------------------------------------------------------------
  # Request dispatching
  # ---------------------------------------------------------------------------

  defp generate(%{model: model} = validated, sources, api_key, frame) do
    if model in @multi_image_models do
      generate_multi_image(validated, sources, api_key, frame)
    else
      generate_edit(validated, sources, api_key, frame)
    end
  end

  # qwen-image-2.0: generations endpoint, JSON, images as base64 data URLs.
  defp generate_multi_image(validated, sources, api_key, frame) do
    body_map = build_generations_body(validated, sources)
    body = Jason.encode!(body_map)

    headers = [
      {"Content-Type", "application/json"},
      {"Authorization", "Bearer #{api_key}"}
    ]

    case HTTPoison.post(@generations_url, body, headers, request_opts(@generations_url)) do
      {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
        ImageGen.handle_success(
          resp_body,
          validated.model,
          ImageGen.normalize_size(validated.size, validated.model),
          validated.prompt,
          body_map["extra_body"],
          frame
        )

      {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
        error(frame, "Gitee AI API error (HTTP #{status}): #{resp_body}")

      {:error, %HTTPoison.Error{reason: reason}} ->
        error(frame, "HTTP request failed: #{inspect(reason)}")
    end
  end

  # Other models: multipart edits endpoint with the primary image.
  defp generate_edit(validated, [primary | _], api_key, frame) do
    case resolve_file_part(primary) do
      {:ok, file_part} ->
        fields = build_edit_fields(validated, file_part)

        boundary = "----ExhubI2I#{:rand.uniform(@boundary_random_max)}"
        {body, content_type} = build_multipart(fields, boundary)

        headers = [
          {"Content-Type", content_type},
          {"Authorization", "Bearer #{api_key}"}
        ]

        case HTTPoison.post(@edits_url, body, headers, request_opts(@edits_url)) do
          {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
            ImageGen.handle_success(
              resp_body,
              validated.model,
              ImageGen.normalize_size(validated.size, validated.model),
              validated.prompt,
              ImageGen.build_extra_body(validated.model, drop_nils(validated)),
              frame
            )

          {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
            error(frame, "Gitee AI API error (HTTP #{status}): #{resp_body}")

          {:error, %HTTPoison.Error{reason: reason}} ->
            error(frame, "HTTP request failed: #{inspect(reason)}")
        end

      {:error, reason} ->
        error(frame, reason)
    end
  end

  # ---------------------------------------------------------------------------
  # Private helpers
  # ---------------------------------------------------------------------------

  # Resolves any image source into a `{filename, bytes, content_type}` multipart
  # file part. Local files / data URIs are read directly; URLs are downloaded.
  defp resolve_file_part(source) do
    case ImageSource.resolve(source) do
      {:ok, resolved} when is_binary(resolved) ->
        cond do
          String.starts_with?(resolved, "data:") -> data_uri_file_part(resolved)
          String.starts_with?(resolved, ["http://", "https://"]) -> fetch_file_part(resolved)
          true -> {:error, "Unsupported image source: #{inspect(source)}"}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp fetch_file_part(url) do
    case HTTPoison.get(url, [], request_opts(url)) do
      {:ok, %HTTPoison.Response{status_code: 200, body: bytes, headers: headers}}
      when is_binary(bytes) ->
        content_type = content_type_from_headers(headers) || "image/png"
        {:ok, {url_filename(url, content_type), bytes, content_type}}

      {:ok, %HTTPoison.Response{status_code: status}} ->
        {:error, "Failed to download image (HTTP #{status}): #{url}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Failed to download image: #{inspect(reason)}"}
    end
  end

  defp content_type_from_headers(headers) do
    Enum.find_value(headers, fn {name, value} ->
      if String.downcase(to_string(name)) == "content-type" do
        value |> to_string() |> String.split(";") |> List.first() |> String.trim()
      end
    end)
  end

  defp url_filename(url, content_type) do
    name = url |> URI.parse() |> Map.get(:path) || ""
    name = Path.basename(name)

    if name in ["", "/", "."], do: "image" <> mime_ext(content_type), else: name
  end

  defp build_multipart(fields, boundary) do
    parts =
      Enum.map(fields, fn
        {name, {filename, content, content_type}} ->
          [
            "--#{boundary}\r\n",
            "Content-Disposition: form-data; name=\"#{name}\"; filename=\"#{filename}\"\r\n",
            "Content-Type: #{content_type}\r\n",
            "\r\n",
            content,
            "\r\n"
          ]

        {name, value} ->
          [
            "--#{boundary}\r\n",
            "Content-Disposition: form-data; name=\"#{name}\"\r\n",
            "\r\n",
            value,
            "\r\n"
          ]
      end)

    body = IO.iodata_to_binary([parts, "--#{boundary}--\r\n"])
    content_type = "multipart/form-data; boundary=#{boundary}"
    {body, content_type}
  end

  defp extra_body_fields(extra_body) do
    extra_body
    |> Enum.sort_by(fn {key, _value} -> key end)
    |> Enum.map(fn {key, value} -> {key, to_string(value)} end)
  end

  defp normalize_refs(value, image_path) when is_list(value) do
    value
    |> Enum.filter(&is_binary/1)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == "" or &1 == image_path))
  end

  defp normalize_refs(_value, _image_path), do: []

  # `ImageGen.build_extra_body/2` treats a present-but-nil key as "no value"
  # (so it keeps the model default). Our validated map always carries the
  # optional keys, so strip the nil ones before handing it over.
  defp drop_nils(map) do
    map
    |> Enum.reject(fn {_key, value} -> is_nil(value) end)
    |> Map.new()
  end

  defp normalize_string(value, default) when is_binary(value) do
    case String.trim(value) do
      "" -> default
      trimmed -> trimmed
    end
  end

  defp normalize_string(_value, default), do: default

  defp normalize_mime(mime) when mime in [nil, ""], do: "application/octet-stream"
  defp normalize_mime(mime), do: mime

  defp mime_ext("image/png"), do: ".png"
  defp mime_ext("image/jpeg"), do: ".jpg"
  defp mime_ext("image/webp"), do: ".webp"
  defp mime_ext("image/gif"), do: ".gif"
  defp mime_ext("image/bmp"), do: ".bmp"
  defp mime_ext(_mime), do: ".bin"

  defp present?(value) when is_binary(value), do: String.trim(value) != ""
  defp present?(_value), do: false

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp maybe_put_field(fields, key, value) when is_integer(value) do
    fields ++ [{key, Integer.to_string(value)}]
  end

  defp maybe_put_field(fields, _key, _value), do: fields

  defp request_opts(url) do
    [recv_timeout: @request_timeout_ms, timeout: @request_timeout_ms] ++
      Exhub.TLSCompat.httpoison_opts(url)
  end

  defp error(frame, message) do
    resp = Response.tool() |> Response.error(message)
    {:reply, resp, frame}
  end
end
