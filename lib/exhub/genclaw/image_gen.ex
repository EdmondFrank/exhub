defmodule Exhub.Genclaw.ImageGen do
  @moduledoc """
  Image generation backend for GenClaw t2i and i2i tools.

  ## t2i (text-to-image)
  Uses Gitee AI `POST /v1/images/generations` endpoint.

  ## i2i (image-to-image / image editing)
  Uses Gitee AI `POST /v1/images/edits` endpoint with multipart form data.

  Both return the absolute path of the generated PNG on the local filesystem.
  Raises `RuntimeError` on failure.
  """

  require Logger

  alias Exhub.Genclaw.Session

  @api_base "https://ai.gitee.com/v1"
  @t2i_url @api_base <> "/images/generations"
  @i2i_url @api_base <> "/images/edits"

  @max_retries 3
  @retry_delays [5_000, 15_000, 45_000]

  # Default models
  @default_t2i_model "qwen-image-2.0"
  @default_i2i_model "qwen-image-2.0"
  @default_size "1024x1024"

  # ─── Public API ───────────────────────────────────────────────────────────

  @doc """
  Generate an image from a text prompt (text-to-image).
  Returns the absolute path of the generated PNG.
  """
  def generate_t2i(prompt, opts \\ []) do
    model = Keyword.get(opts, :model, @default_t2i_model)
    size = Keyword.get(opts, :size, @default_size) |> normalize_size(model)

    do_generate_with_retry(fn ->
      body = %{
        prompt: prompt,
        model: model,
        size: size,
        response_format: "url"
      }

      headers = auth_headers(json: true)
      Logger.info("[Genclaw.ImageGen] t2i model=#{model} size=#{size}")

      resp = Req.post!(@t2i_url, json: body, headers: headers, receive_timeout: 120_000)

      handle_response(resp, "t2i")
    end)
  end

  @doc """
  Generate an image from a text prompt guided by reference image(s) (image-to-image).
  Returns the absolute path of the generated PNG.

  `reference_images` is a list of absolute file paths. The first image is the
  primary canvas; additional images are extra references.
  """
  def generate_i2i(prompt, reference_images, opts \\ []) do
    model = Keyword.get(opts, :model, @default_i2i_model)
    size = Keyword.get(opts, :size, @default_size) |> normalize_size(model)

    primary_image = List.first(reference_images)

    unless primary_image && File.exists?(primary_image) do
      raise RuntimeError, "i2i requires at least one existing reference image path"
    end

    do_generate_with_retry(fn ->
      Logger.info("[Genclaw.ImageGen] i2i model=#{model} size=#{size} refs=#{length(reference_images)}")

      if model == "qwen-image-2.0" do
        # qwen-image-2.0 uses the generations endpoint with images as base64 data URLs
        images =
          Enum.map(reference_images, fn path ->
            data = File.read!(path)
            mime = infer_mime(path)
            "data:#{mime};base64,#{Base.encode64(data)}"
          end)

        body = %{
          prompt: prompt,
          model: model,
          size: size,
          response_format: "url",
          images: images
        }

        headers = auth_headers(json: true)
        resp = Req.post!(@t2i_url, json: body, headers: headers, receive_timeout: 120_000)
        handle_response(resp, "i2i")
      else
        # Other models use the multipart edits endpoint
        image_data = File.read!(primary_image)
        image_name = Path.basename(primary_image)
        mime = infer_mime(primary_image)

        form = [
          {:model, model},
          {:prompt, prompt},
          {:size, size},
          {:response_format, "url"},
          {:image, {:bytes, image_name, image_data, [{"Content-Type", mime}]}}
        ]

        headers = auth_headers(json: false)
        resp = Req.post!(@i2i_url, form_multipart: form, headers: headers, receive_timeout: 120_000)
        handle_response(resp, "i2i")
      end
    end)
  end

  @doc """
  Unified entry point: generate_image(prompt) for t2i, generate_image(prompt, refs) for i2i.
  """
  def generate_image(prompt, reference_images \\ nil)

  def generate_image(prompt, nil), do: generate_t2i(prompt)
  def generate_image(prompt, []), do: generate_t2i(prompt)
  def generate_image(prompt, refs) when is_list(refs), do: generate_i2i(prompt, refs)

  # ─── Internal helpers ─────────────────────────────────────────────────────

  # qwen-image-2.0 uses "*" as size separator (e.g. "1024*1024")
  defp normalize_size(size, "qwen-image-2.0"), do: String.replace(size, "x", "*")
  defp normalize_size(size, _model), do: size

  defp do_generate_with_retry(fun) do
    total = @max_retries + 1

    Enum.reduce_while(1..total, {:error, :no_attempt}, fn attempt, _prev ->
      try do
        path = fun.()
        Logger.info("[Genclaw.ImageGen] success on attempt #{attempt}/#{total}: #{path}")
        {:halt, {:ok, path}}
      rescue
        e ->
          Logger.warning("[Genclaw.ImageGen] attempt #{attempt}/#{total} failed: #{inspect(e)}")

          if attempt < total do
            delay = Enum.at(@retry_delays, attempt - 1, 5_000)
            Logger.info("[Genclaw.ImageGen] retrying in #{delay}ms...")
            Process.sleep(delay)
            {:cont, {:error, :retry}}
          else
            {:halt, {:error, e}}
          end
      end
    end)
    |> case do
      {:ok, path} -> path
      {:error, e} -> raise RuntimeError, "image_gen failed after #{total} attempts: #{inspect(e)}"
    end
  end

  defp handle_response(%Req.Response{status: 200, body: body}, tool) do
    case body do
      %{"data" => [first | _]} when is_map(first) ->
        save_image(first, tool)

      _ ->
        raise RuntimeError, "unexpected API response: #{inspect(body)}"
    end
  end

  defp handle_response(%Req.Response{status: status, body: body}, _tool) do
    raise RuntimeError, "API error (HTTP #{status}): #{inspect(body)}"
  end

  defp save_image(%{"url" => url}, tool) when is_binary(url) and url != "" do
    download_image(url, tool)
  end

  defp save_image(%{"b64_json" => b64}, tool) when is_binary(b64) do
    out_path = Session.gen_png_path(tool)
    File.write!(out_path, Base.decode64!(b64))
    record_artifact(out_path, tool)
    out_path
  end

  defp save_image(first, _tool) do
    raise RuntimeError, "API returned no image data: #{inspect(first)}"
  end

  defp download_image(url, tool) do
    out_path = Session.gen_png_path(tool)

    resp = Req.get!(url, receive_timeout: 120_000)

    case resp do
      %Req.Response{status: 200, body: body} when is_binary(body) ->
        File.write!(out_path, body)
        record_artifact(out_path, tool)
        out_path

      %Req.Response{status: status} ->
        raise RuntimeError, "failed to download image (HTTP #{status})"
    end
  end

  defp record_artifact(path, tool) do
    Session.record_artifact(path,
      tool_name: tool,
      role: "final",
      kind: "image",
      label: "Generated image"
    )
  end

  defp auth_headers(opts) do
    api_key = Application.get_env(:exhub, :giteeai_api_key, "")

    if api_key == "" do
      raise RuntimeError, "Gitee AI API key not configured. Set :giteeai_api_key in config."
    end

    base = [{"Authorization", "Bearer #{api_key}"}]

    if Keyword.get(opts, :json, true) do
      [{"Content-Type", "application/json"} | base]
    else
      base
    end
  end

  defp infer_mime(path) do
    ext = Path.extname(path) |> String.downcase()
    Map.get(%{".png" => "image/png", ".jpg" => "image/jpeg", ".jpeg" => "image/jpeg",
              ".webp" => "image/webp", ".gif" => "image/gif"}, ext, "image/png")
  end
end
