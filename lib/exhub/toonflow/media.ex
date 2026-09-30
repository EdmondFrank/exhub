defmodule Exhub.Toonflow.Media do
  @moduledoc """
  Media generation for Toonflow (Phase 2: images).

  `generate_image/3` resolves a prompt for a shot (or a free prompt), calls the
  configured `Exhub.Toonflow.Media.Client`, saves the frame under the project's
  `assets/images/`, and records an `assets` row.

  The client is injectable — `Application.put_env(:exhub, :toonflow_media_client,
  Mod)` — so tests never hit the network. The default
  (`Exhub.Toonflow.Media.Default`) posts to the shared Gitee AI / moark image
  endpoints using `:giteeai_api_key`.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Assets, Config, DB, Schema, Store, Storyboard}

  @default_model "qwen-image-2.0"
  @default_size "1024x1024"

  @doc "The configured media client implementation module."
  @spec image_client() :: module()
  def image_client,
    do: Application.get_env(:exhub, :toonflow_media_client, Exhub.Toonflow.Media.Default)

  @doc """
  Generate a frame image.

  `opts`: `:shot_id` (build the prompt/references from a shot), `:prompt`
  (free prompt, used as-is), `:model`, `:size`, `:refs` (extra reference image
  paths for i2i). Returns `{:ok, asset}` or `{:error, reason}`.
  """
  @spec generate_image(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def generate_image(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    prompt = Toonflow.blank(Keyword.get(opts, :prompt))

    with {:ok, meta} <- Store.get_project(project, server),
         {:ok, shot, characters} <- load_shot(project, shot_id, server),
         {:ok, final_prompt} <- resolve_prompt(prompt, shot, characters) do
      model = Keyword.get(opts, :model) || media_config()["image_model"] || @default_model
      size = Keyword.get(opts, :size) || @default_size
      refs = resolve_refs(opts, shot, characters)
      key = shot_id || Toonflow.new_id("img")
      out_path = image_path(meta["root_dir"], key)

      case image_client().generate_image(final_prompt,
             model: model,
             size: size,
             refs: refs,
             out_path: out_path
           ) do
        {:ok, result} -> record(project, shot_id, final_prompt, result, server)
        {:error, reason} -> {:error, {:image_failed, reason}}
      end
    end
  end

  @doc "Local output path for a generated frame (pure)."
  @spec image_path(String.t(), String.t()) :: String.t()
  def image_path(project_dir, key),
    do: Path.join([project_dir, "assets", "images", sanitize(key) <> ".png"])

  @doc """
  Insert an `assets` row and return it as a map.

  `attrs` keys: `:shot_id`, `:character_id`, `:kind` (required), `:path`,
  `:url`, `:prompt`, `:meta` (a map). Shared by the image, video and voice
  generators.
  """
  @spec insert_asset(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def insert_asset(project, attrs, server \\ Store) do
    id = Toonflow.new_id("ast")
    now = Toonflow.now_iso()
    meta = attrs[:meta] || %{}

    insert =
      Store.run_project(
        project,
        fn conn ->
          DB.execute(
            conn,
            """
            INSERT INTO assets (id, shot_id, character_id, kind, path, url, prompt, meta_json, created_at)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            [
              id,
              attrs[:shot_id],
              attrs[:character_id],
              attrs[:kind],
              attrs[:path],
              attrs[:url],
              attrs[:prompt],
              Schema.encode_json(meta),
              now
            ]
          )
        end,
        server
      )

    case insert do
      :ok ->
        {:ok,
         %{
           "asset_id" => id,
           "shot_id" => attrs[:shot_id],
           "kind" => attrs[:kind],
           "path" => attrs[:path],
           "url" => attrs[:url],
           "prompt" => attrs[:prompt],
           "meta" => meta,
           "created_at" => now
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc "The most recent asset of `kind` linked to a shot (or `nil`)."
  @spec latest_asset(String.t(), String.t() | nil, String.t(), GenServer.server()) ::
          {:ok, map() | nil} | {:error, term()}
  def latest_asset(project, shot_id, kind, server \\ Store) do
    sql =
      "SELECT #{Schema.asset_columns()} FROM assets WHERE shot_id = ? AND kind = ? " <>
        "ORDER BY created_at DESC LIMIT 1"

    Store.run_project(
      project,
      fn conn ->
        case DB.query_one(conn, sql, [shot_id, kind]) do
          {:ok, nil} -> {:ok, nil}
          {:ok, row} -> {:ok, Schema.decode_asset(row)}
          {:error, reason} -> {:error, reason}
        end
      end,
      server
    )
  end

  @doc "List assets, oldest first. `opts`: `:shot_id`, `:kind`, `:limit`."
  @spec list_assets(String.t(), keyword(), GenServer.server()) ::
          {:ok, [map()]} | {:error, term()}
  def list_assets(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    kind = Toonflow.blank(Keyword.get(opts, :kind))
    limit = Keyword.get(opts, :limit)
    {where, params} = asset_filters(shot_id, kind)
    sql = "SELECT #{Schema.asset_columns()} FROM assets" <> where <> " ORDER BY created_at"

    Store.run_project(
      project,
      fn conn ->
        case DB.query(conn, sql, params) do
          {:ok, rows} ->
            {:ok, rows |> Enum.map(&Schema.decode_asset/1) |> Toonflow.maybe_limit(limit)}

          {:error, reason} ->
            {:error, reason}
        end
      end,
      server
    )
  end

  defp asset_filters(nil, nil), do: {"", []}
  defp asset_filters(shot_id, nil), do: {" WHERE shot_id = ?", [shot_id]}
  defp asset_filters(nil, kind), do: {" WHERE kind = ?", [kind]}
  defp asset_filters(shot_id, kind), do: {" WHERE shot_id = ? AND kind = ?", [shot_id, kind]}

  # --- prompt / reference resolution ---

  defp load_shot(_project, nil, _server), do: {:ok, nil, []}

  defp load_shot(project, shot_id, server) do
    with {:ok, shot} <- Storyboard.get_shot(project, shot_id, server),
         {:ok, characters} <- Assets.list_characters(project, [], server) do
      {:ok, shot, characters}
    end
  end

  defp resolve_prompt(prompt, _shot, _characters) when is_binary(prompt), do: {:ok, prompt}
  defp resolve_prompt(_prompt, nil, _characters), do: {:error, :missing_prompt}

  defp resolve_prompt(_prompt, shot, characters) do
    case Storyboard.shot_prompt(shot, characters) do
      "" -> {:error, :missing_prompt}
      prompt -> {:ok, prompt}
    end
  end

  defp resolve_refs(opts, shot, characters) do
    explicit = Keyword.get(opts, :refs) || []

    from_characters =
      ((shot && shot["characters"]) || [])
      |> Enum.flat_map(fn name ->
        case Enum.find(characters, &(&1["name"] == name)) do
          %{"refs" => refs} when is_list(refs) -> refs
          _ -> []
        end
      end)

    (explicit ++ from_characters)
    |> Enum.filter(&(is_binary(&1) and File.exists?(&1)))
    |> Enum.uniq()
  end

  # --- persistence ---

  defp record(project, shot_id, prompt, result, server) do
    insert_asset(
      project,
      [
        shot_id: shot_id,
        kind: "image",
        path: result["path"],
        url: result["url"],
        prompt: prompt,
        meta: Map.take(result, ["model", "size"])
      ],
      server
    )
  end

  defp media_config, do: Config.get("media", %{}) || %{}

  defp sanitize(key), do: String.replace(to_string(key), ~r/[^A-Za-z0-9._-]/, "_")
end

defmodule Exhub.Toonflow.Media.Client do
  @moduledoc "Behaviour for Toonflow media generation backends."

  @callback generate_image(prompt :: String.t(), opts :: keyword()) ::
              {:ok, map()} | {:error, term()}
end

defmodule Exhub.Toonflow.Media.Default do
  @moduledoc """
  Default Toonflow media client — Gitee AI / moark image generation.

  Text-to-image posts to `/v1/images/generations`; when reference images are
  supplied it conditions the frame (i2i): `qwen-image-2.0` passes base64 data
  URLs to the generations endpoint, other models use the multipart
  `/v1/images/edits` endpoint. The result is written to `:out_path`.
  """

  @behaviour Exhub.Toonflow.Media.Client

  @api_base "https://api.moark.com/v1"
  @generations_url @api_base <> "/images/generations"
  @edits_url @api_base <> "/images/edits"

  @impl true
  def generate_image(prompt, opts) do
    model = Keyword.get(opts, :model) || "qwen-image-2.0"
    size = Keyword.get(opts, :size, "1024x1024") |> normalize_size(model)
    refs = Keyword.get(opts, :refs, [])
    out_path = Keyword.fetch!(opts, :out_path)

    with {:ok, key} <- api_key(),
         {:ok, image} <- request(prompt, model, size, refs, key),
         :ok <- save(image, out_path) do
      {:ok,
       %{
         "path" => out_path,
         "url" => url_of(image),
         "model" => model,
         "size" => size,
         "prompt" => prompt
       }}
    end
  end

  # --- request ---

  defp request(prompt, model, size, [], key) do
    body = %{prompt: prompt, model: model, size: size, response_format: "url"}
    with {:ok, resp} <- post_json(@generations_url, body, key), do: extract_image(resp)
  end

  defp request(prompt, model, size, [primary | _] = refs, key) do
    cond do
      not File.exists?(primary) ->
        {:error, {:missing_ref, primary}}

      model == "qwen-image-2.0" ->
        body = %{
          prompt: prompt,
          model: model,
          size: size,
          response_format: "url",
          images: Enum.map(refs, &data_url/1)
        }

        with {:ok, resp} <- post_json(@generations_url, body, key), do: extract_image(resp)

      true ->
        form = [
          {:model, model},
          {:prompt, prompt},
          {:size, size},
          {:response_format, "url"},
          {:image,
           {:bytes, Path.basename(primary), File.read!(primary),
            [{"Content-Type", infer_mime(primary)}]}}
        ]

        with {:ok, resp} <- post_multipart(@edits_url, form, key), do: extract_image(resp)
    end
  end

  defp post_json(url, body, key) do
    opts =
      [json: body, headers: json_headers(key), receive_timeout: 180_000] ++
        Exhub.TLSCompat.req_opts()

    case Req.post(url, opts) do
      {:ok, %Req.Response{status: 200, body: body}} -> {:ok, body}
      {:ok, %Req.Response{status: status, body: body}} -> {:error, {:http, status, body}}
      {:error, reason} -> {:error, reason}
    end
  end

  defp post_multipart(url, form, key) do
    opts =
      [form_multipart: form, headers: auth_headers(key), receive_timeout: 180_000] ++
        Exhub.TLSCompat.req_opts()

    case Req.post(url, opts) do
      {:ok, %Req.Response{status: 200, body: body}} -> {:ok, body}
      {:ok, %Req.Response{status: status, body: body}} -> {:error, {:http, status, body}}
      {:error, reason} -> {:error, reason}
    end
  end

  defp extract_image(%{"data" => [%{"url" => url} | _]}) when is_binary(url) and url != "",
    do: {:ok, {:url, url}}

  defp extract_image(%{"data" => [%{"b64_json" => b64} | _]}) when is_binary(b64),
    do: {:ok, {:b64, b64}}

  defp extract_image(body), do: {:error, {:unexpected_response, body}}

  # --- saving ---

  defp save({:b64, b64}, out_path) do
    with {:ok, data} <- Base.decode64(b64) do
      File.mkdir_p!(Path.dirname(out_path))
      File.write(out_path, data)
    end
  end

  defp save({:url, url}, out_path) do
    File.mkdir_p!(Path.dirname(out_path))

    case Req.get(url, [receive_timeout: 180_000] ++ Exhub.TLSCompat.req_opts()) do
      {:ok, %Req.Response{status: 200, body: body}} when is_binary(body) ->
        File.write(out_path, body)

      {:ok, %Req.Response{status: status}} ->
        {:error, {:download, status}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp url_of({:url, url}), do: url
  defp url_of({:b64, _b64}), do: nil

  # --- helpers ---

  defp api_key do
    case Application.get_env(:exhub, :giteeai_api_key, "") do
      "" -> {:error, :missing_api_key}
      key -> {:ok, key}
    end
  end

  defp auth_headers(key), do: [{"Authorization", "Bearer #{key}"}]
  defp json_headers(key), do: [{"Content-Type", "application/json"} | auth_headers(key)]

  # qwen-image-2.0 expects "*" as the size separator (e.g. "1024*1024").
  defp normalize_size(size, "qwen-image-2.0"), do: String.replace(size, "x", "*")
  defp normalize_size(size, _model), do: size

  defp data_url(path) do
    "data:#{infer_mime(path)};base64,#{Base.encode64(File.read!(path))}"
  end

  defp infer_mime(path) do
    ext = path |> Path.extname() |> String.downcase()

    Map.get(
      %{
        ".png" => "image/png",
        ".jpg" => "image/jpeg",
        ".jpeg" => "image/jpeg",
        ".webp" => "image/webp",
        ".gif" => "image/gif"
      },
      ext,
      "image/png"
    )
  end
end
