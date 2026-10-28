defmodule Exhub.Toonflow.Media do
  @moduledoc """
  Media generation for Toonflow (Phase 2: images).

  `generate_image/3` resolves a prompt for a shot (or a free prompt), calls the
  configured `Exhub.Toonflow.Media.Client`, saves the frame under the project's
  `assets/images/`, and records an `assets` row.

  `generate_candidates/3` renders a *candidate grid* (default 3×3) in one image
  so a director can pick a composition; `crop_filter/3` + `crop_argv/6` (pure)
  and `crop_image/5` slice a grid cell out of it with ffmpeg.

  The client is injectable — `Application.put_env(:exhub, :toonflow_media_client,
  Mod)` — so tests never hit the network. The default
  (`Exhub.Toonflow.Media.Default`) posts to the shared Gitee AI / moark image
  endpoints using `:giteeai_api_key`.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Assets, Config, DB, Schema, Store, Storyboard}

  @default_model "qwen-image-2.0"
  @default_size "1024x1024"

  # Pixel buckets per delivery aspect, longest side first. `gpt-image-2` accepts
  # exactly these three (`Media.Default`), so a 16:9 project asks for 1536x1024
  # instead of an unsupported 1792x1024; qwen-style models accept them too.
  @canvas_sizes [
    {"16:9", "1536x1024"},
    {"3:2", "1536x1024"},
    {"4:3", "1024x1024"},
    {"1:1", "1024x1024"},
    {"3:4", "1024x1024"},
    {"2:3", "1024x1536"},
    {"9:16", "1024x1536"}
  ]

  @doc "The keyframe pixel size for a delivery aspect ratio. Pure."
  @spec canvas_size(String.t() | nil) :: String.t()
  def canvas_size(aspect_ratio) do
    case List.keyfind(@canvas_sizes, aspect_ratio, 0) do
      {_aspect, size} -> size
      nil -> @default_size
    end
  end

  @doc """
  The default image size: `media.image_size` when configured, else the canvas
  bucket for `media.aspect_ratio`. Keyframes and reference images both use it, so
  a frame is composed for the ratio its clip is rendered at.
  """
  @spec default_size() :: String.t()
  def default_size, do: Config.image_size() || canvas_size(Config.aspect_ratio())

  @doc "The configured media client implementation module."
  @spec image_client() :: module()
  def image_client,
    do: Application.get_env(:exhub, :toonflow_media_client, Exhub.Toonflow.Media.Default)

  @doc """
  Generate a frame image.

  `opts`: `:shot_id` (build the prompt/references from a shot), `:prompt`
  (free prompt, used as-is), `:model`, `:size`, `:refs` (extra reference image
  paths for i2i), `:kind` (the recorded `assets.kind`, default `"image"`) and
  `:key` (the output filename stem, default the shot id), `:meta` (extra keys
  merged into the recorded asset's `meta`), and `:variation`
  (wrap the prompt with `variation_prompt/1` to change only the outfit).
  Keyframes, reference images and candidate grids all go through this function
  with a distinct `:kind`/`:key`. Returns `{:ok, asset}` or `{:error, reason}`.
  """
  @spec generate_image(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def generate_image(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    prompt = Toonflow.blank(Keyword.get(opts, :prompt))

    with {:ok, meta} <- Store.get_project(project, server),
         {:ok, shot, ctx} <- load_context(project, shot_id, server),
         {:ok, resolved_prompt} <- resolve_prompt(prompt, shot, ctx) do
      final_prompt = maybe_variation(resolved_prompt, opts)
      model = Keyword.get(opts, :model) || media_config()["image_model"] || @default_model
      size = Keyword.get(opts, :size) || default_size()
      kind = Keyword.get(opts, :kind) || "image"
      refs = resolve_refs(opts, shot, ctx)
      key = Keyword.get(opts, :key) || shot_id || Toonflow.new_id("img")
      extra_meta = Keyword.get(opts, :meta) || %{}
      out_path = image_path(meta["root_dir"], key)

      case image_client().generate_image(final_prompt,
             model: model,
             size: size,
             refs: refs,
             out_path: out_path
           ) do
        {:ok, result} -> record(project, shot_id, kind, final_prompt, result, extra_meta, server)
        {:error, reason} -> {:error, {:image_failed, reason}}
      end
    end
  end

  @doc "Local output path for a generated frame (pure)."
  @spec image_path(String.t(), String.t()) :: String.t()
  def image_path(project_dir, key),
    do: Path.join([project_dir, "assets", "images", Toonflow.file_key(key) <> ".png"])

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

  # --- candidate grids (Phase 8.5) ---

  @candidates_default 9

  @doc "The `assets.kind` used for candidate grids."
  @spec candidates_kind() :: String.t()
  def candidates_kind, do: "candidates"

  @doc """
  Render a *candidate grid* for a shot: one image holding several alternative
  compositions, so a director can pick a frame (`Keyframe.select_candidate/3`)
  instead of regenerating blindly.

  `opts`: `:shot_id` (build the prompt/references from a shot), `:prompt`,
  `:count` (cells, default `media.candidates` → 9), `:model`, `:size`, `:refs`,
  `:instructions`. The grid shape is derived from the count (`grid_shape/1`) and
  recorded in the asset's `meta.grid`. Returns `{:ok, asset}` or
  `{:error, reason}`.
  """
  @spec generate_candidates(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def generate_candidates(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    count = normalize_count(Keyword.get(opts, :count))
    {cols, rows} = grid_shape(count)

    with {:ok, shot, ctx} <- load_context(project, shot_id, server),
         {:ok, base} <- resolve_prompt(Toonflow.blank(Keyword.get(opts, :prompt)), shot, ctx) do
      prompt = candidates_prompt(base, cols, rows, Keyword.get(opts, :instructions))

      generate_image(
        project,
        [
          shot_id: shot_id,
          prompt: prompt,
          kind: candidates_kind(),
          key: "#{shot_id || Toonflow.new_id("img")}_candidates",
          meta: %{"grid" => %{"cols" => cols, "rows" => rows}, "cells" => count}
        ]
        |> put_opt(:model, Keyword.get(opts, :model))
        |> put_opt(:size, Keyword.get(opts, :size))
        |> put_opt(:refs, Keyword.get(opts, :refs)),
        server
      )
    end
  end

  @doc """
  The grid shape `{cols, rows}` for `count` cells — a perfect square where
  possible, else the smallest near-square that fits. Pure; `9` → `{3, 3}`.
  """
  @spec grid_shape(pos_integer()) :: {pos_integer(), pos_integer()}
  def grid_shape(count) when is_integer(count) and count > 0 do
    rows = count |> :math.sqrt() |> Float.ceil() |> trunc()
    cols = div(count + rows - 1, rows)
    {cols, rows}
  end

  @doc """
  Wrap a shot prompt with the "draw N candidate compositions as a grid"
  instruction. Pure; `instructions` (optional) is appended verbatim.
  """
  @spec candidates_prompt(String.t(), pos_integer(), pos_integer(), String.t() | nil) ::
          String.t()
  def candidates_prompt(base, cols, rows, instructions \\ nil) do
    head =
      "请在同一张图中绘制 #{cols}×#{rows} 个候选构图，按 #{cols} 列 #{rows} 行网格排列，" <>
        "每格一幅完整画面，格与格之间以细白线分隔，不添加任何文字或水印。" <>
        "候选画面主题：#{base}"

    case Toonflow.blank(instructions) do
      nil -> head
      instructions -> head <> "\n额外要求：" <> instructions
    end
  end

  @doc """
  The ffmpeg `-vf` crop expression for grid `cell` (0-based, row-major) in a
  `cols`×`rows` grid. Pure.
  """
  @spec crop_filter(non_neg_integer(), pos_integer(), pos_integer()) :: String.t()
  def crop_filter(cell, cols, rows) do
    col = rem(cell, cols)
    row = div(cell, cols)
    "crop=iw/#{cols}:ih/#{rows}:#{col}*iw/#{cols}:#{row}*ih/#{rows}"
  end

  @doc "ffmpeg argv to crop grid `cell` out of `src` into `out`. Pure."
  @spec crop_argv(
          String.t(),
          String.t(),
          non_neg_integer(),
          pos_integer(),
          pos_integer(),
          String.t()
        ) ::
          [String.t()]
  def crop_argv(ffmpeg, src, cell, cols, rows, out),
    do: [ffmpeg, "-y", "-i", src, "-vf", crop_filter(cell, cols, rows), "-frames:v", "1", out]

  @doc """
  Crop grid `cell` out of the image at `src` into `out` with ffmpeg. Returns
  `{:ok, out}` or `{:error, reason}`.
  """
  @spec crop_image(String.t(), non_neg_integer(), pos_integer(), pos_integer(), String.t()) ::
          {:ok, String.t()} | {:error, term()}
  def crop_image(src, cell, cols, rows, out) do
    ffmpeg = ffmpeg_path()

    cond do
      not (is_binary(src) and File.exists?(src)) ->
        {:error, :source_frame_missing}

      true ->
        with :ok <- check_tool(ffmpeg, :ffmpeg),
             :ok <- File.mkdir_p(Path.dirname(out)),
             :ok <- run_ffmpeg(crop_argv(ffmpeg, src, cell, cols, rows, out)) do
          {:ok, out}
        end
    end
  end

  defp normalize_count(count) when is_integer(count) and count > 0, do: count
  defp normalize_count(_count), do: candidates_default()

  defp candidates_default do
    case media_config()["candidates"] do
      count when is_integer(count) and count > 0 -> count
      _ -> @candidates_default
    end
  end

  # --- prompt / reference resolution ---

  defp load_context(_project, nil, _server), do: {:ok, nil, empty_context()}

  defp load_context(project, shot_id, server) do
    with {:ok, shot} <- Storyboard.get_shot(project, shot_id, server),
         {:ok, characters} <- Assets.list_characters(project, [], server),
         {:ok, scenes} <- Assets.list_scenes(project, [], server),
         {:ok, variations} <- Assets.variations_map(project, server) do
      {:ok, shot, %{"characters" => characters, "scenes" => scenes, "variations" => variations}}
    end
  end

  defp empty_context, do: %{"characters" => [], "scenes" => [], "variations" => %{}}

  # `variation: true` holds the character's face and changes only the outfit —
  # used when rendering a wardrobe variation (`Assets.generate_variation/3`).
  defp maybe_variation(prompt, opts) do
    if Keyword.get(opts, :variation), do: variation_prompt(prompt), else: prompt
  end

  defp resolve_prompt(prompt, _shot, _ctx) when is_binary(prompt), do: {:ok, prompt}
  defp resolve_prompt(_prompt, nil, _ctx), do: {:error, :missing_prompt}

  defp resolve_prompt(_prompt, shot, ctx) do
    case Storyboard.shot_prompt(
           shot,
           Map.get(ctx, "characters") || [],
           Map.get(ctx, "scenes") || []
         ) do
      "" -> {:error, :missing_prompt}
      prompt -> {:ok, prompt}
    end
  end

  defp resolve_refs(opts, shot, ctx), do: refs_for_shot(shot, ctx, opts)

  @variation_instruction "保持人物面部特征、五官、发型、发色、肤色与体型完全一致，仅更换服装与配饰。"

  @doc """
  Wrap `prompt` with the "keep the face, change the outfit" instruction used for
  wardrobe variations. Pure.
  """
  @spec variation_prompt(String.t()) :: String.t()
  def variation_prompt(prompt) when is_binary(prompt) do
    @variation_instruction <> "\n" <> prompt
  end

  @doc """
  Ordered, de-duplicated reference images for a shot.

  Order is **explicit `:refs` → scene reference → per-character reference**; a
  character's *selected variation* reference wins over its base reference (the
  shot's `meta.character_variations` maps a character name to a variation id).
  Paths that do not exist on disk are dropped. Pure, apart from `File.exists?/1`.
  """
  @spec refs_for_shot(map() | nil, map(), keyword()) :: [String.t()]
  def refs_for_shot(shot, ctx, opts \\ []) do
    explicit = Keyword.get(opts, :refs) || []
    scenes = Map.get(ctx, "scenes") || []
    characters = Map.get(ctx, "characters") || []
    variations = Map.get(ctx, "variations") || %{}

    (explicit ++ scene_refs(shot, scenes) ++ character_refs(shot, characters, variations))
    |> Enum.filter(&(is_binary(&1) and File.exists?(&1)))
    |> Enum.uniq()
  end

  defp scene_refs(shot, scenes) do
    case shot do
      %{"scene" => scene} when is_binary(scene) ->
        case find_named(scenes, scene) do
          %{"refs" => refs} when is_list(refs) -> refs
          _ -> []
        end

      _ ->
        []
    end
  end

  defp character_refs(shot, characters, variations) do
    shot
    |> Storyboard.characters_of()
    |> Enum.flat_map(&character_ref(&1, characters, variations, shot))
  end

  defp character_ref(name, characters, variations, shot) do
    case find_named(characters, name) do
      nil -> []
      character -> variation_ref(shot, name, variations) || character_refs_of(character)
    end
  end

  # A per-shot wardrobe selection (`meta.character_variations`) pins the look.
  defp variation_ref(%{"meta" => %{} = meta}, name, variations) do
    with %{} = selected <- meta["character_variations"],
         variation_id when is_binary(variation_id) <- selected[name],
         %{"refs" => refs} when is_list(refs) and refs != [] <- variations[variation_id] do
      refs
    else
      _ -> nil
    end
  end

  defp variation_ref(_shot, _name, _variations), do: nil

  defp character_refs_of(%{"refs" => refs}) when is_list(refs), do: refs
  defp character_refs_of(_character), do: []

  defp find_named(list, name), do: Enum.find(list, &(&1["name"] == name))

  # --- persistence ---

  defp record(project, shot_id, kind, prompt, result, extra_meta, server) do
    insert_asset(
      project,
      [
        shot_id: shot_id,
        kind: kind,
        path: result["path"],
        url: result["url"],
        prompt: prompt,
        meta:
          result
          |> Map.take(["model", "size"])
          |> Map.merge(extra_meta)
          |> Map.put("status", "completed")
      ],
      server
    )
  end

  defp media_config, do: Config.get("media", %{}) || %{}

  defp put_opt(opts, _key, nil), do: opts
  defp put_opt(opts, _key, ""), do: opts
  defp put_opt(opts, key, value), do: Keyword.put(opts, key, value)

  # --- ffmpeg (candidate cropping) ---

  defp ffmpeg_path do
    case Config.get("assembly", %{}) do
      %{"ffmpeg_path" => path} when is_binary(path) and path != "" -> path
      _ -> "ffmpeg"
    end
  end

  defp check_tool(exec, kind) do
    exists? =
      if Path.type(exec) == :absolute,
        do: File.exists?(exec),
        else: System.find_executable(exec) != nil

    if exists?, do: :ok, else: {:error, {:missing_tool, kind, exec}}
  end

  defp run_ffmpeg(argv) do
    case execute(argv) do
      {0, _output} -> :ok
      {code, output} -> {:error, {:ffmpeg_failed, code, String.slice(output, 0, 2000)}}
    end
  end

  defp execute(argv) do
    {out, err, code} =
      Exile.stream(argv, stderr: :consume, env: Exhub.MCP.Desktop.Helpers.clean_env())
      |> Enum.reduce({"", "", 0}, fn
        {:stdout, data}, {o, e, c} -> {o <> data, e, c}
        {:stderr, data}, {o, e, c} -> {o, e <> data, c}
        {:exit, {:status, c}}, {o, e, _} -> {o, e, c}
        {:exit, :epipe}, {o, e, _} -> {o, e, 0}
        _other, acc -> acc
      end)

    {code || 0, out <> err}
  rescue
    e -> {nil, Exception.message(e)}
  end
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

  *Every* reference is sent. The edits endpoint accepts repeated `image` parts,
  so a frame is conditioned on its scene and on each character sheet at once —
  `gpt-image-2` in particular keeps the referenced cast only when all of them
  arrive in one request. Sizes are normalized per model (`gpt-image-2` accepts a
  fixed set; see `gpt_size/1`).
  """

  @behaviour Exhub.Toonflow.Media.Client

  @api_base "https://api.moark.com/v1"
  @generations_url @api_base <> "/images/generations"
  @edits_url @api_base <> "/images/edits"

  # The only sizes the `gpt-image-2` edits endpoint accepts (verified live:
  # `1792x1024` is rejected with `400 参数无效 'size'`).
  @gpt_image_sizes ~w(1024x1024 1536x1024 1024x1536)

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
          images: refs |> usable_refs() |> Enum.map(&data_url/1)
        }

        with {:ok, resp} <- post_json(@generations_url, body, key), do: extract_image(resp)

      true ->
        form = edits_form(prompt, model, size, usable_refs(refs))

        with {:ok, resp} <- post_multipart(@edits_url, form, key), do: extract_image(resp)
    end
  end

  defp usable_refs(refs), do: Enum.filter(refs, &(is_binary(&1) and File.exists?(&1)))

  @doc """
  Build the `/v1/images/edits` multipart fields for a multi-image edit.

  *Every* reference becomes its own `image` part, and the endpoint conditions the
  frame on all of them — so a shot can be grounded in its scene *and* in every
  character sheet at once. Order matters: the first image is the base the model
  preserves (`Toonflow.Media.refs_for_shot/3` puts the scene first, then the
  characters). `gpt-image-2` only honours the reference cast when all of them
  arrive in the same request, which is why this takes a list rather than one
  primary image.

  A Req multipart part is `{name, value}` where `value` is iodata or a
  `{value, filename:, content_type:}` tuple — a bare `{:bytes, …}` tuple is not a
  valid part, and Req answers it with a `Protocol.UndefinedError` (it tries to
  enumerate it). `size` is always sent: `normalize_size/2` has already snapped it
  to a size the model accepts.
  """
  @spec edits_form(String.t(), String.t(), String.t(), [String.t()]) :: list()
  def edits_form(prompt, model, size, refs) when is_list(refs) do
    [
      {:model, model},
      {:prompt, prompt},
      {:response_format, "url"},
      {:size, size}
    ] ++ Enum.map(refs, &image_part/1)
  end

  defp image_part(path) do
    {:image, {File.read!(path), [filename: Path.basename(path), content_type: infer_mime(path)]}}
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

  # qwen-image-2.0 expects "*" as the size separator (e.g. "1024*1024");
  # gpt-image-2 accepts only the fixed set in @gpt_image_sizes.
  defp normalize_size(size, "qwen-image-2.0"), do: String.replace(size, "x", "*")
  defp normalize_size(size, "gpt-image-2"), do: gpt_size(size)
  defp normalize_size(size, _model), do: size

  @doc """
  Snap `size` to a size `gpt-image-2` accepts: `1536x1024` (landscape),
  `1024x1536` (portrait) or `1024x1024` (square). An already-valid size is
  returned verbatim; anything unparseable falls back to square. Pure.
  """
  @spec gpt_size(String.t()) :: String.t()
  def gpt_size(size) do
    size = to_string(size)

    if size in @gpt_image_sizes do
      size
    else
      case Regex.run(~r/^(\d+)x(\d+)$/, size) do
        [_, w, h] -> gpt_size(String.to_integer(w), String.to_integer(h))
        _ -> "1024x1024"
      end
    end
  end

  defp gpt_size(w, h) when w > h, do: "1536x1024"
  defp gpt_size(w, h) when h > w, do: "1024x1536"
  defp gpt_size(_w, _h), do: "1024x1024"

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
