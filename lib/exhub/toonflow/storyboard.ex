defmodule Exhub.Toonflow.Storyboard do
  @moduledoc """
  Storyboard (shot) planning for Toonflow.

  Turns a script into an ordered shot list via the DirectorAgent LLM call
  (`storyboard.system` / `storyboard.user`), grounded in the extracted character
  appearances: each shot carries a scene, description, 景别 (`size`),
  lighting, 运镜 (`motion`), the characters it features, and a text-to-image
  `prompt`. Shots are keyed by `script_id` and replaced on re-run (idempotent),
  and mirrored to `storyboards/<script_id>.json`.

  `split_shot/3` refines one shot after the fact: the director LLM
  (`storyboard.split.*`) turns it into finer sub-shots that replace it in place.

  `parse_storyboard/1`, `characters_of/1` and `shot_prompt/3` are pure and
  unit-tested.
  """

  alias Exhub.Toonflow

  alias Exhub.Toonflow.{
    Assets,
    DB,
    Json,
    Keyframe,
    LLM,
    Media,
    Memory,
    Prompts,
    Schema,
    Script,
    Store,
    Worldview
  }

  @max_subshots 8

  @doc """
  Generate a storyboard for a script.

  `opts`: `:script_id` (default: the latest script), `:instructions`,
  `:recall` (truthy: append relevant project memory to the instructions,
  best-effort), `:recall_top_k`.
  Returns `{:ok, summary}` with the shot count, or `{:error, reason}`
  (e.g. `:no_script`).
  """
  @spec generate_storyboard(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def generate_storyboard(project, opts \\ [], server \\ Store) do
    script_id = Toonflow.blank(Keyword.get(opts, :script_id))
    instructions = Worldview.append(project, Keyword.get(opts, :instructions) || "", server)

    with {:ok, script} <- fetch_script(project, script_id, server),
         {:ok, characters} <- Assets.list_characters(project, [], server),
         {:ok, recall} <- maybe_recall(project, opts, script["content"] || "", server),
         {:ok, system} <- Prompts.resolve("storyboard.system", %{}, project, server),
         {:ok, user} <-
           Prompts.resolve(
             "storyboard.user",
             %{
               "script" => script["content"] || "",
               "characters" => characters_digest(characters),
               "instructions" => append_instructions(instructions, recall)
             },
             project,
             server
           ),
         {:ok, raw} <- LLM.call_llm(system, user, []) do
      case parse_storyboard(raw) do
        {:ok, shots} -> persist(project, script["id"], shots, server)
        {:error, reason} -> {:error, {:parse_failed, reason}}
      end
    end
  end

  @doc "List shots, ordered by script then index. `opts`: `:script_id`, `:scene`, `:limit`."
  @spec list_shots(String.t(), keyword(), GenServer.server()) :: {:ok, [map()]} | {:error, term()}
  def list_shots(project, opts \\ [], server \\ Store) do
    script_id = Toonflow.blank(Keyword.get(opts, :script_id))
    scene = Toonflow.blank(Keyword.get(opts, :scene))
    limit = Keyword.get(opts, :limit)
    {where, params} = filters(script_id, scene)

    sql = "SELECT #{Schema.shot_columns()} FROM shots" <> where <> " ORDER BY script_id, idx"

    Store.run_project(
      project,
      fn conn ->
        case DB.query(conn, sql, params) do
          {:ok, rows} ->
            shots = rows |> Enum.map(&Schema.decode_shot/1) |> Toonflow.maybe_limit(limit)
            {:ok, shots}

          {:error, reason} ->
            {:error, reason}
        end
      end,
      server
    )
  end

  @doc "Fetch a single shot by id."
  @spec get_shot(String.t(), String.t(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def get_shot(project, shot_id, server \\ Store) do
    sql = "SELECT #{Schema.shot_columns()} FROM shots WHERE id = ? LIMIT 1"

    Store.run_project(
      project,
      fn conn ->
        case DB.query_one(conn, sql, [shot_id]) do
          {:ok, nil} -> {:error, :shot_not_found}
          {:ok, row} -> {:ok, Schema.decode_shot(row)}
          {:error, reason} -> {:error, reason}
        end
      end,
      server
    )
  end

  @doc """
  Parse an LLM storyboard response into normalized shot maps (with `idx`
  assigned by position). Tolerates fences/prose and a bare list. Pure.
  """
  @spec parse_storyboard(String.t()) :: {:ok, [map()]} | {:error, term()}
  def parse_storyboard(raw) when is_binary(raw) do
    with {:ok, decoded} <- Json.decode(raw),
         {:ok, list} <- Json.list(decoded, "shots") do
      {:ok,
       list |> Enum.with_index(1) |> Enum.map(fn {shot, idx} -> normalize_shot(shot, idx) end)}
    end
  end

  def parse_storyboard(_), do: {:error, :invalid_payload}

  @doc """
  The character names a shot features.

  Pure. `normalize_shot/2` keeps them at the top level, but `insert_shot/3`
  moves them into `meta`, so a shot loaded back from the database carries them
  in `meta.characters` — accept either.
  """
  @spec characters_of(map() | nil) :: [String.t()]
  def characters_of(%{"characters" => names}) when is_list(names), do: names
  def characters_of(%{"meta" => %{"characters" => names}}) when is_list(names), do: names
  def characters_of(_shot), do: []

  @doc """
  Build a text-to-image prompt for a `shot`, appending its 景别/机位/运镜/光线,
  the appearance of each referenced character and the description of its scene,
  for consistency. Pure.

  `opts`: `:lock_framing` swaps the shot's camera move for a fixed-camera clause
  — `fl2va` clips interpolate between two keyframes, so a prompt asking for a
  push-in fights them and surfaces as a zoom/size jump at the cut.
  """
  @spec shot_prompt(map(), [map()], [map()], keyword()) :: String.t()
  def shot_prompt(shot, characters \\ [], scenes \\ [], opts \\ []) do
    base =
      Toonflow.blank(Keyword.get(opts, :base)) || Json.text(shot["prompt"]) ||
        compose_prompt(shot)

    appearances =
      shot
      |> characters_of()
      |> Enum.flat_map(fn name ->
        case Enum.find(characters, &(&1["name"] == name)) do
          %{"appearance" => appearance} when is_binary(appearance) -> ["#{name}（#{appearance}）"]
          _ -> []
        end
      end)

    ([base] ++ framing_notes(base, shot, opts) ++ scene_notes(shot, scenes) ++ appearances)
    |> Enum.reject(&(is_nil(&1) or &1 == ""))
    |> Enum.join("；")
  end

  # 景别 is the one framing cue the storyboard always carries. Without it the
  # image model picks its own subject size per frame, so cuts jump between wide
  # and close compositions even though every clip is the same pixel size.
  # Ordered longest-first so 大远景 is not matched by 远景.
  @shot_size_lenses [
    {"大远景", "大远景：人物在画面中极小，环境与天空占主导"},
    {"远景", "远景：人物在画面中很小，环境与天空占主导，地平线清晰"},
    {"全景", "全景：人物全身入画，头顶与脚下都留出空间，不切头不切脚"},
    {"中景", "中景：人物膝盖以上入画，主体高度约占画面二分之一"},
    {"近景", "近景：人物胸部以上入画，主体高度约占画面三分之二"},
    {"特写", "特写：面部（或关键道具）充满画面，背景轻微虚化"}
  ]

  @locked_framing "固定机位一镜到底，保持与首帧相同的构图、景别与人物大小，禁止变焦、推拉、裁切与画面缩放"

  # A storyboard's 运镜 is a single character (推/摇/移/跟/拉). Passed through
  # verbatim it invites a whole-scene zoom, which lands as a subject-size jump —
  # the very thing the cut is judged on. Each clause bounds the move so it
  # cannot re-frame the shot.
  @camera_moves [
    {"推", "运镜：缓慢推近，幅度不超过画面的5%，人物大小基本不变"},
    {"拉", "运镜：缓慢拉远，幅度不超过画面的5%，人物大小基本不变"},
    {"摇", "运镜：水平方向缓摇约10度，人物始终留在画面内"},
    {"移", "运镜：横向平移约半个人物宽度，人物大小不变"},
    {"跟", "运镜：随人物轻微横向跟随，人物大小不变"},
    {"升", "运镜：缓慢升高，幅度不超过画面高度的5%"},
    {"降", "运镜：缓慢降低，幅度不超过画面高度的5%"}
  ]

  @fixed_moves ~w(固定 静 静止 不动)

  @doc ~S"""
  Whether the shot holds a fixed camera — an explicit 固定/静止 运镜, or none at
  all. Pure.

  Drives two decisions: a fixed shot is rendered with the locked-framing clause,
  and only a fixed shot keeps its end keyframe (`fl2va` pins both frames; a
  camera move and a pinned end frame fight each other).
  """
  @spec fixed_camera?(map() | nil) :: boolean()
  def fixed_camera?(shot) do
    case Json.text(shot && shot["motion"]) do
      nil -> true
      "" -> true
      motion -> Enum.any?(@fixed_moves, &String.contains?(motion, &1))
    end
  end

  defp framing_notes(base, shot, opts) do
    size = Json.text(shot["size"])

    [
      unless(size && String.contains?(base, size), do: shot_size_lens(size)),
      prefixed("机位", shot["camera"], base),
      if(Keyword.get(opts, :lock_framing),
        do: @locked_framing,
        else: camera_move_note(shot["motion"])
      ),
      prefixed("光线", shot["lighting"], base)
    ]
    |> Enum.reject(&(is_nil(&1) or &1 == "" or String.contains?(base, &1)))
  end

  defp camera_move_note(motion) do
    with text when is_binary(text) <- Json.text(motion) do
      if Enum.any?(@fixed_moves, &String.contains?(text, &1)) do
        @locked_framing
      else
        Enum.find_value(@camera_moves, "运镜：#{text}", fn {key, clause} ->
          if String.contains?(text, key), do: clause
        end)
      end
    end
  end

  defp shot_size_lens(size) when is_binary(size) do
    Enum.find_value(@shot_size_lenses, fn {key, clause} ->
      if String.contains?(size, key), do: clause
    end)
  end

  defp shot_size_lens(_size), do: nil

  # A note is only worth appending when the shot's own prompt does not already
  # carry it (a composed prompt embeds 景别/运镜/光线 verbatim).
  defp prefixed(label, value, base) do
    case Json.text(value) do
      nil -> nil
      "" -> nil
      text -> if String.contains?(base, text), do: nil, else: "#{label}：#{text}"
    end
  end

  # Append the scene's description (set by `toonflow_generate_reference`) so the
  # frame matches the established look of the location.
  defp scene_notes(shot, scenes) do
    with scene when is_binary(scene) <- shot["scene"],
         %{"description" => description} when is_binary(description) <-
           Enum.find(scenes, &(&1["name"] == scene)),
         true <- String.trim(description) != "" do
      ["场景：#{description}"]
    else
      _ -> []
    end
  end

  @doc """
  Choose which wardrobe variation of each character this shot uses.

  `selections` maps a **character name** to a variation **name** (or its id).
  The resolved variation id is stored in `shots.meta.character_variations`, so
  `Exhub.Toonflow.Media.refs_for_shot/3` conditions that shot's frames on the
  variation's reference image instead of the character's base look. Pass an
  empty map to clear all selections. Returns `{:ok, %{...}}` or
  `{:error, reason}`.
  """
  @spec set_shot_variations(String.t(), String.t(), map(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def set_shot_variations(project, shot_id, selections, server \\ Store) do
    with {:ok, shot} <- get_shot(project, shot_id, server),
         {:ok, resolved} <- resolve_selections(project, selections, server) do
      meta = Map.put(shot["meta"] || %{}, "character_variations", resolved)

      result =
        Store.run_project(
          project,
          fn conn ->
            DB.execute(conn, "UPDATE shots SET meta_json = ? WHERE id = ?", [
              Schema.encode_json(meta),
              shot_id
            ])
          end,
          server
        )

      case result do
        :ok -> {:ok, %{"shot_id" => shot_id, "character_variations" => resolved}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  @doc """
  Merge `patch` into a shot's `meta` and persist it.

  Backfills fields the storyboard LLM may not have supplied — notably
  `meta.speaker` (the character who speaks the shot's line), which the `voices`
  stage resolves to a per-character voice via `media.voice_map`. A `nil` value
  deletes the key. Returns `{:ok, %{"shot_id" =>, "meta" =>}}` or
  `{:error, reason}`.
  """
  @spec put_shot_meta(String.t(), String.t(), map(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def put_shot_meta(project, shot_id, patch, server \\ Store) when is_map(patch) do
    with {:ok, shot} <- get_shot(project, shot_id, server) do
      meta = apply_meta_patch(shot["meta"] || %{}, patch)

      case Store.run_project(
             project,
             fn conn ->
               DB.execute(conn, "UPDATE shots SET meta_json = ? WHERE id = ?", [
                 Schema.encode_json(meta),
                 shot_id
               ])
             end,
             server
           ) do
        :ok -> {:ok, %{"shot_id" => shot_id, "meta" => meta}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp apply_meta_patch(meta, patch) do
    Enum.reduce(patch, meta, fn
      {key, nil}, acc -> Map.delete(acc, to_string(key))
      {key, value}, acc -> Map.put(acc, to_string(key), value)
    end)
  end

  # --- editing a shot (canvas inspector) ---

  # The canvas names a field `description`; the column is `shot_desc`. Nothing
  # else is addressable — a stray key in a request can never touch `id`, `idx`,
  # `script_id` or another shot.
  @column_fields %{
    "scene" => "scene",
    "description" => "shot_desc",
    "size" => "size",
    "camera" => "camera",
    "lighting" => "lighting",
    "motion" => "motion",
    "prompt" => "prompt"
  }

  @meta_fields ~w(characters dialogue speaker keyframe_start_prompt keyframe_end_prompt)

  # Which assets an edit invalidates. The canvas shows them as stale so an edit
  # asks for a re-render instead of shipping a clip generated from the old
  # values: a changed line invalidates the voice and the clip that was paced to
  # it, a changed prompt the clip, a changed frame the keyframes and the clip
  # interpolated between them.
  @field_media %{
    "scene" => ~w(image keyframe_start keyframe_end video),
    "description" => ~w(image keyframe_start keyframe_end video),
    "size" => ~w(image keyframe_start keyframe_end video),
    "camera" => ~w(image keyframe_start keyframe_end video),
    "lighting" => ~w(image keyframe_start keyframe_end video),
    "motion" => ~w(keyframe_start keyframe_end video),
    "prompt" => ~w(image video),
    "characters" => ~w(image keyframe_start keyframe_end video),
    "dialogue" => ~w(audio video),
    "speaker" => ~w(audio),
    "keyframe_start_prompt" => ~w(keyframe_start video),
    "keyframe_end_prompt" => ~w(keyframe_end video)
  }

  @doc "The shot fields the canvas may edit, sorted. Pure."
  @spec editable_fields() :: [String.t()]
  def editable_fields, do: Enum.sort(Map.keys(@column_fields) ++ @meta_fields)

  @doc "The 景别 values the canvas offers as a select. Pure."
  @spec size_options() :: [String.t()]
  def size_options, do: Enum.map(@shot_size_lenses, &elem(&1, 0))

  @doc "The 运镜 values the canvas offers as a select (固定 first). Pure."
  @spec motion_options() :: [String.t()]
  def motion_options, do: ["固定" | Enum.map(@camera_moves, &elem(&1, 0))]

  # A shot's 机位 is free text — the storyboard prompt does not ask for it — so
  # the canvas offers these as autocomplete hints and accepts anything typed.
  # Each is appended verbatim as `机位：<text>`, so keep them noun phrases.
  @camera_options [
    "平视",
    "俯拍",
    "仰拍",
    "过肩",
    "正面",
    "侧面",
    "背面",
    "主观视角",
    "鸟瞰",
    "低角度贴地"
  ]

  @doc """
  The 机位 hints the canvas offers as autocomplete (free text stays editable).
  Pure.
  """
  @spec camera_options() :: [String.t()]
  def camera_options, do: @camera_options

  @doc """
  Split a characters field into names: a list, or `、`/`,`/`;` separated text.

  The canvas round-trips the cast as one text input, so this is what both the
  request and the stored `meta.characters` go through. Pure.
  """
  @spec split_characters(term()) :: [String.t()]
  def split_characters(list) when is_list(list) do
    list
    |> Enum.map(&(&1 |> to_string() |> String.trim()))
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  def split_characters(text) when is_binary(text) do
    text
    |> String.split([",", "，", "、", ";", "；"])
    |> split_characters()
  end

  def split_characters(_value), do: []

  @doc """
  Update a shot's editable properties from the canvas inspector.

  `patch` uses the canvas field names (`description`, not `shot_desc`); unknown
  keys are ignored, `nil`/`""` clears a field. Every edit stamps
  `meta.edited` — the asset kinds it invalidated, with a timestamp — which
  `Snapshot` turns into the canvas' stale flag. Clearing
  `keyframe_end_prompt` also drops the end frames it described, since `fl2va`
  would otherwise keep interpolating towards them.

  Returns `{:ok, shot}` with the stored shot, or `{:error, :shot_not_found}`.
  """
  @spec update_shot(String.t(), String.t(), map(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def update_shot(project, shot_id, patch, server \\ Store) when is_map(patch) do
    with {:ok, shot} <- get_shot(project, shot_id, server) do
      {columns, meta_patch, edited} = split_edit(patch)
      meta = (shot["meta"] || %{}) |> apply_meta_patch(meta_patch) |> stamp_edited(edited)

      cond do
        columns == [] and meta_patch == %{} ->
          {:ok, shot}

        true ->
          params = Enum.map(columns, &elem(&1, 1)) ++ [Schema.encode_json(meta), shot_id]

          case Store.run_project(
                 project,
                 fn conn -> DB.execute(conn, update_sql(columns), params) end,
                 server
               ) do
            :ok ->
              if cleared_end_keyframe?(meta_patch) do
                _ = Media.delete_assets(project, shot_id, "keyframe_end", server)
              end

              get_shot(project, shot_id, server)

            {:error, reason} ->
              {:error, reason}
          end
      end
    end
  end

  defp update_sql(columns) do
    sets = Enum.map(columns, fn {column, _value} -> "#{column} = ?" end)
    "UPDATE shots SET " <> Enum.join(sets ++ ["meta_json = ?"], ", ") <> " WHERE id = ?"
  end

  defp split_edit(patch) do
    {columns, meta, edited} =
      Enum.reduce(patch, {[], %{}, []}, fn {key, value}, {columns, meta, edited} ->
        key = to_string(key)

        cond do
          column = @column_fields[key] ->
            {[{column, Toonflow.blank(value)} | columns], meta, [key | edited]}

          key in @meta_fields ->
            {columns, Map.put(meta, key, meta_value(key, value)), [key | edited]}

          true ->
            {columns, meta, edited}
        end
      end)

    {Enum.sort(columns), meta, affected_media(edited)}
  end

  defp meta_value("characters", value), do: split_characters(value)
  defp meta_value(_key, value), do: Toonflow.blank(value)

  defp affected_media(keys) do
    keys
    |> Enum.flat_map(&Map.get(@field_media, &1, []))
    |> Enum.uniq()
    |> Enum.sort()
  end

  defp stamp_edited(meta, []), do: meta

  defp stamp_edited(meta, kinds) do
    now = Toonflow.now_iso()
    edited = Enum.reduce(kinds, meta["edited"] || %{}, &Map.put(&2, &1, now))

    meta
    |> Map.put("edited", edited)
    |> Map.put("edited_at", now)
  end

  # `meta_patch` is normalized (`Toonflow.blank/1`), so a cleared end-frame
  # prompt is an explicit `nil` there.
  defp cleared_end_keyframe?(meta_patch) do
    Map.has_key?(meta_patch, "keyframe_end_prompt") and
      is_nil(meta_patch["keyframe_end_prompt"])
  end

  # --- shot splitting (Phase 8.5) ---

  @doc """
  Split one shot into finer sub-shots with the director LLM
  (`storyboard.split.system` / `storyboard.split.user`).

  BigBanana's "shot splitting" move: a shot is often carrying two beats, and
  splitting it after the fact is cheaper than regenerating the whole board. The
  sub-shots **replace** the original **in place** — same order position, the
  script's indices resequenced — and inherit the original's scene, cast and the
  outer keyframe prompts (the first sub-shot keeps the original's start frame
  prompt, the last keeps its end frame prompt), so start/end continuity survives
  the split. The replaced shot's generated assets are removed.

  `opts`: `:shot_id` (required), `:instructions`, `:recall`, `:recall_top_k`.
  Returns `{:ok, summary}` or `{:error, reason}`.
  """
  @spec split_shot(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def split_shot(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    instructions = Worldview.append(project, Keyword.get(opts, :instructions) || "", server)

    with :ok <- validate_shot(shot_id),
         {:ok, shot} <- get_shot(project, shot_id, server),
         {:ok, characters} <- Assets.list_characters(project, [], server),
         {:ok, recall} <- maybe_recall(project, opts, shot["shot_desc"] || "", server),
         {:ok, system} <- Prompts.resolve("storyboard.split.system", %{}, project, server),
         {:ok, user} <-
           Prompts.resolve(
             "storyboard.split.user",
             %{
               "shot" => shot_digest(shot),
               "characters" => characters_digest(characters),
               "instructions" => append_instructions(instructions, recall)
             },
             project,
             server
           ),
         {:ok, raw} <- LLM.call_llm(system, user, []) do
      case parse_storyboard(raw) do
        {:ok, []} -> {:error, :no_subshots}
        {:ok, subshots} -> replace_shot(project, shot, subshots, server)
        {:error, reason} -> {:error, {:parse_failed, reason}}
      end
    end
  end

  defp validate_shot(nil), do: {:error, :missing_shot_id}
  defp validate_shot(_shot_id), do: :ok

  # The shot, rendered for the split prompt.
  defp shot_digest(shot) do
    [
      {"场景", shot["scene"]},
      {"画面", shot["shot_desc"]},
      {"景别", shot["size"]},
      {"光线", shot["lighting"]},
      {"运镜", shot["motion"]},
      {"角色", Enum.join(characters_of(shot), "、")},
      {"提示词", shot["prompt"]},
      {"首帧", Keyframe.keyframe_prompt(shot, "start")},
      {"尾帧", Keyframe.keyframe_prompt(shot, "end")}
    ]
    |> Enum.flat_map(fn
      {_label, value} when value in [nil, ""] -> []
      {label, value} -> ["#{label}：#{value}"]
    end)
    |> Enum.join("\n")
  end

  # Replace `shot` with `subshots` in place. The shots after it are shifted by
  # `length(subshots) - 1` first, so the sub-shots slot into the original's
  # index without renumbering the whole board.
  defp replace_shot(project, shot, subshots, server) do
    subshots = subshots |> Enum.take(@max_subshots) |> inherit_from(shot)
    script_id = shot["script_id"]
    base_idx = shot["idx"] || 0
    shift = length(subshots) - 1

    result =
      Store.run_project(
        project,
        fn conn ->
          DB.transaction(conn, fn ->
            with :ok <- shift_indices(conn, script_id, base_idx, shift),
                 :ok <- DB.execute(conn, "DELETE FROM assets WHERE shot_id = ?", [shot["id"]]),
                 :ok <- DB.execute(conn, "DELETE FROM shots WHERE id = ?", [shot["id"]]),
                 {:ok, ids} <- insert_subshots(conn, script_id, subshots, base_idx) do
              {:ok, ids}
            end
          end)
        end,
        server
      )

    case result do
      {:ok, ids} ->
        {:ok,
         %{
           "shot_id" => shot["id"],
           "script_id" => script_id,
           "sub_shots" => length(subshots),
           "created" => ids,
           "shots" => Enum.map(subshots, &shot_summary/1)
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp shift_indices(_conn, _script_id, _base_idx, 0), do: :ok

  defp shift_indices(conn, script_id, base_idx, shift) do
    DB.execute(
      conn,
      "UPDATE shots SET idx = idx + ? WHERE script_id = ? AND idx > ?",
      [shift, script_id, base_idx]
    )
  end

  defp insert_subshots(conn, script_id, subshots, base_idx) do
    subshots
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {shot, offset}, {:ok, acc} ->
      case insert_shot(conn, script_id, Map.put(shot, "idx", base_idx + offset)) do
        {:ok, id} -> {:cont, {:ok, acc ++ [id]}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  # A sub-shot keeps the original's scene/cast, and the outer keyframe prompts
  # anchor the first and last sub-shot so the split does not break continuity.
  defp inherit_from(subshots, shot) do
    last = length(subshots) - 1

    subshots
    |> Enum.with_index()
    |> Enum.map(fn {sub, index} ->
      sub
      |> Map.put("scene", sub["scene"] || shot["scene"])
      |> Map.put("characters", inherit_cast(sub, shot))
      |> Map.put(
        "meta",
        sub
        |> Map.get("meta", %{})
        |> inherit_prompt(
          index == 0,
          "keyframe_start_prompt",
          Keyframe.keyframe_prompt(shot, "start")
        )
        |> inherit_prompt(
          index == last,
          "keyframe_end_prompt",
          Keyframe.keyframe_prompt(shot, "end")
        )
      )
    end)
  end

  defp inherit_cast(%{"characters" => [_ | _]} = sub, _shot), do: sub["characters"]
  defp inherit_cast(_sub, shot), do: characters_of(shot)

  defp inherit_prompt(meta, true, _key, value) when not is_binary(value), do: meta

  defp inherit_prompt(meta, true, key, value) do
    # `Toonflow.blank/1` returns nil for an absent prompt, so compare explicitly.
    if Toonflow.blank(meta[key]) == nil, do: Map.put(meta, key, value), else: meta
  end

  defp inherit_prompt(meta, _cond, _key, _value), do: meta

  # --- persistence ---

  # --- wardrobe selections ---

  defp resolve_selections(_project, selections, _server) when selections in [nil, %{}],
    do: {:ok, %{}}

  defp resolve_selections(project, selections, server) when is_map(selections) do
    selections
    |> Enum.reduce_while({:ok, %{}}, fn {character, variation}, {:ok, acc} ->
      case resolve_selection(project, to_string(character), to_string(variation), server) do
        {:ok, id} -> {:cont, {:ok, Map.put(acc, to_string(character), id)}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp resolve_selections(_project, _selections, _server), do: {:error, :invalid_selections}

  defp resolve_selection(project, character, variation, server) do
    with {:ok, variations} <- Assets.list_variations(project, [character: character], server) do
      case Enum.find(variations, &(&1["name"] == variation or &1["id"] == variation)) do
        nil -> {:error, {:variation_not_found, character, variation}}
        variation -> {:ok, variation["id"]}
      end
    end
  end

  defp fetch_script(project, script_id, server) do
    opts = if script_id, do: [script_id: script_id], else: []

    case Script.get_script(project, opts, server) do
      {:ok, script} -> {:ok, script}
      {:error, :not_found} -> {:error, :no_script}
      {:error, reason} -> {:error, reason}
    end
  end

  defp persist(project, script_id, shots, server) do
    with {:ok, meta} <- Store.get_project(project, server) do
      name = meta["name"]
      dir = meta["root_dir"]

      result =
        Store.run_project(
          name,
          fn conn ->
            DB.transaction(conn, fn ->
              with :ok <- DB.execute(conn, "DELETE FROM shots WHERE script_id = ?", [script_id]) do
                shots
                |> Enum.reduce_while({:ok, 0}, fn shot, {:ok, count} ->
                  case insert_shot(conn, script_id, shot) do
                    {:ok, _id} -> {:cont, {:ok, count + 1}}
                    {:error, reason} -> {:halt, {:error, reason}}
                  end
                end)
              end
            end)
          end,
          server
        )

      case result do
        {:ok, count} ->
          write_mirror(dir, script_id, shots)

          {:ok,
           %{
             "script_id" => script_id,
             "shot_count" => count,
             "shots" => Enum.map(shots, &shot_summary/1)
           }}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  defp insert_shot(conn, script_id, shot) do
    meta = Map.put(shot["meta"] || %{}, "characters", shot["characters"] || [])
    id = Toonflow.new_id("sht")

    case DB.execute(
           conn,
           """
           INSERT INTO shots (id, script_id, idx, scene, shot_desc, size, camera, lighting, motion, prompt, meta_json)
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
           """,
           [
             id,
             script_id,
             shot["idx"],
             shot["scene"],
             shot["shot_desc"],
             shot["size"],
             shot["camera"],
             shot["lighting"],
             shot["motion"],
             shot["prompt"],
             Schema.encode_json(meta)
           ]
         ) do
      :ok -> {:ok, id}
      {:error, reason} -> {:error, reason}
    end
  end

  defp write_mirror(dir, script_id, shots) do
    File.mkdir_p(Path.join(dir, "storyboards"))

    payload = %{"script_id" => script_id, "shots" => shots, "created_at" => Toonflow.now_iso()}

    File.write(
      Path.join([dir, "storyboards", script_id <> ".json"]),
      Jason.encode!(payload, pretty: true)
    )
  end

  defp shot_summary(shot) do
    Map.take(shot, ["idx", "scene", "shot_desc", "size", "lighting", "motion"])
  end

  # --- memory recall ---

  defp maybe_recall(project, opts, query, server) do
    if Keyword.get(opts, :recall) do
      Memory.recall(
        project,
        [query: Toonflow.preview(query, 200), top_k: Keyword.get(opts, :recall_top_k)],
        server
      )
    else
      {:ok, ""}
    end
  end

  defp append_instructions(instructions, ""), do: instructions
  defp append_instructions(instructions, recall) when instructions in [nil, ""], do: recall
  defp append_instructions(instructions, recall), do: instructions <> "\n\n" <> recall

  # --- parsing helpers (pure) ---

  defp compose_prompt(shot) do
    [shot["scene"], shot["shot_desc"], shot["size"], shot["motion"], shot["lighting"]]
    |> Enum.reject(&(is_nil(&1) or &1 == ""))
    |> Enum.join("，")
  end

  defp normalize_shot(%{} = shot, idx) do
    %{
      "idx" => idx,
      "scene" => Json.text(shot["scene"] || shot["场景"]),
      "shot_desc" =>
        Json.text(
          shot["shot_desc"] || shot["description"] || shot["desc"] || shot["action"] || shot["画面"]
        ),
      "size" => Json.text(shot["size"] || shot["shot_size"] || shot["景别"]),
      "camera" => Json.text(shot["camera"] || shot["机位"]),
      "lighting" => Json.text(shot["lighting"] || shot["光线"]),
      "motion" => Json.text(shot["motion"] || shot["camera_move"] || shot["运镜"]),
      "prompt" => Json.text(shot["prompt"] || shot["image_prompt"]),
      "characters" => character_names(shot["characters"] || shot["角色"]),
      "meta" =>
        Map.drop(shot, [
          "scene",
          "场景",
          "shot_desc",
          "description",
          "desc",
          "action",
          "画面",
          "size",
          "shot_size",
          "景别",
          "camera",
          "机位",
          "lighting",
          "光线",
          "motion",
          "camera_move",
          "运镜",
          "prompt",
          "image_prompt",
          "characters",
          "角色",
          "keyframe_start_prompt",
          "start_frame_prompt",
          "首帧提示词",
          "keyframe_end_prompt",
          "end_frame_prompt",
          "尾帧提示词",
          "speaker",
          "说话人"
        ])
        |> put_keyframe_prompts(shot)
        |> put_speaker(shot)
    }
  end

  defp normalize_shot(other, idx) do
    %{
      "idx" => idx,
      "scene" => nil,
      "shot_desc" => Json.text(other),
      "size" => nil,
      "camera" => nil,
      "lighting" => nil,
      "motion" => nil,
      "prompt" => nil,
      "characters" => [],
      "meta" => %{"raw" => other}
    }
  end

  # The storyboard LLM may describe a first/last frame per shot (used by the
  # `keyframes` stage and `Exhub.Toonflow.Keyframe`); store them under stable
  # keys in `meta`, dropping the aliases it used.
  defp put_keyframe_prompts(meta, shot) do
    keyframes =
      %{}
      |> put_prompt(
        "keyframe_start_prompt",
        shot["keyframe_start_prompt"] || shot["start_frame_prompt"] || shot["首帧提示词"]
      )
      |> put_prompt(
        "keyframe_end_prompt",
        shot["keyframe_end_prompt"] || shot["end_frame_prompt"] || shot["尾帧提示词"]
      )

    Map.merge(meta, keyframes)
  end

  # The storyboard LLM may name the character who speaks a shot's line; store it
  # under a stable `meta.speaker` key, which the `voices` stage uses to pick a
  # per-character voice from `media.voice_map`.
  defp put_speaker(meta, shot) do
    case Json.text(shot["speaker"] || shot["说话人"]) do
      nil -> meta
      speaker -> Map.put(meta, "speaker", speaker)
    end
  end

  defp put_prompt(map, _key, nil), do: map

  defp put_prompt(map, key, value) do
    case Json.text(value) do
      nil -> map
      text -> Map.put(map, key, text)
    end
  end

  defp character_names(list) when is_list(list) do
    list |> Enum.map(&Json.text/1) |> Enum.reject(&is_nil/1)
  end

  defp character_names(name) when is_binary(name) do
    if String.trim(name) == "", do: [], else: [name]
  end

  defp character_names(_), do: []

  defp characters_digest([]), do: "（未提取角色）"

  defp characters_digest(characters) do
    characters
    |> Enum.map_join("\n", fn character ->
      appearance = character["appearance"] || character["role"] || ""
      "- #{character["name"]}：#{appearance}"
    end)
  end

  defp filters(nil, nil), do: {"", []}
  defp filters(script_id, nil), do: {" WHERE script_id = ?", [script_id]}
  defp filters(nil, scene), do: {" WHERE scene = ?", [scene]}
  defp filters(script_id, scene), do: {" WHERE script_id = ? AND scene = ?", [script_id, scene]}
end
