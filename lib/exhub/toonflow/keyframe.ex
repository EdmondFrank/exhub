defmodule Exhub.Toonflow.Keyframe do
  @moduledoc """
  Start/end keyframe generation for Toonflow shots (Phase 7).

  A keyframe is an ordinary `assets` row whose `kind` is `"keyframe_start"` or
  `"keyframe_end"`, so it flows through `Exhub.Toonflow.Media`, the snapshot
  `{shot_id, kind}` map and the asset tooling unchanged. A *pending* keyframe is
  just a prompt in the shot's `meta` (`keyframe_start_prompt` /
  `keyframe_end_prompt`) with no asset row yet — the storyboard LLM writes those
  prompts, and `generate/3` renders them.

  Two frames per shot make `fl2va` (first-last-frame-to-video) possible:
  `Exhub.Toonflow.Video` prefers `keyframe_start` over a plain `image` for the
  first frame and uses `keyframe_end` as the interpolation target.

  `copy/3` duplicates an adjacent shot's frame so a shot can *start* where the
  previous one ended (`direction: "prev_end"`) or *end* where the next one
  starts (`direction: "next_start"`) — the continuity trick that keeps a
  cut from jumping.

  `select_candidate/3` is the other way in: crop a cell out of a candidate grid
  (`Media.generate_candidates/3`) and record it as the keyframe.

  Pure helpers (`types/0`, `normalize_type/1`, `normalize_direction/1`,
  `asset_kind/1`, `keyframe_prompt/2`, `adjacent/2`, `continuity_attrs/3`,
  `normalize_cell/1`) are unit-tested; `generate/3`, `copy/3`,
  `select_candidate/3`, `list_for_shot/3`, `set_prompt/5`, `next_shot/3` and
  `previous_shot/3` touch the project database.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{DB, Media, Schema, Store, Storyboard}

  @types ~w(start end)

  @directions %{
    "prev_end" => %{source: "end", target: "start"},
    "next_start" => %{source: "start", target: "end"}
  }

  @doc "The supported keyframe types."
  @spec types() :: [String.t()]
  def types, do: @types

  @doc "Normalize a keyframe type to `\"start\"` / `\"end\"`, or `nil`."
  @spec normalize_type(term()) :: String.t() | nil
  def normalize_type(type) when is_binary(type) do
    case type |> String.trim() |> String.downcase() do
      normalized when normalized in @types -> normalized
      _ -> nil
    end
  end

  def normalize_type(_type), do: nil

  @doc "Normalize a continuity direction to `\"prev_end\"` / `\"next_start\"`, or `nil`."
  @spec normalize_direction(term()) :: String.t() | nil
  def normalize_direction(direction) when is_binary(direction) do
    normalized = direction |> String.trim() |> String.downcase()
    if Map.has_key?(@directions, normalized), do: normalized, else: nil
  end

  def normalize_direction(_direction), do: nil

  @doc "The `assets.kind` recorded for a keyframe type, e.g. `\"keyframe_start\"`."
  @spec asset_kind(String.t()) :: String.t()
  def asset_kind(type), do: "keyframe_" <> type

  @doc """
  The keyframe prompt a shot carries for `type` (from `meta`), or `nil`. Pure.
  """
  @spec keyframe_prompt(map() | nil, String.t()) :: String.t() | nil
  def keyframe_prompt(%{"meta" => %{} = meta}, type),
    do: Toonflow.blank(meta["keyframe_#{type}_prompt"])

  def keyframe_prompt(_shot, _type), do: nil

  @doc """
  The `{previous, next}` shots around `shot_id` in an ordered shot list. Pure;
  shot ids are unique, so position in `shots` (ordered by `(script_id, idx)`)
  is what matters.
  """
  @spec adjacent([map()], String.t()) :: {map() | nil, map() | nil}
  def adjacent(shots, shot_id) do
    case Enum.find_index(shots, &(&1["id"] == shot_id)) do
      nil -> {nil, nil}
      0 -> {nil, Enum.at(shots, 1)}
      index -> {Enum.at(shots, index - 1), Enum.at(shots, index + 1)}
    end
  end

  @doc """
  Attributes for a copied keyframe asset row: the source frame's prompt under a
  new `kind`, with the source id recorded in `meta`. Pure.
  """
  @spec continuity_attrs(map(), String.t(), String.t()) :: map()
  def continuity_attrs(source, kind, path) do
    %{
      "kind" => kind,
      "path" => path,
      "prompt" => source["prompt"],
      "meta" => Map.merge(source["meta"] || %{}, %{"copied_from" => asset_id(source)})
    }
  end

  # `Media.latest_asset/4` decodes a row (`"id"`); `Media.insert_asset/3` returns
  # the freshly inserted asset (`"asset_id"`).
  defp asset_id(%{"id" => id}) when is_binary(id), do: id
  defp asset_id(%{"asset_id" => id}), do: id
  defp asset_id(_source), do: nil

  # --- candidates (Phase 8.5) ---

  @doc """
  Promote one cell of a shot's candidate grid (`Media.generate_candidates/3`)
  into a keyframe.

  `opts`: `:shot_id` (required), `:type` (`"start"` | `"end"`, required),
  `:cell` (0-based, row-major grid position; omit or `"whole"` to use the whole
  image — e.g. when a single candidate was generated), `:candidates_id` (a
  specific candidates asset; default: the shot's latest).

  A numeric cell is cropped out with ffmpeg; anything else copies the whole
  image. The result is recorded as a `keyframe_<type>` asset (so it flows into
  the `videos` stage like any other keyframe), with the source and grid recorded
  in `meta`. Returns `{:ok, asset}` or `{:error, reason}`.
  """
  @spec select_candidate(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def select_candidate(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    type = normalize_type(Keyword.get(opts, :type))
    cell = normalize_cell(Keyword.get(opts, :cell))

    with :ok <- validate_target(shot_id, type),
         {:ok, candidates} <- candidates_asset(project, shot_id, opts, server),
         {:ok, meta} <- Store.get_project(project, server),
         {:ok, path} <- produce_frame(meta["root_dir"], candidates, shot_id, type, cell) do
      Media.insert_asset(
        project,
        [
          shot_id: shot_id,
          kind: asset_kind(type),
          path: path,
          url: candidates["url"],
          prompt: candidates["prompt"],
          meta:
            candidates
            |> Map.get("meta", %{})
            |> Map.take(["grid"])
            |> Map.merge(%{
              "from_candidates" => asset_id(candidates),
              "cell" => cell,
              "status" => "completed"
            })
        ],
        server
      )
    end
  end

  @doc """
  Normalize a candidate cell: `nil`/`"whole"` → `nil` (whole image), integers
  and numeric strings → an integer. Pure.
  """
  @spec normalize_cell(term()) :: non_neg_integer() | nil
  def normalize_cell(nil), do: nil
  def normalize_cell(cell) when is_integer(cell) and cell >= 0, do: cell

  def normalize_cell(cell) when is_binary(cell) do
    case cell |> String.trim() |> String.downcase() do
      "whole" ->
        nil

      trimmed ->
        case Integer.parse(trimmed) do
          {n, ""} when n >= 0 -> n
          _ -> nil
        end
    end
  end

  def normalize_cell(_cell), do: nil

  defp candidates_asset(project, shot_id, opts, server) do
    case Toonflow.blank(Keyword.get(opts, :candidates_id)) do
      nil -> latest_candidates(project, shot_id, server)
      id -> fetch_asset(project, id, server)
    end
  end

  defp latest_candidates(project, shot_id, server) do
    case Media.latest_asset(project, shot_id, Media.candidates_kind(), server) do
      {:ok, %{} = asset} -> {:ok, asset}
      {:ok, nil} -> {:error, :no_candidates}
      {:error, reason} -> {:error, reason}
    end
  end

  defp fetch_asset(project, id, server) do
    sql = "SELECT #{Schema.asset_columns()} FROM assets WHERE id = ? LIMIT 1"

    Store.run_project(
      project,
      fn conn ->
        case DB.query_one(conn, sql, [id]) do
          {:ok, nil} -> {:error, :candidate_not_found}
          {:ok, row} -> {:ok, Schema.decode_asset(row)}
          {:error, reason} -> {:error, reason}
        end
      end,
      server
    )
  end

  defp produce_frame(root_dir, candidates, shot_id, type, nil) do
    duplicate(candidates["path"], root_dir, shot_id, type)
  end

  defp produce_frame(root_dir, candidates, shot_id, type, cell) do
    {cols, rows} = grid_of(candidates)
    out = Media.image_path(root_dir, frame_key(shot_id, type))

    if cell >= cols * rows do
      {:error, {:invalid_cell, cell, cols, rows}}
    else
      Media.crop_image(candidates["path"], cell, cols, rows, out)
    end
  end

  defp grid_of(candidates) do
    grid = (candidates["meta"] || %{})["grid"] || %{}
    {pos_int(grid["cols"], 3), pos_int(grid["rows"], 3)}
  end

  defp pos_int(value, _default) when is_integer(value) and value > 0, do: value
  defp pos_int(_value, default), do: default

  # --- generation ---

  @doc """
  Render one keyframe for a shot.

  `opts`: `:shot_id` (required), `:type` (`"start"` | `"end"`, required),
  `:prompt` (overrides the shot's keyframe prompt), `:refs`, `:model`, `:size`.
  The prompt resolves explicit → shot `meta.keyframe_<type>_prompt` → the shot's
  own prompt/description (via `Media.generate_image/3`, which also applies
  character consistency). Returns `{:ok, asset}` or `{:error, reason}`.
  """
  @spec generate(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def generate(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    type = normalize_type(Keyword.get(opts, :type))

    with :ok <- validate_target(shot_id, type),
         {:ok, shot} <- Storyboard.get_shot(project, shot_id, server) do
      prompt = Toonflow.blank(Keyword.get(opts, :prompt)) || keyframe_prompt(shot, type)

      media_opts =
        [shot_id: shot_id, kind: asset_kind(type), key: frame_key(shot_id, type)]
        |> put_opt(:prompt, prompt)
        |> put_opt(:refs, Keyword.get(opts, :refs))
        |> put_opt(:model, Keyword.get(opts, :model))
        |> put_opt(:size, Keyword.get(opts, :size))

      Media.generate_image(project, media_opts, server)
    end
  end

  # --- continuity ---

  @doc """
  Copy an adjacent shot's keyframe onto this shot.

  `opts`: `:shot_id` (required), `:direction` (`"prev_end"` | `"next_start"`,
  required). `"prev_end"` copies the previous shot's end frame into this shot's
  **start** frame; `"next_start"` copies the next shot's start frame into this
  shot's **end** frame. The image file is duplicated under this shot's key so
  regenerating the neighbour cannot mutate the copy. Returns `{:ok, asset}` or
  `{:error, reason}`.
  """
  @spec copy(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def copy(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    direction = normalize_direction(Keyword.get(opts, :direction))

    with :ok <- validate_copy(shot_id, direction),
         {:ok, shot} <- Storyboard.get_shot(project, shot_id, server),
         {:ok, sibling} <- sibling_shot(project, shot, direction, server),
         {:ok, source} <- source_frame(project, sibling, direction, server),
         {:ok, meta} <- Store.get_project(project, server),
         {:ok, path} <-
           duplicate(source["path"], meta["root_dir"], shot_id, target_type(direction)) do
      attrs = continuity_attrs(source, asset_kind(target_type(direction)), path)

      Media.insert_asset(
        project,
        [
          shot_id: shot_id,
          kind: attrs["kind"],
          path: attrs["path"],
          prompt: attrs["prompt"],
          meta: attrs["meta"]
        ],
        server
      )
    end
  end

  @doc "The shot's current start/end keyframe assets as `%{\"start\" =>, \"end\" =>}`."
  @spec list_for_shot(String.t(), String.t(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def list_for_shot(project, shot_id, server \\ Store) do
    with {:ok, start} <- Media.latest_asset(project, shot_id, asset_kind("start"), server),
         {:ok, finish} <- Media.latest_asset(project, shot_id, asset_kind("end"), server) do
      {:ok, %{"start" => start, "end" => finish}}
    end
  end

  @doc """
  Set (or clear) a shot's keyframe prompt in `shots.meta`.

  `prompt` of `nil`/`""` removes the key. Returns `{:ok, %{...}}` or
  `{:error, reason}`.
  """
  @spec set_prompt(String.t(), String.t(), String.t(), String.t() | nil, GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def set_prompt(project, shot_id, type, prompt, server \\ Store) do
    type = normalize_type(type)

    with :ok <- validate_target(shot_id, type),
         {:ok, shot} <- Storyboard.get_shot(project, shot_id, server) do
      key = "keyframe_#{type}_prompt"
      meta = (shot["meta"] || %{}) |> Map.put(key, Toonflow.blank(prompt))

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
        :ok -> {:ok, %{"shot_id" => shot_id, "type" => type, "prompt" => meta[key]}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  @doc "The shot after `shot_id` in the same script (or `nil`)."
  @spec next_shot(String.t(), String.t(), GenServer.server()) ::
          {:ok, map() | nil} | {:error, term()}
  def next_shot(project, shot_id, server \\ Store) do
    with {:ok, shots} <- script_shots(project, shot_id, server) do
      {_previous, next} = adjacent(shots, shot_id)
      {:ok, next}
    end
  end

  @doc "The shot before `shot_id` in the same script (or `nil`)."
  @spec previous_shot(String.t(), String.t(), GenServer.server()) ::
          {:ok, map() | nil} | {:error, term()}
  def previous_shot(project, shot_id, server \\ Store) do
    with {:ok, shots} <- script_shots(project, shot_id, server) do
      {previous, _next} = adjacent(shots, shot_id)
      {:ok, previous}
    end
  end

  # --- validation ---

  defp validate_target(nil, _type), do: {:error, :missing_shot_id}
  defp validate_target(_shot_id, nil), do: {:error, :invalid_keyframe_type}
  defp validate_target(_shot_id, _type), do: :ok

  defp validate_copy(nil, _direction), do: {:error, :missing_shot_id}
  defp validate_copy(_shot_id, nil), do: {:error, :invalid_direction}
  defp validate_copy(_shot_id, _direction), do: :ok

  # --- continuity helpers ---

  defp target_type(direction), do: @directions[direction].target
  defp source_type(direction), do: @directions[direction].source

  defp sibling_shot(project, shot, direction, server) do
    with {:ok, shots} <- script_shots(project, shot["id"], server) do
      {previous, next} = adjacent(shots, shot["id"])

      case if(direction == "prev_end", do: previous, else: next) do
        nil -> {:error, :no_adjacent_shot}
        sibling -> {:ok, sibling}
      end
    end
  end

  defp source_frame(project, sibling, direction, server) do
    case Media.latest_asset(project, sibling["id"], asset_kind(source_type(direction)), server) do
      {:ok, %{"path" => path} = asset} when is_binary(path) -> {:ok, asset}
      {:ok, _} -> {:error, :no_source_keyframe}
      {:error, reason} -> {:error, reason}
    end
  end

  defp duplicate(source_path, root_dir, shot_id, type) do
    target = Media.image_path(root_dir, frame_key(shot_id, type))

    cond do
      not is_binary(source_path) ->
        {:error, :no_source_keyframe}

      not File.exists?(source_path) ->
        {:error, :source_frame_missing}

      true ->
        with :ok <- File.mkdir_p(Path.dirname(target)),
             :ok <- File.cp(source_path, target) do
          {:ok, target}
        else
          {:error, reason} -> {:error, {:copy_failed, reason}}
        end
    end
  end

  defp script_shots(project, shot_id, server) do
    with {:ok, shot} <- Storyboard.get_shot(project, shot_id, server) do
      Storyboard.list_shots(project, compact(script_id: shot["script_id"]), server)
    end
  end

  # --- small helpers ---

  defp frame_key(shot_id, type), do: "#{shot_id}_#{type}"

  defp put_opt(opts, _key, nil), do: opts
  defp put_opt(opts, _key, ""), do: opts
  defp put_opt(opts, key, value), do: Keyword.put(opts, key, value)

  defp compact(kw), do: Enum.reject(kw, fn {_key, value} -> is_nil(value) end)
end
