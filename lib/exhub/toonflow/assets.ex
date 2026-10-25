defmodule Exhub.Toonflow.Assets do
  @moduledoc """
  Character / scene / prop extraction for Toonflow.

  Reads a script (the latest by default, or a specific `script_id`), asks the
  LLM (`assets.system` / `assets.user`) for a cast list with stable appearances,
  and upserts the characters, scenes and props into the project `characters` /
  `scenes` / `props` tables by name — re-running is idempotent. The full
  extraction is mirrored to `characters/appearance.json`, so the appearance
  database is inspectable alongside the rest of the workspace.

  `generate_reference/3` renders a *reference image* for a character, scene or
  prop and records it on that row's `refs`. Those references feed
  `Exhub.Toonflow.Media.refs_for_shot/3`, so shots are conditioned on the
  character look and the scene — not on text alone.

  `generate_variation/3` adds a *wardrobe variation* of a character: a new
  outfit rendered from the character's base reference with the face held fixed
  (see `Exhub.Toonflow.Media.variation_prompt/1`). A shot selects variations via
  `Exhub.Toonflow.Storyboard.set_shot_variations/4`.

  `parse_assets/1` is pure and unit-tested.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{DB, Json, LLM, Media, Prompts, Schema, Script, Store, Worldview}

  # Reference-image look-and-feel per subject kind. The description follows.
  @reference_prefixes %{
    "character" => "角色定妆设定图：正面、全身、中性表情、纯色背景，人物五官与发型需在后续分镜中保持一致。",
    "scene" => "场景概念设定图：无人物，体现空间结构、光线与氛围，需在后续分镜中保持一致。",
    "prop" => "道具设定图：单品居中、纯色背景，需在后续分镜中保持一致。"
  }

  @default_variation_prompt "全新造型：更换服装与配饰，整体风格与角色气质一致。"

  @doc """
  Extract characters, scenes and props from a script.

  `opts`: `:script_id` (default: the latest script), `:instructions`.
  Returns `{:ok, summary}` with counts and the character names, or
  `{:error, reason}` (e.g. `:no_script`).
  """
  @spec extract_assets(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def extract_assets(project, opts \\ [], server \\ Store) do
    script_id = Toonflow.blank(Keyword.get(opts, :script_id))
    instructions = Worldview.append(project, Keyword.get(opts, :instructions) || "", server)

    with {:ok, script} <- fetch_script(project, script_id, server),
         {:ok, system} <- Prompts.resolve("assets.system", %{}, project, server),
         {:ok, user} <-
           Prompts.resolve(
             "assets.user",
             %{
               "script" => script["content"] || "",
               "instructions" => instructions
             },
             project,
             server
           ),
         {:ok, raw} <- LLM.call_llm(system, user, []) do
      case parse_assets(raw) do
        {:ok, assets} -> persist(project, script["id"], assets, server)
        {:error, reason} -> {:error, {:parse_failed, reason}}
      end
    end
  end

  @doc "List characters. `opts`: `:name`, `:limit`."
  @spec list_characters(String.t(), keyword(), GenServer.server()) ::
          {:ok, [map()]} | {:error, term()}
  def list_characters(project, opts \\ [], server \\ Store) do
    name = Toonflow.blank(Keyword.get(opts, :name))
    limit = Keyword.get(opts, :limit)
    {where, params} = if name, do: {" WHERE name = ?", [name]}, else: {"", []}

    sql = "SELECT #{Schema.character_columns()} FROM characters" <> where <> " ORDER BY name"

    Store.run_project(
      project,
      fn conn ->
        case DB.query(conn, sql, params) do
          {:ok, rows} ->
            characters =
              rows |> Enum.map(&Schema.decode_character/1) |> Toonflow.maybe_limit(limit)

            {:ok, characters}

          {:error, reason} ->
            {:error, reason}
        end
      end,
      server
    )
  end

  @doc "List scenes. `opts`: `:name`, `:limit`."
  @spec list_scenes(String.t(), keyword(), GenServer.server()) ::
          {:ok, [map()]} | {:error, term()}
  def list_scenes(project, opts \\ [], server \\ Store),
    do: list_named("scenes", &Schema.decode_scene/1, project, opts, server)

  @doc "List props. `opts`: `:name`, `:limit`."
  @spec list_props(String.t(), keyword(), GenServer.server()) :: {:ok, [map()]} | {:error, term()}
  def list_props(project, opts \\ [], server \\ Store),
    do: list_named("props", &Schema.decode_prop/1, project, opts, server)

  @doc "Fetch one scene by name."
  @spec get_scene(String.t(), String.t(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def get_scene(project, name, server \\ Store),
    do: fetch_one("scenes", &Schema.decode_scene/1, project, name, server)

  @doc "Fetch one prop by name."
  @spec get_prop(String.t(), String.t(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def get_prop(project, name, server \\ Store),
    do: fetch_one("props", &Schema.decode_prop/1, project, name, server)

  @doc """
  List a character's wardrobe variations. `opts`: `:character` (name),
  `:limit`.
  """
  @spec list_variations(String.t(), keyword(), GenServer.server()) ::
          {:ok, [map()]} | {:error, term()}
  def list_variations(project, opts \\ [], server \\ Store) do
    character = Toonflow.blank(Keyword.get(opts, :character))
    limit = Keyword.get(opts, :limit)

    with {:ok, character_id} <- character_filter(project, character, server) do
      {where, params} =
        if character_id, do: {" WHERE character_id = ?", [character_id]}, else: {"", []}

      sql =
        "SELECT #{Schema.variation_columns()} FROM character_variations" <>
          where <> " ORDER BY character_id, name"

      Store.run_project(
        project,
        fn conn ->
          case DB.query(conn, sql, params) do
            {:ok, rows} ->
              variations =
                rows |> Enum.map(&Schema.decode_variation/1) |> Toonflow.maybe_limit(limit)

              {:ok, variations}

            {:error, reason} ->
              {:error, reason}
          end
        end,
        server
      )
    end
  end

  @doc """
  Every variation in the project as `%{variation_id => variation}` — the lookup
  `Exhub.Toonflow.Media.refs_for_shot/3` uses to honour a shot's per-character
  wardrobe selection.
  """
  @spec variations_map(String.t(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def variations_map(project, server \\ Store) do
    with {:ok, variations} <- list_variations(project, [], server) do
      {:ok, Map.new(variations, &{&1["id"], &1})}
    end
  end

  @doc """
  Create (or update) a wardrobe variation of a character and render its
  reference image.

  `opts`: `:character` (name, required), `:name` (variation name, required),
  `:visual_prompt` (the outfit; defaults to a generic new look), `:model`,
  `:size`. The image is conditioned on the character's base reference(s) with a
  "keep the face, change the outfit" instruction (`i2i`), and stored on the
  variation's `refs`. A shot can then select it via
  `Exhub.Toonflow.Storyboard.set_shot_variations/4`.
  """
  @spec generate_variation(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def generate_variation(project, opts \\ [], server \\ Store) do
    character = Toonflow.blank(Keyword.get(opts, :character))
    name = Toonflow.blank(Keyword.get(opts, :name))
    instruction = Toonflow.blank(Keyword.get(opts, :visual_prompt))

    with :ok <- validate_variation(character, name),
         {:ok, subject} <- fetch_character(project, character, server) do
      media_opts =
        [
          prompt: Media.variation_prompt(instruction || @default_variation_prompt),
          kind: "variation_ref",
          key: "ref_variation_#{subject["name"]}_#{name}",
          refs: subject["refs"] || [],
          variation: true
        ]
        |> put_opt(:model, Keyword.get(opts, :model))
        |> put_opt(:size, Keyword.get(opts, :size))

      case Media.generate_image(project, media_opts, server) do
        {:ok, asset} -> upsert_variation(project, subject, name, instruction, asset, server)
        {:error, reason} -> {:error, reason}
      end
    end
  end

  @doc """
  Render a reference image for a character, scene or prop and record it on that
  row's `refs`.

  `opts`: `:kind` (`"character"` | `"scene"` | `"prop"`, required), `:name`
  (required), `:prompt` (full override; by default a kind-specific setup prefix
  plus the subject's appearance/description is used), `:model`, `:size`.
  Returns `{:ok, %{"kind", "name", "path", "refs"}}` or `{:error, reason}`.
  """
  @spec generate_reference(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def generate_reference(project, opts \\ [], server \\ Store) do
    kind = normalize_kind(Keyword.get(opts, :kind))
    name = Toonflow.blank(Keyword.get(opts, :name))

    with :ok <- validate_reference(kind, name),
         {:ok, subject} <- fetch_subject(project, kind, name, server),
         {:ok, prompt} <- reference_prompt(kind, subject, Keyword.get(opts, :prompt)) do
      media_opts =
        [prompt: prompt, kind: "#{kind}_ref", key: "ref_#{kind}_#{name}"]
        |> put_opt(:model, Keyword.get(opts, :model))
        |> put_opt(:size, Keyword.get(opts, :size))

      case Media.generate_image(project, media_opts, server) do
        {:ok, asset} -> attach_reference(project, kind, subject, asset, server)
        {:error, reason} -> {:error, reason}
      end
    end
  end

  @doc """
  Parse an LLM cast-list response into
  `%{"characters" => [...], "scenes" => [...], "props" => [...]}`.

  Tolerates Markdown fences, surrounding prose, and a bare top-level list
  (treated as the character list). Pure.
  """
  @spec parse_assets(String.t()) :: {:ok, map()} | {:error, term()}
  def parse_assets(raw) when is_binary(raw) do
    with {:ok, decoded} <- Json.decode(raw) do
      {:ok, normalize_assets(decoded)}
    end
  end

  def parse_assets(_), do: {:error, :invalid_payload}

  # --- persistence ---

  defp fetch_script(project, script_id, server) do
    opts = if script_id, do: [script_id: script_id], else: []

    case Script.get_script(project, opts, server) do
      {:ok, script} -> {:ok, script}
      {:error, :not_found} -> {:error, :no_script}
      {:error, reason} -> {:error, reason}
    end
  end

  defp persist(project, script_id, assets, server) do
    with {:ok, meta} <- Store.get_project(project, server) do
      name = meta["name"]
      dir = meta["root_dir"]

      result =
        Store.run_project(
          name,
          fn conn ->
            DB.transaction(conn, fn ->
              with {:ok, characters} <-
                     upsert_all(conn, assets["characters"], &upsert_character/2),
                   {:ok, scenes} <-
                     upsert_all(conn, assets["scenes"], fn conn, item ->
                       upsert_named(conn, "scenes", "scn", item)
                     end),
                   {:ok, props} <-
                     upsert_all(conn, assets["props"], fn conn, item ->
                       upsert_named(conn, "props", "prp", item)
                     end) do
                {:ok, %{"characters" => characters, "scenes" => scenes, "props" => props}}
              end
            end)
          end,
          server
        )

      case result do
        {:ok, counts} ->
          write_mirror(dir, script_id, assets)

          {:ok,
           %{
             "script_id" => script_id,
             "characters" => counts["characters"],
             "scenes" => counts["scenes"],
             "props" => counts["props"],
             "character_names" => Enum.map(assets["characters"], & &1["name"])
           }}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  defp upsert_all(conn, items, fun) do
    Enum.reduce_while(items, {:ok, 0}, fn item, {:ok, count} ->
      case fun.(conn, item) do
        :ok -> {:cont, {:ok, count + 1}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp upsert_character(conn, character) do
    case DB.query_one(conn, "SELECT id FROM characters WHERE name = ? LIMIT 1", [
           character["name"]
         ]) do
      {:ok, [id]} ->
        DB.execute(
          conn,
          "UPDATE characters SET appearance = ?, meta_json = ? WHERE id = ?",
          [character["appearance"], Schema.encode_json(character["meta"]), id]
        )

      {:ok, nil} ->
        DB.execute(
          conn,
          "INSERT INTO characters (id, name, appearance, refs_json, meta_json) VALUES (?, ?, ?, ?, ?)",
          [
            Toonflow.new_id("chr"),
            character["name"],
            character["appearance"],
            Schema.encode_json(character["refs"]),
            Schema.encode_json(character["meta"])
          ]
        )

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp upsert_named(conn, table, prefix, item) do
    with {:ok, found} <- find_id(conn, table, item["name"]) do
      case found do
        nil ->
          DB.execute(
            conn,
            "INSERT INTO #{table} (id, name, description, refs_json, meta_json) VALUES (?, ?, ?, ?, ?)",
            [
              Toonflow.new_id(prefix),
              item["name"],
              item["description"],
              Schema.encode_json([]),
              Schema.encode_json(item["meta"])
            ]
          )

        id ->
          DB.execute(conn, "UPDATE #{table} SET description = ?, meta_json = ? WHERE id = ?", [
            item["description"],
            Schema.encode_json(item["meta"]),
            id
          ])
      end
    end
  end

  # --- named lookups (characters / scenes / props share the name+refs shape) ---

  defp find_id(conn, table, name) do
    case DB.query_one(conn, "SELECT id FROM #{table} WHERE name = ? LIMIT 1", [name]) do
      {:ok, [id]} -> {:ok, id}
      {:ok, nil} -> {:ok, nil}
      {:error, reason} -> {:error, reason}
    end
  end

  defp list_named(table, decode, project, opts, server) do
    name = Toonflow.blank(Keyword.get(opts, :name))
    limit = Keyword.get(opts, :limit)
    {where, params} = if name, do: {" WHERE name = ?", [name]}, else: {"", []}

    sql =
      "SELECT id, name, description, refs_json, meta_json FROM #{table}" <>
        where <> " ORDER BY name"

    Store.run_project(
      project,
      fn conn ->
        case DB.query(conn, sql, params) do
          {:ok, rows} -> {:ok, rows |> Enum.map(decode) |> Toonflow.maybe_limit(limit)}
          {:error, reason} -> {:error, reason}
        end
      end,
      server
    )
  end

  defp fetch_one(table, decode, project, name, server) do
    with {:ok, rows} <- list_named(table, decode, project, [name: name, limit: 1], server) do
      case rows do
        [row | _] -> {:ok, row}
        [] -> {:error, :subject_not_found}
      end
    end
  end

  # --- reference images ---

  # --- wardrobe variations ---

  defp fetch_character(project, name, server) do
    with {:ok, rows} <- list_characters(project, [name: name, limit: 1], server) do
      case rows do
        [row | _] -> {:ok, row}
        [] -> {:error, :character_not_found}
      end
    end
  end

  defp character_filter(_project, nil, _server), do: {:ok, nil}

  defp character_filter(project, name, server) do
    with {:ok, character} <- fetch_character(project, name, server), do: {:ok, character["id"]}
  end

  defp validate_variation(nil, _name), do: {:error, :missing_character}
  defp validate_variation(_character, nil), do: {:error, :missing_name}
  defp validate_variation(_character, _name), do: :ok

  defp upsert_variation(project, subject, name, instruction, asset, server) do
    refs = [asset["path"]]
    meta = Schema.encode_json(%{"status" => "completed"})

    result =
      Store.run_project(
        project,
        fn conn ->
          with {:ok, existing} <- find_variation(conn, subject["id"], name) do
            case existing do
              nil ->
                id = Toonflow.new_id("var")

                with :ok <-
                       DB.execute(
                         conn,
                         """
                         INSERT INTO character_variations
                           (id, character_id, name, visual_prompt, refs_json, meta_json)
                         VALUES (?, ?, ?, ?, ?, ?)
                         """,
                         [
                           id,
                           subject["id"],
                           name,
                           instruction,
                           Schema.encode_json(refs),
                           meta
                         ]
                       ),
                     do: {:ok, id}

              id ->
                with :ok <-
                       DB.execute(
                         conn,
                         """
                         UPDATE character_variations
                         SET visual_prompt = ?, refs_json = ?, meta_json = ?
                         WHERE id = ?
                         """,
                         [instruction, Schema.encode_json(refs), meta, id]
                       ),
                     do: {:ok, id}
            end
          end
        end,
        server
      )

    case result do
      {:ok, id} ->
        {:ok,
         %{
           "variation_id" => id,
           "character" => subject["name"],
           "name" => name,
           "path" => asset["path"],
           "asset_id" => asset["asset_id"],
           "refs" => refs,
           "visual_prompt" => instruction
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp find_variation(conn, character_id, name) do
    sql =
      "SELECT id FROM character_variations WHERE character_id = ? AND name = ? LIMIT 1"

    case DB.query_one(conn, sql, [character_id, name]) do
      {:ok, [id]} -> {:ok, id}
      {:ok, nil} -> {:ok, nil}
      {:error, reason} -> {:error, reason}
    end
  end

  defp normalize_kind(kind) when is_binary(kind) do
    normalized = kind |> String.trim() |> String.downcase()
    if Map.has_key?(@reference_prefixes, normalized), do: normalized, else: nil
  end

  defp normalize_kind(_kind), do: nil

  defp validate_reference(nil, _name), do: {:error, :invalid_reference_kind}
  defp validate_reference(_kind, nil), do: {:error, :missing_name}
  defp validate_reference(_kind, _name), do: :ok

  defp fetch_subject(project, "character", name, server) do
    with {:ok, rows} <- list_characters(project, [name: name, limit: 1], server) do
      case rows do
        [row | _] -> {:ok, row}
        [] -> {:error, :subject_not_found}
      end
    end
  end

  defp fetch_subject(project, "scene", name, server), do: get_scene(project, name, server)
  defp fetch_subject(project, "prop", name, server), do: get_prop(project, name, server)

  defp reference_prompt(_kind, _subject, explicit) when is_binary(explicit) do
    case Toonflow.blank(explicit) do
      nil -> {:error, :missing_description}
      text -> {:ok, text}
    end
  end

  defp reference_prompt(kind, subject, _explicit) do
    case subject["appearance"] || subject["description"] do
      description when is_binary(description) ->
        {:ok, @reference_prefixes[kind] <> "\n" <> description}

      _ ->
        {:error, :missing_description}
    end
  end

  defp attach_reference(project, kind, subject, asset, server) do
    refs = Enum.uniq([asset["path"] | subject["refs"] || []])

    result =
      Store.run_project(
        project,
        fn conn ->
          DB.execute(conn, "UPDATE #{table_for(kind)} SET refs_json = ? WHERE id = ?", [
            Schema.encode_json(refs),
            subject["id"]
          ])
        end,
        server
      )

    case result do
      :ok ->
        {:ok,
         %{
           "kind" => kind,
           "name" => subject["name"],
           "path" => asset["path"],
           "asset_id" => asset["asset_id"],
           "refs" => refs
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp table_for("character"), do: "characters"
  defp table_for("scene"), do: "scenes"
  defp table_for("prop"), do: "props"

  defp put_opt(opts, _key, nil), do: opts
  defp put_opt(opts, _key, ""), do: opts
  defp put_opt(opts, key, value), do: Keyword.put(opts, key, value)

  defp write_mirror(dir, script_id, assets) do
    File.mkdir_p(Path.join(dir, "characters"))

    payload =
      assets
      |> Map.put("script_id", script_id)
      |> Map.put("extracted_at", Toonflow.now_iso())

    File.write(
      Path.join([dir, "characters", "appearance.json"]),
      Jason.encode!(payload, pretty: true)
    )
  end

  # --- parsing helpers (pure) ---

  defp normalize_assets(list) when is_list(list) do
    %{"characters" => Enum.map(list, &normalize_character/1), "scenes" => [], "props" => []}
  end

  defp normalize_assets(%{} = decoded) do
    %{
      "characters" => list_of(decoded, "characters", &normalize_character/1),
      "scenes" => list_of(decoded, "scenes", &normalize_named/1),
      "props" => list_of(decoded, "props", &normalize_named/1)
    }
  end

  defp normalize_assets(_other), do: %{"characters" => [], "scenes" => [], "props" => []}

  defp list_of(decoded, key, fun) do
    case Json.list(decoded, key) do
      {:ok, items} -> Enum.map(items, fun)
      {:error, _reason} -> []
    end
  end

  defp normalize_character(%{} = character) do
    %{
      "name" =>
        Json.text(character["name"] || character["角色"] || character["character"]) || "未命名",
      "appearance" =>
        Json.text(character["appearance"] || character["description"] || character["形象"]),
      "role" => Json.text(character["role"] || character["type"] || character["身份"]),
      "refs" => [],
      "meta" =>
        Map.drop(character, [
          "name",
          "角色",
          "character",
          "appearance",
          "description",
          "形象",
          "role",
          "type",
          "身份"
        ])
    }
  end

  defp normalize_character(other) do
    %{
      "name" => Json.text(other) || "未命名",
      "appearance" => nil,
      "role" => nil,
      "refs" => [],
      "meta" => %{"raw" => other}
    }
  end

  defp normalize_named(%{} = item) do
    %{
      "name" => Json.text(item["name"] || item["名称"] || item["title"]) || "未命名",
      "description" => Json.text(item["description"] || item["desc"] || item["描述"]),
      "meta" => Map.drop(item, ["name", "名称", "title", "description", "desc", "描述"])
    }
  end

  defp normalize_named(other) do
    %{"name" => Json.text(other) || "未命名", "description" => nil, "meta" => %{"raw" => other}}
  end
end
