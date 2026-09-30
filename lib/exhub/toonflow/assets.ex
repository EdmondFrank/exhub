defmodule Exhub.Toonflow.Assets do
  @moduledoc """
  Character / scene / prop extraction for Toonflow.

  Reads a script (the latest by default, or a specific `script_id`), asks the
  LLM (`assets.system` / `assets.user`) for a cast list with stable appearances,
  and upserts the characters into the project `characters` table by name —
  re-running is idempotent. The full extraction (characters + scenes + props)
  is mirrored to `characters/appearance.json`, so the appearance database is
  inspectable alongside the rest of the workspace.

  `parse_assets/1` is pure and unit-tested.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{DB, Json, LLM, Prompts, Schema, Script, Store}

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
    instructions = Keyword.get(opts, :instructions) || ""

    with {:ok, script} <- fetch_script(project, script_id, server),
         {:ok, system} <- Prompts.render("assets.system", %{}),
         {:ok, user} <-
           Prompts.render("assets.user", %{
             "script" => script["content"] || "",
             "instructions" => instructions
           }),
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
              assets["characters"]
              |> Enum.reduce_while({:ok, 0}, fn character, {:ok, count} ->
                case upsert_character(conn, character) do
                  :ok -> {:cont, {:ok, count + 1}}
                  {:error, reason} -> {:halt, {:error, reason}}
                end
              end)
            end)
          end,
          server
        )

      case result do
        {:ok, count} ->
          write_mirror(dir, script_id, assets)

          {:ok,
           %{
             "script_id" => script_id,
             "characters" => count,
             "scenes" => length(assets["scenes"]),
             "props" => length(assets["props"]),
             "character_names" => Enum.map(assets["characters"], & &1["name"])
           }}

        {:error, reason} ->
          {:error, reason}
      end
    end
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
