defmodule Exhub.Toonflow.Storyboard do
  @moduledoc """
  Storyboard (shot) planning for Toonflow.

  Turns a script into an ordered shot list via the DirectorAgent LLM call
  (`storyboard.system` / `storyboard.user`), grounded in the extracted character
  appearances: each shot carries a scene, description, 景别 (`size`),
  lighting, 运镜 (`motion`), the characters it features, and a text-to-image
  `prompt`. Shots are keyed by `script_id` and replaced on re-run (idempotent),
  and mirrored to `storyboards/<script_id>.json`.

  `parse_storyboard/1` and `shot_prompt/2` are pure and unit-tested.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Assets, DB, Json, LLM, Memory, Prompts, Schema, Script, Store}

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
    instructions = Keyword.get(opts, :instructions) || ""

    with {:ok, script} <- fetch_script(project, script_id, server),
         {:ok, characters} <- Assets.list_characters(project, [], server),
         {:ok, recall} <- maybe_recall(project, opts, script["content"] || "", server),
         {:ok, system} <- Prompts.render("storyboard.system", %{}),
         {:ok, user} <-
           Prompts.render("storyboard.user", %{
             "script" => script["content"] || "",
             "characters" => characters_digest(characters),
             "instructions" => append_instructions(instructions, recall)
           }),
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
  Build a text-to-image prompt for a `shot`, appending the appearance of each
  referenced character for consistency. Pure.
  """
  @spec shot_prompt(map(), [map()]) :: String.t()
  def shot_prompt(shot, characters \\ []) do
    base = Json.text(shot["prompt"]) || compose_prompt(shot)

    appearances =
      (shot["characters"] || [])
      |> Enum.flat_map(fn name ->
        case Enum.find(characters, &(&1["name"] == name)) do
          %{"appearance" => appearance} when is_binary(appearance) -> ["#{name}（#{appearance}）"]
          _ -> []
        end
      end)

    [base | appearances]
    |> Enum.reject(&(is_nil(&1) or &1 == ""))
    |> Enum.join("；")
  end

  # --- persistence ---

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
                    :ok -> {:cont, {:ok, count + 1}}
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

    DB.execute(
      conn,
      """
      INSERT INTO shots (id, script_id, idx, scene, shot_desc, size, camera, lighting, motion, prompt, meta_json)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
      """,
      [
        Toonflow.new_id("sht"),
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
    )
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
          "角色"
        ])
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
