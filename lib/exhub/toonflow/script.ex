defmodule Exhub.Toonflow.Script do
  @moduledoc """
  Script generation, retrieval and versioning.

  Turns chapter text plus its extracted event graph into a short-drama script
  (LLM, templates `script.system` / `script.user`) and stores it in the
  `scripts` table. Every generation or edit inserts a new immutable version, so
  history is preserved.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{DB, Events, LLM, Memory, Novel, Prompts, Schema, Store}

  @doc """
  Generate a script.

  `opts`:
    * `:chapter_id` — a single chapter; omit to adapt the whole novel.
    * `:instructions` — extra director/writer guidance.
    * `:recall` — when truthy, append relevant project memory (semantic
      recall) to the instructions; best-effort and never fatal.
    * `:recall_top_k` — number of memory hits to recall (default from config).

  Returns `{:ok, script}` with the new version, or `{:error, reason}`.
  """
  @spec generate_script(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def generate_script(project, opts \\ [], server \\ Store) do
    chapter_id = Toonflow.blank(Keyword.get(opts, :chapter_id))
    instructions = Keyword.get(opts, :instructions) || ""

    with {:ok, chapters} <- Novel.chapters_for(project, chapter_id, server) do
      case chapters do
        [] ->
          {:error, :no_chapters}

        chapters ->
          {title, text} = source_of(chapters, chapter_id)

          with {:ok, events} <- Events.list_events(project, [chapter_id: chapter_id], server),
               {:ok, recall} <- maybe_recall(project, opts, title, server),
               {:ok, system} <- Prompts.render("script.system", %{}),
               {:ok, user} <-
                 Prompts.render("script.user", %{
                   "title" => title,
                   "text" => text,
                   "events" => events_digest(events),
                   "instructions" => append_instructions(instructions, recall)
                 }),
               {:ok, raw} <- LLM.call_llm(system, user, []) do
            persist(project, chapter_id, parse_script(raw), instructions, server)
          end
      end
    end
  end

  @doc """
  Fetch a script. `opts`: `:script_id`, or `:chapter_id` (+ optional `:version`);
  with neither, the most recent script in the project is returned.
  """
  @spec get_script(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def get_script(project, opts \\ [], server \\ Store) do
    script_id = Toonflow.blank(Keyword.get(opts, :script_id))
    chapter_id = Toonflow.blank(Keyword.get(opts, :chapter_id))
    version = Keyword.get(opts, :version)

    Store.run_project(
      project,
      fn conn -> fetch(conn, script_id, chapter_id, version) end,
      server
    )
  end

  @doc """
  Apply an edit to a script as a new version. `opts`: `:script_id` (required),
  `:content` (required), `:note` (optional).
  """
  @spec update_script(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def update_script(project, opts \\ [], server \\ Store) do
    script_id = Toonflow.blank(Keyword.get(opts, :script_id))
    content = Toonflow.blank(Keyword.get(opts, :content))
    note = Keyword.get(opts, :note) || ""

    cond do
      is_nil(script_id) ->
        {:error, :missing_script_id}

      is_nil(content) ->
        {:error, :missing_content}

      true ->
        Store.run_project(
          project,
          fn conn -> do_update(conn, script_id, content, note) end,
          server
        )
    end
  end

  @doc "Normalize an LLM script response (strips code fences, trims)."
  @spec parse_script(String.t()) :: String.t()
  def parse_script(raw) when is_binary(raw) do
    raw
    |> String.trim()
    |> strip_fence()
    |> String.replace(~r/```\s*$/, "")
    |> String.trim()
  end

  def parse_script(_), do: ""

  # --- persistence ---

  defp persist(project, chapter_id, content, instructions, server) do
    meta = %{"instructions" => instructions, "generator" => "toonflow_generate_script"}

    Store.run_project(
      project,
      fn conn -> insert_version(conn, chapter_id, content, meta) end,
      server
    )
  end

  defp do_update(conn, script_id, content, note) do
    with {:ok, original} <- fetch(conn, script_id, nil, nil) do
      chapter_id = original["chapter_id"]
      meta = %{"note" => note, "generator" => "toonflow_update_script", "parent" => script_id}
      insert_version(conn, chapter_id, content, meta)
    end
  end

  defp insert_version(conn, chapter_id, content, meta) do
    version = next_version(conn, chapter_id)
    id = Toonflow.new_id("scr")
    now = Toonflow.now_iso()

    case DB.execute(
           conn,
           "INSERT INTO scripts (id, chapter_id, version, format, content, meta_json, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)",
           [id, chapter_id, version, "markdown", content, Schema.encode_json(meta), now]
         ) do
      :ok ->
        {:ok,
         %{
           "script_id" => id,
           "chapter_id" => chapter_id,
           "version" => version,
           "format" => "markdown",
           "content" => content,
           "content_length" => String.length(content),
           "meta" => meta,
           "created_at" => now
         }}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp next_version(conn, chapter_id) do
    {sql, params} =
      if chapter_id do
        {"SELECT COALESCE(MAX(version), 0) + 1 FROM scripts WHERE chapter_id = ?", [chapter_id]}
      else
        {"SELECT COALESCE(MAX(version), 0) + 1 FROM scripts WHERE chapter_id IS NULL", []}
      end

    case DB.query_one(conn, sql, params) do
      {:ok, [version]} when is_integer(version) -> version
      _ -> 1
    end
  end

  defp fetch(conn, script_id, chapter_id, version) do
    {sql, params} =
      cond do
        script_id ->
          {"SELECT #{Schema.script_columns()} FROM scripts WHERE id = ?", [script_id]}

        chapter_id && version ->
          {"SELECT #{Schema.script_columns()} FROM scripts WHERE chapter_id = ? AND version = ? LIMIT 1",
           [chapter_id, version]}

        chapter_id ->
          {"SELECT #{Schema.script_columns()} FROM scripts WHERE chapter_id = ? ORDER BY version DESC LIMIT 1",
           [chapter_id]}

        true ->
          {"SELECT #{Schema.script_columns()} FROM scripts ORDER BY created_at DESC LIMIT 1", []}
      end

    case DB.query_one(conn, sql, params) do
      {:ok, nil} -> {:error, :not_found}
      {:ok, row} -> {:ok, Schema.decode_script(row)}
      {:error, reason} -> {:error, reason}
    end
  end

  # --- helpers ---

  defp source_of([chapter], chapter_id) when not is_nil(chapter_id) do
    {chapter["title"] || "", chapter["text"] || ""}
  end

  defp source_of(chapters, _chapter_id) do
    text = chapters |> Enum.map(&(&1["text"] || "")) |> Enum.join("\n\n")
    {"全文", text}
  end

  defp events_digest([]), do: "（无，未提取事件图）"

  defp events_digest(events) do
    events
    |> Enum.sort_by(& &1["idx"])
    |> Enum.map_join("\n", fn event ->
      "- [#{event["kind"]}] #{event["summary"]}"
    end)
  end

  defp maybe_recall(project, opts, query, server) do
    if Keyword.get(opts, :recall) do
      Memory.recall(project, [query: query, top_k: Keyword.get(opts, :recall_top_k)], server)
    else
      {:ok, ""}
    end
  end

  defp append_instructions(instructions, ""), do: instructions
  defp append_instructions(instructions, recall) when instructions in [nil, ""], do: recall
  defp append_instructions(instructions, recall), do: instructions <> "\n\n" <> recall

  defp strip_fence(text), do: String.replace(text, ~r/^(```|~~~)[a-zA-Z0-9_-]*\s*\n?/, "")
end
