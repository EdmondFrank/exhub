defmodule Exhub.Toonflow.Memory do
  @moduledoc """
  Project memory notes for Toonflow.

  CRUD over the `memory_notes` table, plus the Phase 4 semantic layer: notes
  are exported to markdown (`export_notes/3`), indexed with `sqlite-vec` via
  `Exhub.Toonflow.Memory.Index` (`index/3`), searched (`search/3`) and recalled
  as a prompt fragment (`recall/3`) for the script/storyboard stages.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Config, DB, Schema, Store}
  alias Exhub.Toonflow.Memory.Index

  @doc """
  Add a memory note. `opts`: `:kind` (default `"note"`), `:title`, `:text`
  (required), `:meta` (a map).
  """
  @spec add_note(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def add_note(project, opts \\ [], server \\ Store) do
    kind = Keyword.get(opts, :kind) || "note"
    title = Toonflow.blank(Keyword.get(opts, :title)) || "未命名"
    text = Toonflow.blank(Keyword.get(opts, :text))
    meta = Keyword.get(opts, :meta) || %{}

    if is_nil(text) do
      {:error, :missing_text}
    else
      id = Toonflow.new_id("mem")
      now = Toonflow.now_iso()

      result =
        Store.run_project(
          project,
          fn conn ->
            DB.execute(
              conn,
              "INSERT INTO memory_notes (id, kind, title, text, meta_json, created_at) VALUES (?, ?, ?, ?, ?, ?)",
              [id, kind, title, text, Schema.encode_json(meta), now]
            )
          end,
          server
        )

      case result do
        :ok ->
          {:ok,
           %{
             "id" => id,
             "kind" => kind,
             "title" => title,
             "text" => text,
             "meta" => meta,
             "created_at" => now
           }}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  @doc "List memory notes, newest first. `opts`: `:kind`, `:limit`."
  @spec list_notes(String.t(), keyword(), GenServer.server()) :: {:ok, [map()]} | {:error, term()}
  def list_notes(project, opts \\ [], server \\ Store) do
    kind = Toonflow.blank(Keyword.get(opts, :kind))
    limit = Keyword.get(opts, :limit)
    {where, params} = if kind, do: {" WHERE kind = ?", [kind]}, else: {"", []}

    sql =
      "SELECT #{Schema.memory_note_columns()} FROM memory_notes" <>
        where <> " ORDER BY created_at DESC"

    Store.run_project(
      project,
      fn conn ->
        case DB.query(conn, sql, params) do
          {:ok, rows} ->
            rows = rows |> Enum.map(&Schema.decode_memory_note/1) |> Toonflow.maybe_limit(limit)
            {:ok, rows}

          {:error, reason} ->
            {:error, reason}
        end
      end,
      server
    )
  end

  @doc """
  Export a project's memory notes as markdown files under
  `<project>/memory/notes/<id>.md`, so they can be indexed and browsed.

  Returns `{:ok, %{"exported" => n, "files" => [path]}}`.
  """
  @spec export_notes(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def export_notes(project, opts \\ [], server \\ Store) do
    with {:ok, meta} <- Store.get_project(project, server),
         {:ok, notes} <- list_notes(project, opts, server) do
      # `meta["root_dir"]` is the project directory itself.
      dir = Path.join([meta["root_dir"], "memory", "notes"])

      with :ok <- File.mkdir_p(dir) do
        files =
          Enum.map(notes, fn note ->
            path = Path.join(dir, "#{note["id"]}.md")
            File.write!(path, note_markdown(note))
            path
          end)

        {:ok, %{"exported" => length(files), "files" => files, "dir" => dir}}
      end
    end
  end

  @doc """
  Export the project's notes and (re)build them into the semantic index.

  Returns the `Exhub.Toonflow.Memory.Index.rebuild/2` summary.
  """
  @spec index(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def index(project, opts \\ [], server \\ Store) do
    case export_notes(project, opts, server) do
      {:ok, %{"files" => files}} -> Index.rebuild(files)
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Semantically search a project's indexed memory. `opts`: `:query` (required),
  `:top_k`. Returns `[%{"text", "similarity", "source", "chunk_index"}]`.
  """
  @spec search(String.t(), keyword(), GenServer.server()) :: {:ok, [map()]} | {:error, term()}
  def search(project, opts \\ [], _server \\ Store) do
    case Toonflow.blank(Keyword.get(opts, :query)) do
      nil -> {:error, :missing_query}
      query -> Index.search(query, project: project, top_k: Keyword.get(opts, :top_k))
    end
  end

  @doc """
  Recall relevant memory as a prompt fragment for a generation stage.

  Best-effort: returns `{:ok, ""}` when recall is disabled, the query is
  blank, or the index is empty/unavailable — generation must never fail on it.
  """
  @spec recall(String.t(), keyword(), GenServer.server()) :: {:ok, String.t()}
  def recall(project, opts \\ [], server \\ Store) do
    query = Toonflow.blank(Keyword.get(opts, :query))
    config = memory_config()

    if is_nil(query) or config["enabled"] == false do
      {:ok, ""}
    else
      top_k = Keyword.get(opts, :top_k) || config["top_k"] || 5

      case search(project, [query: query, top_k: top_k], server) do
        {:ok, hits} when hits != [] -> {:ok, format_recall(hits)}
        _ -> {:ok, ""}
      end
    end
  end

  # --- helpers ---

  defp memory_config, do: Config.get("memory", %{}) || %{}

  defp note_markdown(note) do
    meta = note["meta"] || %{}

    front =
      [
        "---",
        "id: #{note["id"]}",
        "kind: #{note["kind"]}",
        "title: #{note["title"]}",
        "created_at: #{note["created_at"]}",
        if(meta == %{}, do: nil, else: "meta: #{Schema.encode_json(meta)}"),
        "---"
      ]
      |> Enum.reject(&is_nil/1)
      |> Enum.join("\n")

    "#{front}\n\n# #{note["title"]}\n\n#{note["text"]}\n"
  end

  defp format_recall(hits) do
    items =
      hits
      |> Enum.map(fn hit -> "- [#{hit["similarity"]}] #{Toonflow.preview(hit["text"], 200)}" end)
      |> Enum.join("\n")

    "（项目记忆，来自既往笔记，供参考）\n#{items}"
  end
end
