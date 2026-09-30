defmodule Exhub.Toonflow.Memory.Index do
  @moduledoc """
  SQLite + `sqlite-vec` semantic index over Toonflow project memory (Phase 4).

  A single, global index database (`<root>/toonflow_index.db`, override via
  `:exhub -> :toonflow -> "memory" -> "index_path"`) holds every project's
  note chunks; each chunk row carries its `project`, so a search can be scoped
  to one project while sharing one connection (serialized through this
  `GenServer`, the same pattern as `Exhub.MCP.Brain.RAG.VectorIndex`).

  Sources are the markdown notes exported under
  `<root>/workspaces/<project>/memory/notes/` (see `Exhub.Toonflow.Memory`).
  Chunking and embedding are delegated to
  `Exhub.MCP.Brain.RAG.{Chunker, Embedder}` so Toonflow shares the Brain RAG
  embedding stack. The embedder is injectable via
  `:exhub -> :toonflow_embedder` (tests use a fake), and the server name via
  `:exhub -> :toonflow_index_server`.

  Tables:

    * `chunks` — `id`, `project`, `source`, `chunk_index`, `text` (metadata)
    * `vec_chunks` — `id`, `embedding float[N]` (vectors, `vec0`)
    * `files` — content signature per fully-indexed source (change detection)
    * `vector_meta` — the configured embedding dimension
  """

  use GenServer
  require Logger

  alias Exhub.MCP.Brain.RAG.Chunker
  alias Exhub.Toonflow.Config

  @default_batch_size 16
  @default_rebuild_timeout 600_000
  @default_search_timeout 60_000

  # ── client API ───────────────────────────────────────────────────────

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, server_name())
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Index `files` (absolute paths), re-embedding only those whose content
  signature changed since the last build. Returns `{:ok, summary}` with
  `scanned`/`changed`/`indexed`/`failed` counts.
  """
  @spec rebuild([String.t()], GenServer.server()) :: {:ok, map()} | {:error, String.t()}
  def rebuild(files, server \\ nil) do
    GenServer.call(server(server), {:rebuild, files}, rebuild_timeout())
  end

  @doc """
  Return the top-`top_k` chunks most similar to `query`, as
  `[%{project, source, chunk_index, text, similarity}]`.

  `opts`: `:project` (scope results to one project), `:top_k` (default 5),
  `:server`.
  """
  @spec search(String.t(), keyword()) :: {:ok, [map()]} | {:error, String.t()}
  def search(query, opts \\ []) do
    top_k = Keyword.get(opts, :top_k, 5)
    project = Keyword.get(opts, :project)

    # The query is embedded inside the GenServer, so allow for the embedding
    # HTTP round trip rather than GenServer's 5s default.
    GenServer.call(
      server(Keyword.get(opts, :server)),
      {:search, query, project, top_k},
      search_timeout()
    )
  end

  @doc "Return the number of indexed chunks."
  @spec chunk_count(GenServer.server()) :: non_neg_integer()
  def chunk_count(server \\ nil), do: GenServer.call(server(server), :chunk_count)

  @doc "Whether the index holds at least one chunk."
  @spec ready?(GenServer.server()) :: boolean()
  def ready?(server \\ nil), do: chunk_count(server) > 0

  @doc "The path of the Toonflow memory index database."
  @spec index_path() :: String.t()
  def index_path do
    case Config.get("memory", %{})["index_path"] do
      path when is_binary(path) and path != "" -> Path.expand(path)
      _ -> Path.join(Config.root_dir(), "toonflow_index.db")
    end
  end

  @doc "The registered server name for the index (configurable for tests)."
  @spec server_name() :: atom()
  def server_name do
    Application.get_env(:exhub, :toonflow_index_server, __MODULE__)
  end

  @doc "The effective server for a call: an explicit one, else the configured name."
  @spec server(GenServer.server() | nil) :: GenServer.server()
  def server(nil), do: server_name()
  def server(server), do: server

  # ── GenServer callbacks ──────────────────────────────────────────────

  @impl true
  def init(opts) do
    path = Keyword.get(opts, :path) || index_path()
    embedder = Keyword.get(opts, :embedder) || embedder()
    root = Keyword.get(opts, :root) || Config.root_dir()
    File.mkdir_p!(Path.dirname(path))

    case open_db(path, embedder) do
      {:ok, conn} ->
        {:ok, %{conn: conn, path: path, embedder: embedder, root: root}}

      {:error, reason} ->
        Logger.error("[Toonflow.Memory.Index] unavailable at #{path}: #{inspect(reason)}")
        {:ok, %{conn: nil, path: path, embedder: embedder, root: root}}
    end
  end

  @impl true
  def handle_call({:rebuild, _files}, _from, %{conn: nil} = state) do
    {:reply, {:error, "Memory index unavailable (SQLite not open)"}, state}
  end

  def handle_call({:rebuild, files}, _from, state) do
    {:reply, do_rebuild(state, files), state}
  end

  @impl true
  def handle_call({:search, _q, _p, _k}, _from, %{conn: nil} = state) do
    {:reply, {:error, "Memory index unavailable (SQLite not open)"}, state}
  end

  def handle_call({:search, query, project, top_k}, _from, state) do
    {:reply, do_search(state, query, project, top_k), state}
  end

  @impl true
  def handle_call(:chunk_count, _from, %{conn: nil} = state) do
    {:reply, 0, state}
  end

  def handle_call(:chunk_count, _from, state) do
    {:reply, do_chunk_count(state.conn), state}
  end

  # ── rebuild ──────────────────────────────────────────────────────────

  defp do_rebuild(state, files) do
    conn = state.conn
    ensure_schema(conn, state.embedder.dimension())

    changed =
      Enum.filter(files, fn file ->
        signature = file_signature(file)

        case get_stored_signature(conn, file) do
          nil -> signature != nil
          stored -> signature != nil and signature != stored
        end
      end)

    Enum.each(changed, &delete_source_chunks(conn, &1))
    results = embed_changed(state, changed)
    prune_missing(conn, files)

    {:ok,
     %{
       "scanned" => length(files),
       "changed" => length(changed),
       "indexed" => count_ok(results),
       "failed" => count_errors(results),
       "chunks" => do_chunk_count(conn)
     }}
  end

  defp chunk_source(source) do
    case File.read(source) do
      {:ok, content} ->
        chunks = Chunker.chunk(content)
        # Short notes can yield no chunks (the chunker drops anything below its
        # `min_chars`); fall back to the whole note so nothing is silently lost.
        chunks = if chunks == [], do: [%{index: 1, text: body(content)}], else: chunks

        chunks
        |> Enum.reject(fn c -> String.trim(c.text) == "" end)
        |> Enum.map(fn c -> %{source: source, chunk_index: c.index, text: c.text} end)

      {:error, _} ->
        []
    end
  end

  # Strip YAML frontmatter, matching `Exhub.MCP.Brain.RAG.Chunker`.
  defp body(content) do
    case Regex.run(~r/\A---\n.*?\n---\n?(.*)/s, content) do
      [_, rest] -> String.trim(rest)
      _ -> String.trim(content)
    end
  end

  # Embed each changed source; record its signature only when every chunk
  # succeeded, so a partial batch failure is retried on the next rebuild.
  defp embed_changed(state, files) do
    Enum.flat_map(files, fn file ->
      results = embed_chunks(state, chunk_source(file))

      if Enum.all?(results, &match?({:ok, _}, &1)) do
        case file_signature(file) do
          nil -> :ok
          sig -> put_file_signature(state.conn, file, sig)
        end
      end

      results
    end)
  end

  defp embed_chunks(_state, []), do: []

  defp embed_chunks(state, chunks) do
    project = project_of(state.root, hd(chunks).source)

    chunks
    |> Enum.chunk_every(batch_size())
    |> Enum.flat_map(fn batch ->
      texts = Enum.map(batch, & &1.text)

      case state.embedder.encode_batch(texts) do
        {:ok, embeddings} when is_list(embeddings) and length(embeddings) == length(batch) ->
          Enum.zip(batch, embeddings)
          |> Enum.map(fn {chunk, embedding} ->
            store_chunk(state.conn, project, chunk, embedding)
          end)

        {:ok, _} ->
          [{:error, hd(batch).source, "embedding count mismatch"}]

        {:error, reason} ->
          [{:error, hd(batch).source, reason}]
      end
    end)
  end

  defp store_chunk(conn, project, chunk, embedding) do
    id = chunk_id(chunk.source, chunk.chunk_index)

    with :ok <- upsert_chunk_meta(conn, project, chunk, id),
         :ok <- upsert_vec(conn, id, embedding) do
      {:ok, chunk.source}
    else
      {:error, reason} -> {:error, chunk.source, reason}
    end
  end

  defp upsert_chunk_meta(conn, project, chunk, id) do
    execute(
      conn,
      "INSERT OR REPLACE INTO chunks(id, project, source, chunk_index, text) VALUES (?, ?, ?, ?, ?)",
      [id, project, chunk.source, chunk.chunk_index, chunk.text]
    )
  end

  defp upsert_vec(conn, id, embedding) do
    execute(conn, "INSERT OR REPLACE INTO vec_chunks(id, embedding) VALUES (?, ?)", [
      id,
      Jason.encode!(embedding)
    ])
  end

  # ── search ───────────────────────────────────────────────────────────

  defp do_search(state, query, project, top_k) do
    with true <- do_chunk_count(state.conn) > 0,
         {:ok, query_embedding} <- state.embedder.encode(query) do
      {filter, params} = project_filter(project)

      sql = """
      SELECT c.project, c.source, c.chunk_index, c.text,
             vec_distance_cosine(v.embedding, ?) AS distance
      FROM vec_chunks v
      JOIN chunks c ON c.id = v.id
      #{filter}
      ORDER BY distance ASC
      LIMIT ?
      """

      case query_rows(state.conn, sql, [Jason.encode!(query_embedding)] ++ params ++ [top_k]) do
        {:ok, rows} ->
          {:ok, Enum.map(rows, &decode_hit/1)}

        {:error, reason} ->
          {:error, reason}
      end
    else
      false -> {:error, "Memory index is empty — run toonflow_memory_index first"}
      {:error, reason} -> {:error, reason}
    end
  end

  defp project_filter(nil), do: {"", []}
  defp project_filter(project), do: {"WHERE c.project = ?", [project]}

  defp decode_hit([project, source, chunk_index, text, distance]) do
    %{
      "project" => project,
      "source" => source,
      "chunk_index" => to_int(chunk_index),
      "text" => text,
      "similarity" => round3(1.0 - to_float(distance))
    }
  end

  defp do_chunk_count(conn) do
    case query_rows(conn, "SELECT COUNT(*) FROM chunks", []) do
      {:ok, [[count]]} -> to_int(count)
      _ -> 0
    end
  end

  # ── schema ───────────────────────────────────────────────────────────

  defp ensure_schema(conn, dim) do
    execute(
      conn,
      """
      CREATE TABLE IF NOT EXISTS chunks (
        id TEXT PRIMARY KEY,
        project TEXT,
        source TEXT NOT NULL,
        chunk_index INTEGER NOT NULL,
        text TEXT NOT NULL
      );
      """,
      []
    )

    execute(conn, "CREATE INDEX IF NOT EXISTS idx_chunks_source ON chunks(source);", [])
    execute(conn, "CREATE INDEX IF NOT EXISTS idx_chunks_project ON chunks(project);", [])

    execute(
      conn,
      """
      CREATE TABLE IF NOT EXISTS files (
        source TEXT PRIMARY KEY,
        signature TEXT
      );
      """,
      []
    )

    execute(
      conn,
      """
      CREATE TABLE IF NOT EXISTS vector_meta (
        key TEXT PRIMARY KEY,
        value TEXT
      );
      """,
      []
    )

    ensure_vec_table(conn, dim)
  end

  defp ensure_vec_table(conn, dim) do
    dim_str = to_string(dim)
    stored = get_meta(conn, "dim")

    if is_nil(stored) or stored == dim_str do
      create_vec_table(conn, dim)
      put_meta(conn, "dim", dim_str)
    else
      # Dimension changed — the old vectors are incompatible; drop and rebuild.
      execute(conn, "DROP TABLE IF EXISTS vec_chunks", [])
      execute(conn, "DELETE FROM chunks", [])
      execute(conn, "DELETE FROM files", [])
      create_vec_table(conn, dim)
      put_meta(conn, "dim", dim_str)
    end
  end

  defp create_vec_table(conn, dim) do
    execute(
      conn,
      """
      CREATE VIRTUAL TABLE IF NOT EXISTS vec_chunks USING vec0(
        id TEXT PRIMARY KEY,
        embedding float[#{dim}]
      );
      """,
      []
    )
  end

  defp get_meta(conn, key) do
    case query_rows(conn, "SELECT value FROM vector_meta WHERE key = ?", [key]) do
      {:ok, [[value]]} -> value
      _ -> nil
    end
  end

  defp put_meta(conn, key, value) do
    execute(conn, "INSERT OR REPLACE INTO vector_meta(key, value) VALUES (?, ?)", [key, value])
  end

  # ── signature tracking / pruning ─────────────────────────────────────

  defp get_stored_signature(conn, source) do
    case query_rows(conn, "SELECT signature FROM files WHERE source = ?", [source]) do
      {:ok, [[sig]]} -> sig
      _ -> nil
    end
  end

  defp put_file_signature(conn, source, sig) do
    execute(conn, "INSERT OR REPLACE INTO files(source, signature) VALUES (?, ?)", [source, sig])
  end

  defp file_signature(source) do
    case File.read(source) do
      {:ok, content} -> :crypto.hash(:sha256, content) |> Base.encode16()
      {:error, _} -> nil
    end
  end

  defp prune_missing(conn, files) do
    Enum.each(files, fn source ->
      if is_nil(file_signature(source)), do: delete_source_chunks(conn, source)
    end)
  end

  defp delete_source_chunks(conn, source) do
    case query_rows(conn, "SELECT id FROM chunks WHERE source = ?", [source]) do
      {:ok, ids} ->
        Enum.each(ids, fn [id] -> execute(conn, "DELETE FROM vec_chunks WHERE id = ?", [id]) end)
        execute(conn, "DELETE FROM chunks WHERE source = ?", [source])

      _ ->
        :ok
    end

    execute(conn, "DELETE FROM files WHERE source = ?", [source])
  end

  # ── low-level sqlite ─────────────────────────────────────────────────

  defp open_db(path, embedder) do
    {:ok, conn} = Exqlite.Sqlite3.open(path)
    :ok = Exqlite.Sqlite3.enable_load_extension(conn, true)

    case load_vec_extension(conn) do
      :ok ->
        ensure_schema(conn, embedder.dimension())
        {:ok, conn}

      {:error, reason} ->
        Exqlite.Sqlite3.close(conn)
        {:error, reason}
    end
  end

  defp load_vec_extension(conn) do
    case vec_extension_path() do
      nil ->
        {:error, "sqlite-vec extension (vec0) not found"}

      path ->
        with {:ok, stmt} <- Exqlite.Sqlite3.prepare(conn, "SELECT load_extension(?)"),
             :ok <- Exqlite.Sqlite3.bind(stmt, [path]),
             _ <- Exqlite.Sqlite3.step(conn, stmt),
             :ok <- Exqlite.Sqlite3.release(conn, stmt) do
          :ok
        else
          {:error, reason} -> {:error, reason}
          _ -> :ok
        end
    end
  end

  defp vec_extension_path do
    base = Application.app_dir(:sqlite_vec, "priv")
    version = Application.get_env(:sqlite_vec, :version, "0.1.5")

    candidates = [
      Application.app_dir(:sqlite_vec, "priv/#{version}/vec0"),
      Application.app_dir(:sqlite_vec, "priv/#{version}/vec0.dylib"),
      Application.app_dir(:sqlite_vec, "priv/#{version}/vec0.so")
    ]

    (candidates ++ Path.wildcard(Path.join(base, "**/vec0.*")))
    |> Enum.find(&File.exists?/1)
  end

  defp execute(conn, sql, params) do
    with {:ok, stmt} <- Exqlite.Sqlite3.prepare(conn, sql),
         :ok <- Exqlite.Sqlite3.bind(stmt, params),
         _ <- Exqlite.Sqlite3.step(conn, stmt),
         :ok <- Exqlite.Sqlite3.release(conn, stmt) do
      :ok
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp query_rows(conn, sql, params) do
    with {:ok, stmt} <- Exqlite.Sqlite3.prepare(conn, sql),
         :ok <- Exqlite.Sqlite3.bind(stmt, params) do
      rows = collect_rows(conn, stmt, [])
      :ok = Exqlite.Sqlite3.release(conn, stmt)
      {:ok, rows}
    end
  end

  defp collect_rows(conn, stmt, acc) do
    case Exqlite.Sqlite3.step(conn, stmt) do
      {:row, row} -> collect_rows(conn, stmt, [row | acc])
      :done -> Enum.reverse(acc)
      :busy -> collect_rows(conn, stmt, acc)
    end
  end

  # ── helpers ──────────────────────────────────────────────────────────

  @doc "Derive the project name from a source path under `<root>/workspaces/`."
  @spec project_of(String.t(), String.t()) :: String.t() | nil
  def project_of(root, source) do
    root_parts = Path.split(Path.expand(root))
    source_parts = Path.split(Path.expand(source))

    case Enum.drop(source_parts, length(root_parts)) do
      ["workspaces", project | _] -> project
      _ -> nil
    end
  end

  defp chunk_id(source, index), do: "#{source}##{index}"

  defp embedder do
    Application.get_env(:exhub, :toonflow_embedder, Exhub.MCP.Brain.RAG.Embedder)
  end

  defp batch_size, do: memory_cfg()["batch_size"] || @default_batch_size

  defp rebuild_timeout do
    memory_cfg()["rebuild_timeout"] || @default_rebuild_timeout
  end

  defp search_timeout do
    memory_cfg()["search_timeout"] || @default_search_timeout
  end

  defp memory_cfg do
    :exhub |> Application.get_env(:toonflow, %{}) |> Map.get("memory", %{})
  end

  defp count_ok(results) do
    results
    |> Enum.filter(&match?({:ok, _}, &1))
    |> Enum.map(fn {:ok, f} -> f end)
    |> Enum.uniq()
    |> length()
  end

  defp count_errors(results) do
    results |> Enum.filter(&match?({:error, _, _}, &1)) |> Enum.uniq() |> length()
  end

  defp to_int(v) when is_integer(v), do: v
  defp to_int(v) when is_float(v), do: trunc(v)

  defp to_int(v) when is_binary(v) do
    case Integer.parse(v) do
      {i, _} -> i
      _ -> 0
    end
  end

  defp to_int(_), do: 0

  defp to_float(v) when is_float(v), do: v
  defp to_float(v) when is_number(v), do: v * 1.0
  defp to_float(_), do: 0.0

  defp round3(v) when is_float(v), do: Float.round(v, 3)
  defp round3(v), do: v
end
