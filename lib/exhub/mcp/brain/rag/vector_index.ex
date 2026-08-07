defmodule Exhub.MCP.Brain.RAG.VectorIndex do
  @moduledoc """
  SQLite-backed vector index for Brain RAG, using the `sqlite-vec` (`vec0`)
  extension.

  The index is a single SQLite database file (default `~/.config/exhub/brain_index.db`,
  configurable via `:exhub -> :brain_rag -> "index_path"`) that stores:

    * a `chunks` table — `file`, `chunk_index`, `text` (metadata)
    * a `vec_chunks` virtual table — `id`, `embedding float[N]` (vectors)

  Chunk `id` is `"\#{file}##\#{chunk_index}"`. The index is rebuilt lazily by
  `rebuild/0`, which re-chunks and re-embeds only files whose content signature
  changed since the last build (tracked in the `chunks` table), so repeated
  rebuilds are cheap.

  All public functions route through the `GenServer` so access to the SQLite
  connection is serialized.
  """

  use GenServer
  require Logger

  alias Exhub.MCP.Brain.RAG.Chunker
  alias Exhub.MCP.Brain.RAG.Embedder

  @default_index_path Path.join(System.user_home!(), ".config/exhub/brain_index.db")
  @default_batch_size 16
  @default_rebuild_timeout 600_000

  @type chunk_map :: %{file: String.t(), chunk_index: pos_integer(), text: String.t()}

  def start_link(opts) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc "Rebuild the index for changed files in `files` (absolute paths)."
  @spec rebuild([String.t()], GenServer.server()) :: {:ok, map()} | {:error, String.t()}
  def rebuild(files, server \\ __MODULE__) do
    # A first-time full rebuild embeds every file, which can take minutes, so
    # use a generous, configurable timeout instead of GenServer's 5s default.
    GenServer.call(server, {:rebuild, files}, rebuild_timeout())
  end

  @doc """
  Search for the top-`top_k` chunks most similar to `query`.

  Returns `{:ok, [%{file, chunk_index, text, similarity}]}` or
  `{:error, reason}` if the index is empty/unavailable.
  """
  @spec search(String.t(), keyword()) :: {:ok, [map()]} | {:error, String.t()}
  def search(query, opts \\ []) do
    top_k = Keyword.get(opts, :top_k, 5)
    server = Keyword.get(opts, :server, __MODULE__)
    GenServer.call(server, {:search, query, top_k})
  end

  @doc "Return the number of indexed chunks."
  @spec chunk_count(GenServer.server()) :: non_neg_integer()
  def chunk_count(server \\ __MODULE__), do: GenServer.call(server, :chunk_count)

  @doc "Return the configured SQLite index path."
  @spec index_path() :: String.t()
  def index_path do
    Application.get_env(:exhub, :brain_rag, %{})["index_path"] || @default_index_path
  end

  @doc "Return the registered server name for the index (configurable for tests)."
  @spec registry_name() :: atom()
  def registry_name do
    Application.get_env(:exhub, :brain_rag, %{})["vector_index_server"] || __MODULE__
  end

  # ── GenServer callbacks ──────────────────────────────────────────────

  @impl true
  def init(_opts) do
    path = index_path()
    File.mkdir_p!(Path.dirname(path))

    case open_db(path) do
      {:ok, conn} ->
        {:ok, %{conn: conn, path: path}}

      {:error, reason} ->
        Logger.error("[BrainRAG] Failed to open vector index at #{path}: #{inspect(reason)}")
        {:ok, %{conn: nil, path: path}}
    end
  end

  @impl true
  def handle_call({:rebuild, _files}, _from, %{conn: nil} = state) do
    {:reply, {:error, "Vector index unavailable (SQLite not open)"}, state}
  end

  def handle_call({:rebuild, files}, _from, state) do
    result = do_rebuild(state.conn, files)
    {:reply, result, state}
  end

  def handle_call({:search, _query, _top_k}, _from, %{conn: nil} = state) do
    {:reply, {:error, "Vector index unavailable (SQLite not open)"}, state}
  end

  def handle_call({:search, query, top_k}, _from, state) do
    result = do_search(state.conn, query, top_k)
    {:reply, result, state}
  end

  def handle_call(:chunk_count, _from, %{conn: nil} = state) do
    {:reply, 0, state}
  end

  def handle_call(:chunk_count, _from, state) do
    {:reply, do_chunk_count(state.conn), state}
  end

  # ── rebuild ──────────────────────────────────────────────────────────

  defp do_rebuild(conn, files) do
    ensure_schema(conn)

    changed =
      Enum.filter(files, fn file ->
        signature = file_signature(file)
        case get_stored_signature(conn, file) do
          nil -> signature != nil
          stored -> signature != nil and signature != stored
        end
      end)

    # Drop any existing chunks for changed files before re-embedding, so a
    # shrink in chunk count (trimmed sections, content below min_chars) never
    # leaves stale higher-index chunks behind.
    Enum.each(changed, &delete_file_chunks(conn, &1))

    results = embed_changed(conn, changed)

    # Remove stale chunks for files that no longer exist on disk.
    prune_deleted(conn, files)

    indexed =
      results
      |> Enum.filter(fn {:ok, _} -> true; _ -> false end)
      |> Enum.map(fn {:ok, file} -> file end)
      |> Enum.uniq()
      |> length()

    failed =
      results
      |> Enum.filter(fn {:error, _, _} -> true; _ -> false end)
      |> Enum.map(fn {:error, file, _} -> file end)
      |> Enum.uniq()
      |> length()

    {:ok,
     %{
       scanned: length(files),
       changed: length(changed),
       indexed: indexed,
       failed: failed
     }}
  end

  defp chunk_file(file) do
    case File.read(file) do
      {:ok, content} ->
        chunks = Chunker.chunk(content)

        Enum.map(chunks, fn c ->
          %{file: file, chunk_index: c.index, text: c.text}
        end)

      {:error, _} ->
        []
    end
  end

  # Embed each changed file and record its signature only when the whole file
  # succeeded. Because change detection reads the `files` table (not the
  # per-chunk rows), a partial batch failure leaves no signature behind, so the
  # next rebuild retries the file instead of misclassifying it as unchanged.
  defp embed_changed(conn, files) do
    Enum.flat_map(files, fn file ->
      results = embed_chunks(chunk_file(file), conn)

      if Enum.all?(results, &match?({:ok, _}, &1)) do
        case file_signature(file) do
          nil -> :ok
          sig -> put_file_signature(conn, file, sig)
        end
      end

      results
    end)
  end

  defp embed_chunks([], _conn), do: []

  defp embed_chunks(chunks, conn) do
    chunks
    |> Enum.chunk_every(batch_size())
    |> Enum.flat_map(fn batch ->
      texts = Enum.map(batch, & &1.text)
      files = batch |> Enum.map(& &1.file) |> Enum.uniq()

      case embedder().encode_batch(texts) do
        {:ok, embeddings} when is_list(embeddings) and length(embeddings) == length(batch) ->
          Enum.zip(batch, embeddings)
          |> Enum.map(fn {chunk, embedding} ->
            store_chunk(conn, chunk, embedding)
          end)

        {:ok, _} ->
          Enum.map(files, &{:error, &1, "embedding count mismatch"})

        {:error, reason} ->
          Enum.map(files, &{:error, &1, reason})
      end
    end)
  end

  defp store_chunk(conn, chunk, embedding) do
    id = chunk_id(chunk.file, chunk.chunk_index)

    with :ok <- upsert_chunk_meta(conn, chunk, id),
         :ok <- upsert_vec(conn, id, embedding) do
      {:ok, chunk.file}
    else
      {:error, reason} -> {:error, chunk.file, reason}
    end
  end

  defp upsert_chunk_meta(conn, chunk, id) do
    execute(
      conn,
      "INSERT OR REPLACE INTO chunks(id, file, chunk_index, text) VALUES (?, ?, ?, ?)",
      [id, chunk.file, chunk.chunk_index, chunk.text]
    )
  end

  defp upsert_vec(conn, id, embedding) do
    vec = Jason.encode!(embedding)
    execute(conn, "INSERT OR REPLACE INTO vec_chunks(id, embedding) VALUES (?, ?)", [id, vec])
  end

  # ── search ───────────────────────────────────────────────────────────

  defp do_search(conn, query, top_k) do
    with true <- index_ready?(conn),
         {:ok, query_embedding} <- embedder().encode(query) do
      vec = Jason.encode!(query_embedding)

      sql = """
      SELECT c.file, c.chunk_index, c.text,
             vec_distance_cosine(v.embedding, ?) as distance
      FROM vec_chunks v
      JOIN chunks c ON c.id = v.id
      ORDER BY distance ASC
      LIMIT ?
      """

      case query_rows(conn, sql, [vec, top_k]) do
        {:ok, rows} ->
          results =
            Enum.map(rows, fn [file, chunk_index, text, distance] ->
              %{
                file: file,
                chunk_index: chunk_index,
                text: text,
                similarity: round3(1.0 - to_float(distance))
              }
            end)

          {:ok, results}

        {:error, reason} ->
          {:error, reason}
      end
    else
      false -> {:error, "Vector index is empty — rebuild it first"}
      {:error, reason} -> {:error, reason}
    end
  end

  defp index_ready?(conn) do
    do_chunk_count(conn) > 0
  end

  defp do_chunk_count(conn) do
    case query_rows(conn, "SELECT COUNT(*) FROM chunks", []) do
      {:ok, [[count]]} -> to_int(count)
      _ -> 0
    end
  end

  # ── schema ───────────────────────────────────────────────────────────

  defp ensure_schema(conn) do
    execute(conn, """
    CREATE TABLE IF NOT EXISTS chunks (
      id TEXT PRIMARY KEY,
      file TEXT NOT NULL,
      chunk_index INTEGER NOT NULL,
      text TEXT NOT NULL
    );
    """, [])

    execute(conn, """
    CREATE INDEX IF NOT EXISTS idx_chunks_file ON chunks(file);
    """, [])

    # Tracks the content signature of each file that was FULLY indexed. This is
    # what change detection reads, so a partially-embedded file is never
    # mistaken for up-to-date.
    execute(conn, """
    CREATE TABLE IF NOT EXISTS files (
      file TEXT PRIMARY KEY,
      signature TEXT
    );
    """, [])

    execute(conn, """
    CREATE TABLE IF NOT EXISTS vector_meta (
      key TEXT PRIMARY KEY,
      value TEXT
    );
    """, [])

    ensure_vec_table(conn, embedder().dimension())
  end

  defp ensure_vec_table(conn, dim) do
    dim_str = to_string(dim)
    stored = get_meta(conn, "dim")

    if is_nil(stored) or stored == dim_str do
      execute(conn, """
      CREATE VIRTUAL TABLE IF NOT EXISTS vec_chunks USING vec0(
        id TEXT PRIMARY KEY,
        embedding float[#{dim}]
      );
      """, [])
      put_meta(conn, "dim", dim_str)
    else
      # Configured dimension changed — rebuild the vector table at the new
      # dimension and clear stored signatures (chunks + files) so the next
      # rebuild re-embeds everything (the old vectors are incompatible).
      execute(conn, "DROP TABLE IF EXISTS vec_chunks", [])
      execute(conn, "DELETE FROM chunks", [])
      execute(conn, "DELETE FROM files", [])
      execute(conn, """
      CREATE VIRTUAL TABLE IF NOT EXISTS vec_chunks USING vec0(
        id TEXT PRIMARY KEY,
        embedding float[#{dim}]
      );
      """, [])
      put_meta(conn, "dim", dim_str)
    end
  end

  defp get_meta(conn, key) do
    case query_rows(conn, "SELECT value FROM vector_meta WHERE key = ?", [key]) do
      {:ok, [[value]]} -> value
      _ -> nil
    end
  end

  defp put_meta(conn, key, value) do
    execute(
      conn,
      "INSERT OR REPLACE INTO vector_meta(key, value) VALUES (?, ?)",
      [key, value]
    )
  end

  # ── low-level sqlite helpers ─────────────────────────────────────────

  defp open_db(path) do
    {:ok, conn} = Exqlite.Sqlite3.open(path)
    :ok = Exqlite.Sqlite3.enable_load_extension(conn, true)

    case load_vec_extension(conn) do
      :ok ->
        ensure_schema(conn)
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
    # Prefer SqliteVec.path() (the deps downloader), but fall back to
    # scanning the priv dir for the actual platform extension name.
    base = Application.app_dir(:sqlite_vec, "priv")
    version = Application.get_env(:sqlite_vec, :version, "0.1.5")

    candidates = [
      Application.app_dir(:sqlite_vec, "priv/#{version}/vec0"),
      Application.app_dir(:sqlite_vec, "priv/#{version}/vec0.dylib"),
      Application.app_dir(:sqlite_vec, "priv/#{version}/vec0.so")
    ]

    wildcard = Path.join(base, "**/vec0.*")

    (candidates ++ Path.wildcard(wildcard))
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

  # ── signature tracking ───────────────────────────────────────────────

  # Change detection reads the `files` table, which only holds signatures for
  # files that were FULLY indexed. Per-chunk rows in `chunks` are not consulted
  # here, so a partial embed failure cannot leave a stale "up-to-date" marker.
  defp get_stored_signature(conn, file) do
    case query_rows(conn, "SELECT signature FROM files WHERE file = ?", [file]) do
      {:ok, [[nil]]} -> nil
      {:ok, [[sig]]} -> sig
      _ -> nil
    end
  end

  defp put_file_signature(conn, file, sig) do
    execute(conn, "INSERT OR REPLACE INTO files(file, signature) VALUES (?, ?)", [file, sig])
  end

  defp file_signature(file) do
    case File.read(file) do
      {:ok, content} -> signature(content)
      {:error, _} -> nil
    end
  end

  # ── stale entry pruning ──────────────────────────────────────────────

  defp prune_deleted(conn, files) do
    Enum.each(files, fn file ->
      # A nil signature means the file is gone from disk (or unreadable).
      if is_nil(file_signature(file)) do
        delete_file_chunks(conn, file)
      end
    end)
  end

  defp delete_file_chunks(conn, file) do
    case query_rows(conn, "SELECT id FROM chunks WHERE file = ?", [file]) do
      {:ok, ids} ->
        Enum.each(ids, fn [id] ->
          execute(conn, "DELETE FROM vec_chunks WHERE id = ?", [id])
        end)

        execute(conn, "DELETE FROM chunks WHERE file = ?", [file])

      _ ->
        :ok
    end

    # Remove the fully-built marker too, so a file whose rebuild fails is
    # retried on the next pass rather than being treated as up-to-date.
    execute(conn, "DELETE FROM files WHERE file = ?", [file])
  end

  defp signature(content) when is_binary(content), do: :crypto.hash(:sha256, content) |> Base.encode16()

  # ── helpers ──────────────────────────────────────────────────────────

  defp chunk_id(file, index), do: "#{file}##{index}"

  defp batch_size, do: Application.get_env(:exhub, :brain_rag, %{})["batch_size"] || @default_batch_size

  defp rebuild_timeout do
    Application.get_env(:exhub, :brain_rag, %{})["rebuild_timeout"] || @default_rebuild_timeout
  end

  defp embedder do
    Application.get_env(:exhub, :brain_rag, %{})["embedder_module"] || Embedder
  end

  defp to_int(v) when is_integer(v), do: v
  defp to_int(v) when is_float(v), do: trunc(v)
  defp to_int(v) when is_binary(v) do
    case Integer.parse(v) do
      {i, _} -> i
      _ -> 0
    end
  end

  defp to_float(v) when is_float(v), do: v
  defp to_float(v) when is_number(v), do: v * 1.0
  defp to_float(_), do: 0.0

  defp round3(v) when is_float(v), do: Float.round(v, 3)
  defp round3(v), do: v
end