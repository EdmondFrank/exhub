defmodule Exhub.Sagents.Store do
  @moduledoc """
  SQLite storage for Agent Hub conversations.

  Owns a single `Exqlite` connection to `~/.config/exhub/agent_hub/hub.db`
  (override with the `:path` option or `config :exhub, :sagents,
  db_path: "..."`) and serializes every statement through one GenServer —
  the same pattern as `Exhub.Toonflow.Store` and
  `Exhub.MCP.Brain.RAG.VectorIndex`. SQLite connections are not safe to share
  between processes, so all access funnels through this server.

  Three tables:

    * `conversations` — one row per chat thread, keyed by a generated id.
      `last_message_at` drives "recent first" ordering, matching the
      `last_message_at` column sagents 0.21 generates for Postgres.
    * `display_messages` — UI-facing transcript rows (text, tool_call,
      tool_result, thinking). `id` is monotonic, so ordering by it reproduces
      insertion order even when several rows of one message share a timestamp.
    * `agent_states` — the serialized `Sagents.State` envelope written by
      `Sagents.AgentPersistence`.

  Timestamps are ISO-8601 UTC strings: they sort lexicographically, compare
  correctly in SQL, and need no conversion to render.
  """

  use GenServer
  require Logger

  @default_path Path.join(System.user_home!(), ".config/exhub/agent_hub/hub.db")
  @call_timeout 15_000

  @ddl [
    """
    CREATE TABLE IF NOT EXISTS conversations (
      id TEXT PRIMARY KEY,
      agent_name TEXT NOT NULL,
      title TEXT,
      metadata TEXT NOT NULL DEFAULT '{}',
      status TEXT NOT NULL DEFAULT 'active',
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL,
      last_message_at TEXT
    )
    """,
    """
    CREATE INDEX IF NOT EXISTS conversations_agent_idx
      ON conversations(agent_name, last_message_at DESC)
    """,
    """
    CREATE INDEX IF NOT EXISTS conversations_recent_idx
      ON conversations(last_message_at DESC)
    """,
    """
    CREATE TABLE IF NOT EXISTS display_messages (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      conversation_id TEXT NOT NULL,
      message_type TEXT NOT NULL,
      content TEXT NOT NULL,
      content_type TEXT NOT NULL DEFAULT 'text',
      status TEXT,
      call_id TEXT,
      sequence INTEGER NOT NULL DEFAULT 0,
      inserted_at TEXT NOT NULL,
      FOREIGN KEY (conversation_id) REFERENCES conversations(id) ON DELETE CASCADE
    )
    """,
    """
    CREATE INDEX IF NOT EXISTS display_messages_conversation_idx
      ON display_messages(conversation_id, id)
    """,
    """
    CREATE UNIQUE INDEX IF NOT EXISTS display_messages_tool_call_idx
      ON display_messages(conversation_id, call_id)
      WHERE content_type = 'tool_call'
    """,
    """
    CREATE TABLE IF NOT EXISTS agent_states (
      conversation_id TEXT PRIMARY KEY,
      state_data TEXT NOT NULL,
      version INTEGER NOT NULL DEFAULT 1,
      updated_at TEXT NOT NULL,
      FOREIGN KEY (conversation_id) REFERENCES conversations(id) ON DELETE CASCADE
    )
    """
  ]

  @type conversation :: %{
          id: String.t(),
          agent_name: String.t(),
          title: String.t() | nil,
          metadata: map(),
          status: String.t(),
          created_at: String.t(),
          updated_at: String.t(),
          last_message_at: String.t() | nil
        }

  @type message :: %{
          id: integer(),
          conversation_id: String.t(),
          message_type: String.t(),
          content: map(),
          content_type: String.t(),
          status: String.t() | nil,
          call_id: String.t() | nil,
          sequence: integer(),
          inserted_at: String.t()
        }

  # --- Client API ---

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc "Create a conversation for `agent_name` and return it."
  @spec create_conversation(String.t(), keyword(), GenServer.server()) ::
          {:ok, conversation()} | {:error, term()}
  def create_conversation(agent_name, opts \\ [], server \\ __MODULE__) do
    GenServer.call(server, {:create_conversation, agent_name, opts}, @call_timeout)
  end

  @doc """
  List conversations, most recently active first.

  Options: `:agent_name` (filter), `:limit` (default 200).
  """
  @spec list_conversations(keyword(), GenServer.server()) ::
          {:ok, [conversation()]} | {:error, term()}
  def list_conversations(opts \\ [], server \\ __MODULE__) do
    GenServer.call(server, {:list_conversations, opts}, @call_timeout)
  end

  @spec get_conversation(String.t(), GenServer.server()) ::
          {:ok, conversation()} | {:error, :not_found | term()}
  def get_conversation(id, server \\ __MODULE__) do
    GenServer.call(server, {:get_conversation, id}, @call_timeout)
  end

  @doc "Update `:title`, `:metadata`, or `:status` on a conversation."
  @spec update_conversation(String.t(), map(), GenServer.server()) ::
          {:ok, conversation()} | {:error, term()}
  def update_conversation(id, attrs, server \\ __MODULE__) do
    GenServer.call(server, {:update_conversation, id, attrs}, @call_timeout)
  end

  @doc "Delete a conversation and (via cascade) its messages and state."
  @spec delete_conversation(String.t(), GenServer.server()) :: :ok | {:error, term()}
  def delete_conversation(id, server \\ __MODULE__) do
    GenServer.call(server, {:delete_conversation, id}, @call_timeout)
  end

  @spec list_messages(String.t(), GenServer.server()) ::
          {:ok, [message()]} | {:error, term()}
  def list_messages(conversation_id, server \\ __MODULE__) do
    GenServer.call(server, {:list_messages, conversation_id}, @call_timeout)
  end

  @doc """
  Append display messages for one conversation, in order.

  Each attribute map accepts `:message_type`, `:content` (map),
  `:content_type`, `:status`, `:call_id`, `:sequence`. The conversation's
  `last_message_at` moves to the newest row's timestamp.
  """
  @spec append_messages(String.t(), [map()], GenServer.server()) ::
          {:ok, [message()]} | {:error, term()}
  def append_messages(conversation_id, attrs_list, server \\ __MODULE__) do
    GenServer.call(server, {:append_messages, conversation_id, attrs_list}, @call_timeout)
  end

  @doc "Move a tool call's status and merge `extra` into its content."
  @spec update_tool_status(String.t(), String.t(), String.t(), map(), GenServer.server()) ::
          {:ok, message()} | {:error, :not_found | term()}
  def update_tool_status(conversation_id, call_id, status, extra \\ %{}, server \\ __MODULE__) do
    GenServer.call(
      server,
      {:update_tool_status, conversation_id, call_id, status, extra},
      @call_timeout
    )
  end

  @doc "Replace the content of an interrupted tool result row."
  @spec resolve_tool_result(String.t(), String.t(), String.t(), GenServer.server()) ::
          {:ok, message()} | {:error, :not_found | term()}
  def resolve_tool_result(conversation_id, call_id, result_content, server \\ __MODULE__) do
    GenServer.call(
      server,
      {:resolve_tool_result, conversation_id, call_id, result_content},
      @call_timeout
    )
  end

  @spec save_state(String.t(), map(), GenServer.server()) :: :ok | {:error, term()}
  def save_state(conversation_id, state_data, server \\ __MODULE__) do
    GenServer.call(server, {:save_state, conversation_id, state_data}, @call_timeout)
  end

  @spec load_state(String.t(), GenServer.server()) :: {:ok, map()} | {:error, :not_found | term()}
  def load_state(conversation_id, server \\ __MODULE__) do
    GenServer.call(server, {:load_state, conversation_id}, @call_timeout)
  end

  @spec delete_state(String.t(), GenServer.server()) :: :ok | {:error, term()}
  def delete_state(conversation_id, server \\ __MODULE__) do
    GenServer.call(server, {:delete_state, conversation_id}, @call_timeout)
  end

  @doc "Delete every display message for a conversation."
  @spec clear_messages(String.t(), GenServer.server()) :: :ok | {:error, term()}
  def clear_messages(conversation_id, server \\ __MODULE__) do
    GenServer.call(server, {:clear_messages, conversation_id}, @call_timeout)
  end

  @doc "Timestamp used for new rows."
  @spec now() :: String.t()
  def now, do: DateTime.utc_now() |> DateTime.to_iso8601()

  # --- Server callbacks ---

  @impl true
  def init(opts) do
    path = Keyword.get(opts, :path) || configured_path()

    case open(path) do
      {:ok, conn} ->
        {:ok, %{conn: conn, path: Path.expand(path)}}

      {:error, reason} ->
        Logger.error("[Sagents.Store] unavailable at #{path}: #{inspect(reason)}")
        {:ok, %{conn: nil, path: Path.expand(path)}}
    end
  end

  @impl true
  def terminate(_reason, %{conn: nil}), do: :ok

  def terminate(_reason, %{conn: conn}) do
    _ = Exqlite.Sqlite3.close(conn)
    :ok
  end

  @impl true
  def handle_call({:create_conversation, _agent_name, _opts}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:create_conversation, agent_name, opts}, _from, state) do
    id = Keyword.get(opts, :id) || new_id()
    now = now()
    title = Keyword.get(opts, :title)
    metadata = Keyword.get(opts, :metadata, %{})
    status = Keyword.get(opts, :status, "active")

    sql = """
    INSERT INTO conversations (id, agent_name, title, metadata, status, created_at, updated_at)
    VALUES (?, ?, ?, ?, ?, ?, ?)
    """

    with :ok <-
           execute(state.conn, sql, [id, agent_name, title, encode(metadata), status, now, now]),
         {:ok, conversation} <- fetch_conversation(state.conn, id) do
      {:reply, {:ok, conversation}, state}
    else
      {:error, reason} ->
        Logger.error("[Sagents.Store] create_conversation failed: #{inspect(reason)}")
        {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:list_conversations, _opts}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:list_conversations, opts}, _from, state) do
    limit = Keyword.get(opts, :limit, 200)

    {sql, params} =
      case Keyword.get(opts, :agent_name) do
        nil ->
          {"""
           SELECT #{conversation_columns()} FROM conversations
           ORDER BY COALESCE(last_message_at, created_at) DESC, rowid DESC
           LIMIT ?
           """, [limit]}

        agent_name ->
          {"""
           SELECT #{conversation_columns()} FROM conversations
           WHERE agent_name = ?
           ORDER BY COALESCE(last_message_at, created_at) DESC, rowid DESC
           LIMIT ?
           """, [agent_name, limit]}
      end

    case query(state.conn, sql, params) do
      {:ok, rows} -> {:reply, {:ok, Enum.map(rows, &to_conversation/1)}, state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:get_conversation, _id}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:get_conversation, id}, _from, state) do
    {:reply, fetch_conversation(state.conn, id), state}
  end

  def handle_call({:update_conversation, _id, _attrs}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:update_conversation, id, attrs}, _from, state) do
    fields =
      [
        {"title", fetch(attrs, :title)},
        {"metadata", fetch(attrs, :metadata)},
        {"status", fetch(attrs, :status)}
      ]
      |> Enum.reject(fn {_column, value} -> is_nil(value) end)
      |> Enum.map(fn
        {"metadata", value} -> {"metadata", encode(value)}
        {column, value} -> {column, value}
      end)

    if fields == [] do
      {:reply, fetch_conversation(state.conn, id), state}
    else
      assignments = Enum.map_join(fields, ", ", fn {column, _} -> "#{column} = ?" end)
      params = Enum.map(fields, fn {_, value} -> value end)

      sql = "UPDATE conversations SET #{assignments}, updated_at = ? WHERE id = ?"

      with :ok <- execute(state.conn, sql, params ++ [now(), id]),
           {:ok, conversation} <- fetch_conversation(state.conn, id) do
        {:reply, {:ok, conversation}, state}
      else
        {:error, reason} -> {:reply, {:error, reason}, state}
      end
    end
  end

  def handle_call({:delete_conversation, _id}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:delete_conversation, id}, _from, state) do
    # Foreign keys cascade only when enabled on this connection; delete
    # children explicitly so the behaviour does not depend on the pragma.
    result =
      with :ok <-
             execute(state.conn, "DELETE FROM display_messages WHERE conversation_id = ?", [id]),
           :ok <- execute(state.conn, "DELETE FROM agent_states WHERE conversation_id = ?", [id]),
           :ok <- execute(state.conn, "DELETE FROM conversations WHERE id = ?", [id]) do
        :ok
      end

    {:reply, result, state}
  end

  def handle_call({:list_messages, _conversation_id}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:list_messages, conversation_id}, _from, state) do
    sql = """
    SELECT #{message_columns()} FROM display_messages
    WHERE conversation_id = ?
    ORDER BY id ASC
    """

    case query(state.conn, sql, [conversation_id]) do
      {:ok, rows} -> {:reply, {:ok, Enum.map(rows, &to_message/1)}, state}
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  def handle_call({:append_messages, _conversation_id, _attrs}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:append_messages, conversation_id, attrs_list}, _from, state) do
    inserted =
      transaction(state.conn, fn ->
        results =
          Enum.reduce_while(attrs_list, {:ok, []}, fn attrs, {:ok, acc} ->
            case insert_message(state.conn, conversation_id, attrs) do
              {:ok, message} -> {:cont, {:ok, [message | acc]}}
              {:error, reason} -> {:halt, {:error, reason}}
            end
          end)

        with {:ok, messages} <- results,
             :ok <- touch_conversation(state.conn, conversation_id) do
          {:ok, Enum.reverse(messages)}
        end
      end)

    {:reply, inserted, state}
  end

  def handle_call(
        {:update_tool_status, _conversation_id, _call_id, _status, _extra},
        _from,
        %{conn: nil} = state
      ) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:update_tool_status, conversation_id, call_id, status, extra}, _from, state) do
    sql = """
    SELECT #{message_columns()} FROM display_messages
    WHERE conversation_id = ? AND content_type = 'tool_call' AND call_id = ?
    LIMIT 1
    """

    result =
      with {:ok, rows} <- query(state.conn, sql, [conversation_id, call_id]),
           [row | _] <- rows,
           message <- to_message(row),
           content <- Map.merge(message.content, stringify(extra)),
           :ok <-
             execute(
               state.conn,
               "UPDATE display_messages SET status = ?, content = ? WHERE id = ?",
               [status, encode(content), message.id]
             ) do
        {:ok, %{message | status: status, content: content}}
      else
        [] -> {:error, :not_found}
        {:error, reason} -> {:error, reason}
      end

    {:reply, result, state}
  end

  def handle_call(
        {:resolve_tool_result, _conversation_id, _call_id, _content},
        _from,
        %{conn: nil} = state
      ) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:resolve_tool_result, conversation_id, call_id, result_content}, _from, state) do
    sql = """
    SELECT #{message_columns()} FROM display_messages
    WHERE conversation_id = ? AND content_type = 'tool_result' AND call_id = ?
    LIMIT 1
    """

    result =
      with {:ok, rows} <- query(state.conn, sql, [conversation_id, call_id]),
           [row | _] <- rows,
           message <- to_message(row),
           content <-
             message.content
             |> Map.put("content", result_content)
             |> Map.put("is_interrupt", false),
           :ok <-
             execute(state.conn, "UPDATE display_messages SET content = ? WHERE id = ?", [
               encode(content),
               message.id
             ]) do
        {:ok, %{message | content: content}}
      else
        [] -> {:error, :not_found}
        {:error, reason} -> {:error, reason}
      end

    {:reply, result, state}
  end

  def handle_call({:save_state, _conversation_id, _data}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:save_state, conversation_id, state_data}, _from, state) do
    sql = """
    INSERT INTO agent_states (conversation_id, state_data, version, updated_at)
    VALUES (?, ?, 1, ?)
    ON CONFLICT(conversation_id) DO UPDATE SET
      state_data = excluded.state_data,
      version = agent_states.version + 1,
      updated_at = excluded.updated_at
    """

    result = execute(state.conn, sql, [conversation_id, encode(state_data), now()])

    if result == :ok do
      # Keep "last activity" honest even for state-only writes.
      _ = touch_conversation(state.conn, conversation_id)
    end

    {:reply, result, state}
  end

  def handle_call({:load_state, _conversation_id}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:load_state, conversation_id}, _from, state) do
    sql = "SELECT state_data FROM agent_states WHERE conversation_id = ?"

    result =
      case query_one(state.conn, sql, [conversation_id]) do
        {:ok, [json]} -> {:ok, decode(json, %{})}
        {:ok, nil} -> {:error, :not_found}
        {:error, reason} -> {:error, reason}
      end

    {:reply, result, state}
  end

  def handle_call({:delete_state, _conversation_id}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:delete_state, conversation_id}, _from, state) do
    sql = "DELETE FROM agent_states WHERE conversation_id = ?"
    {:reply, execute(state.conn, sql, [conversation_id]), state}
  end

  def handle_call({:clear_messages, _conversation_id}, _from, %{conn: nil} = state) do
    {:reply, {:error, :store_unavailable}, state}
  end

  def handle_call({:clear_messages, conversation_id}, _from, state) do
    sql = "DELETE FROM display_messages WHERE conversation_id = ?"
    {:reply, execute(state.conn, sql, [conversation_id]), state}
  end

  # --- SQLite plumbing ---

  defp configured_path do
    case Application.get_env(:exhub, :sagents, []) do
      config when is_list(config) -> Keyword.get(config, :db_path, @default_path)
      %{} = config -> Map.get(config, "db_path") || Map.get(config, :db_path) || @default_path
      _other -> @default_path
    end
  end

  defp open(path) do
    expanded = Path.expand(path)

    with :ok <- File.mkdir_p(Path.dirname(expanded)),
         {:ok, conn} <- Exqlite.Sqlite3.open(expanded),
         :ok <- execute(conn, "PRAGMA journal_mode = WAL"),
         :ok <- execute(conn, "PRAGMA foreign_keys = ON"),
         :ok <- run_ddl(conn, @ddl) do
      {:ok, conn}
    else
      {:error, reason} -> {:error, reason}
    end
  end

  defp run_ddl(conn, statements) do
    Enum.reduce_while(statements, :ok, fn sql, :ok ->
      case execute(conn, sql) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp execute(conn, sql, params \\ []) do
    with {:ok, stmt} <- Exqlite.Sqlite3.prepare(conn, sql),
         :ok <- Exqlite.Sqlite3.bind(stmt, params) do
      result = Exqlite.Sqlite3.step(conn, stmt)
      _ = Exqlite.Sqlite3.release(conn, stmt)

      case result do
        :done -> :ok
        :ok -> :ok
        :busy -> :ok
        {:row, _} -> :ok
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp query(conn, sql, params) do
    with {:ok, stmt} <- Exqlite.Sqlite3.prepare(conn, sql),
         :ok <- Exqlite.Sqlite3.bind(stmt, params) do
      result = collect(conn, stmt, [])
      _ = Exqlite.Sqlite3.release(conn, stmt)
      result
    end
  end

  defp query_one(conn, sql, params) do
    case query(conn, sql, params) do
      {:ok, [row | _]} -> {:ok, row}
      {:ok, []} -> {:ok, nil}
      {:error, reason} -> {:error, reason}
    end
  end

  defp collect(conn, stmt, acc) do
    case Exqlite.Sqlite3.step(conn, stmt) do
      {:row, row} -> collect(conn, stmt, [row | acc])
      :done -> {:ok, Enum.reverse(acc)}
      :busy -> collect(conn, stmt, acc)
      {:error, reason} -> {:error, reason}
    end
  end

  defp transaction(conn, fun) do
    case execute(conn, "BEGIN") do
      :ok ->
        try do
          case fun.() do
            {:ok, value} ->
              with :ok <- execute(conn, "COMMIT"), do: {:ok, value}

            {:error, reason} ->
              _ = execute(conn, "ROLLBACK")
              {:error, reason}
          end
        rescue
          error ->
            _ = execute(conn, "ROLLBACK")
            {:error, Exception.message(error)}
        catch
          :throw, reason ->
            _ = execute(conn, "ROLLBACK")
            {:error, reason}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp insert_message(conn, conversation_id, attrs) do
    sql = """
    INSERT INTO display_messages
      (conversation_id, message_type, content, content_type, status, call_id, sequence, inserted_at)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    RETURNING #{message_columns()}
    """

    params = [
      conversation_id,
      fetch(attrs, :message_type) || "assistant",
      encode(fetch(attrs, :content) || %{}),
      fetch(attrs, :content_type) || "text",
      fetch(attrs, :status),
      fetch(attrs, :call_id),
      fetch(attrs, :sequence) || 0,
      fetch(attrs, :inserted_at) || now()
    ]

    case query(conn, sql, params) do
      {:ok, [row | _]} -> {:ok, to_message(row)}
      {:ok, []} -> {:error, :insert_failed}
      {:error, reason} -> {:error, reason}
    end
  end

  defp touch_conversation(conn, conversation_id) do
    timestamp = now()

    sql = """
    UPDATE conversations
    SET last_message_at = ?, updated_at = ?
    WHERE id = ? AND (last_message_at IS NULL OR last_message_at < ?)
    """

    with :ok <- execute(conn, sql, [timestamp, timestamp, conversation_id, timestamp]) do
      execute(conn, "UPDATE conversations SET updated_at = ? WHERE id = ?", [
        timestamp,
        conversation_id
      ])
    end
  end

  defp fetch_conversation(conn, id) do
    sql = "SELECT #{conversation_columns()} FROM conversations WHERE id = ?"

    case query_one(conn, sql, [id]) do
      {:ok, nil} -> {:error, :not_found}
      {:ok, row} -> {:ok, to_conversation(row)}
      {:error, reason} -> {:error, reason}
    end
  end

  defp conversation_columns do
    "id, agent_name, title, metadata, status, created_at, updated_at, last_message_at"
  end

  defp message_columns do
    "id, conversation_id, message_type, content, content_type, status, call_id, sequence, inserted_at"
  end

  defp to_conversation([id, agent_name, title, metadata, status, created_at, updated_at, last]) do
    %{
      id: id,
      agent_name: agent_name,
      title: title,
      metadata: decode(metadata, %{}),
      status: status,
      created_at: created_at,
      updated_at: updated_at,
      last_message_at: last
    }
  end

  defp to_message([
         id,
         conversation_id,
         message_type,
         content,
         content_type,
         status,
         call_id,
         sequence,
         inserted_at
       ]) do
    %{
      id: id,
      conversation_id: conversation_id,
      message_type: message_type,
      content: decode(content, %{}),
      content_type: content_type,
      status: status,
      call_id: call_id,
      sequence: sequence,
      inserted_at: inserted_at
    }
  end

  defp fetch(attrs, key) do
    cond do
      is_map_key(attrs, key) -> Map.get(attrs, key)
      is_map_key(attrs, to_string(key)) -> Map.get(attrs, to_string(key))
      true -> nil
    end
  end

  defp stringify(map) when is_map(map) do
    Map.new(map, fn {key, value} -> {to_string(key), value} end)
  end

  defp encode(map), do: Jason.encode!(map)

  defp decode(nil, default), do: default
  defp decode("", default), do: default

  defp decode(json, default) when is_binary(json) do
    case Jason.decode(json) do
      {:ok, value} -> value
      {:error, _} -> default
    end
  end

  defp decode(_other, default), do: default

  defp new_id do
    "conv_" <> Base.url_encode64(:crypto.strong_rand_bytes(12), padding: false)
  end
end
