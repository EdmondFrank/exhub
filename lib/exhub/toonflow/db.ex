defmodule Exhub.Toonflow.DB do
  @moduledoc """
  Low-level helpers over `Exqlite.Sqlite3`, shared by `Exhub.Toonflow.Store`
  (registry connection) and the project-scoped domain modules (`Novel`,
  `Events`, `Script`, `Memory`), which run against a project's `index.db`.

  Each function prepares, binds, steps and releases a statement, translating
  `Exqlite` return values into `:ok` / `{:ok, rows}` / `{:error, reason}`. The
  caller owns the connection lifecycle (`open/2`, `close/1`).
  """

  @type conn :: Exqlite.Sqlite3.db()

  @doc """
  Open the SQLite database at `path` (creating parent directories) and apply
  the `ddl` statements. On DDL failure the connection is closed.
  """
  @spec open(String.t(), [String.t()]) :: {:ok, conn()} | {:error, term()}
  def open(path, ddl \\ []) do
    with :ok <- ensure_dir(Path.dirname(path)),
         {:ok, conn} <- Exqlite.Sqlite3.open(path) do
      case run_ddl(conn, ddl) do
        :ok ->
          {:ok, conn}

        {:error, reason} ->
          close(conn)
          {:error, reason}
      end
    end
  end

  @doc "Close a connection (never raises)."
  @spec close(conn()) :: :ok
  def close(conn) do
    _ = Exqlite.Sqlite3.close(conn)
    :ok
  end

  @doc "Run a statement that yields no rows (INSERT/UPDATE/DELETE/DDL)."
  @spec execute(conn(), String.t(), [term()]) :: :ok | {:error, term()}
  def execute(conn, sql, params \\ []) do
    with {:ok, stmt} <- Exqlite.Sqlite3.prepare(conn, sql),
         :ok <- Exqlite.Sqlite3.bind(stmt, params) do
      result = Exqlite.Sqlite3.step(conn, stmt)
      _ = Exqlite.Sqlite3.release(conn, stmt)

      case result do
        :done -> :ok
        :ok -> :ok
        :busy -> :ok
        {:row, _} -> :ok
        {:error, _} = error -> error
      end
    end
  end

  @doc "Run a SELECT and collect every row."
  @spec query(conn(), String.t(), [term()]) :: {:ok, [[term()]]} | {:error, term()}
  def query(conn, sql, params \\ []) do
    with {:ok, stmt} <- Exqlite.Sqlite3.prepare(conn, sql),
         :ok <- Exqlite.Sqlite3.bind(stmt, params) do
      rows = collect(conn, stmt, [])
      _ = Exqlite.Sqlite3.release(conn, stmt)
      {:ok, rows}
    end
  end

  @doc "Run a SELECT and return the first row (or `nil`)."
  @spec query_one(conn(), String.t(), [term()]) :: {:ok, [term()] | nil} | {:error, term()}
  def query_one(conn, sql, params \\ []) do
    case query(conn, sql, params) do
      {:ok, [row | _]} -> {:ok, row}
      {:ok, []} -> {:ok, nil}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Run `fun` inside a transaction. `fun` must return `{:ok, value}` to commit or
  `{:error, reason}` to roll back; a raised exception also rolls back.
  """
  @spec transaction(conn(), (-> {:ok, term()} | {:error, term()})) ::
          {:ok, term()} | {:error, term()}
  def transaction(conn, fun) when is_function(fun, 0) do
    with :ok <- execute(conn, "BEGIN") do
      try do
        case fun.() do
          {:ok, value} ->
            with :ok <- execute(conn, "COMMIT"), do: {:ok, value}

          {:error, reason} ->
            _ = execute(conn, "ROLLBACK")
            {:error, reason}

          other ->
            _ = execute(conn, "ROLLBACK")
            {:error, {:unexpected_result, other}}
        end
      rescue
        e ->
          _ = execute(conn, "ROLLBACK")
          {:error, {:exception, Exception.message(e)}}
      end
    end
  end

  # --- internals ---

  defp run_ddl(conn, ddl) do
    Enum.reduce_while(ddl, :ok, fn sql, :ok ->
      case execute(conn, sql) do
        :ok -> {:cont, :ok}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp collect(conn, stmt, acc) do
    case Exqlite.Sqlite3.step(conn, stmt) do
      {:row, row} -> collect(conn, stmt, [row | acc])
      :busy -> collect(conn, stmt, acc)
      _ -> Enum.reverse(acc)
    end
  end

  defp ensure_dir(dir) do
    case File.mkdir_p(dir) do
      :ok -> :ok
      {:error, reason} -> {:error, reason}
    end
  end
end
