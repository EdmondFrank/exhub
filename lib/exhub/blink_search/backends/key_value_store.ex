defmodule Exhub.BlinkSearch.Backends.KeyValueStore do
  @moduledoc """
  Key Value Store backend — read/get/del/update key-value pairs.

  Uses SQLite via Exqlite for persistent storage. Supports commands:
  - `set key value` — insert or update
  - `del key` — delete
  - plain key — lookup and copy value to the kill ring

  Configuration (pushed from Emacs via `update`):
  - `db_path` — path to the SQLite database file (default: `~/.emacs.d/blink-search-kv.db`)
  - `table` — table name (default: `blink_search_kv`)
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @set_regex ~r/^set\s(\S+)\s(.+)$/
  @del_regex ~r/^del\s(\S+)$/

  @default_db_path Path.join([System.user_home!(), ".emacs.d", "blink-search-kv.db"])
  @default_table "blink_search_kv"

  @impl true
  def search_match(prefix, state) do
    db_path = Map.get(state, :db_path, @default_db_path)
    table = safe_table_name(Map.get(state, :table, @default_table))

    with {:ok, conn} <- Exqlite.Sqlite3.open(db_path),
         :ok <- ensure_schema(conn, table),
         {:ok, rows} <- query_keys(conn, table, prefix),
         :ok <- Exqlite.Sqlite3.close(conn) do
      extra =
        cond do
          Regex.match?(@set_regex, prefix) -> [prefix]
          Regex.match?(@del_regex, prefix) -> [prefix]
          true -> []
        end

      rows ++ extra
    else
      _ -> []
    end
  end

  @impl true
  def do_action(candidate, state) do
    text = Backend.candidate_text(candidate)
    db_path = Map.get(state, :db_path, @default_db_path)
    table = safe_table_name(Map.get(state, :table, @default_table))

    with {:ok, conn} <- Exqlite.Sqlite3.open(db_path),
         :ok <- ensure_schema(conn, table) do
      do_operation(conn, table, text)
      Exqlite.Sqlite3.close(conn)
    end

    :ok
  end

  @impl true
  def update(config, state) when is_list(config) do
    case config do
      [db_path, table] when is_binary(db_path) and is_binary(table) ->
        state
        |> Map.put(:db_path, db_path)
        |> Map.put(:table, table)

      _ ->
        state
    end
  end

  # ── Operation dispatch ──────────────────────────────────────────────

  defp do_operation(conn, table, text) do
    cond do
      match = Regex.run(@set_regex, text) ->
        [_, key, value] = match
        handle_set(conn, table, key, value)

      match = Regex.run(@del_regex, text) ->
        [_, key] = match
        handle_del(conn, table, key)

      true ->
        handle_get(conn, table, text)
    end
  end

  defp handle_set(conn, table, key, value) do
    case lookup_key(conn, table, key) do
      {:ok, _} ->
        execute(conn, "UPDATE #{table} SET value = ? WHERE key = ?", [value, key])

        Exhub.send_message(
          ~s|(message "[Blink-Search] Updated key-value (#{Backend.escape_message(key)}, #{Backend.escape_message(value)}) successfully")|
        )

      _ ->
        execute(conn, "INSERT INTO #{table} (key, value) VALUES (?, ?)", [key, value])

        Exhub.send_message(
          ~s|(message "[Blink-Search] Inserted key-value (#{Backend.escape_message(key)}, #{Backend.escape_message(value)}) successfully")|
        )
    end
  end

  defp handle_del(conn, table, key) do
    execute(conn, "DELETE FROM #{table} WHERE key = ?", [key])

    Exhub.send_message(
      ~s|(message "[Blink-Search] Deleted #{Backend.escape_message(key)} successfully")|
    )
  end

  defp handle_get(conn, table, key) do
    case lookup_key(conn, table, key) do
      {:ok, value} ->
        Exhub.send_message(~s|(kill-new #{Backend.elisp_quote(value)})|)

        Exhub.send_message(
          ~s|(message "[Blink-Search] Copied value for key: #{Backend.escape_message(key)}")|
        )

      _ ->
        Exhub.send_message(
          ~s|(message "[Blink-Search] Key not found: #{Backend.escape_message(key)}")|
        )
    end
  end

  # ── SQLite helpers ──────────────────────────────────────────────────

  defp safe_table_name(name) when is_binary(name) do
    if String.match?(name, ~r/^[a-zA-Z_][a-zA-Z0-9_]*$/) do
      name
    else
      @default_table
    end
  end

  defp safe_table_name(_), do: @default_table

  defp ensure_schema(conn, table) do
    execute(
      conn,
      "CREATE TABLE IF NOT EXISTS #{table} (key TEXT PRIMARY KEY, value TEXT NOT NULL)",
      []
    )
  end

  defp query_keys(conn, table, prefix) do
    # Substring match (mirrors the legacy Python backend's `LIKE '%prefix%'`)
    # so keys with the term mid-string are still found.
    #
    # CAST(key AS TEXT): legacy tables declare `"key" string`, which SQLite
    # treats as NUMERIC affinity — numeric-looking keys (e.g. 2210506045) are
    # stored as INTEGER and would otherwise come back as Elixir integers,
    # crashing downstream elisp rendering.
    case query_rows(
           conn,
           "SELECT CAST(key AS TEXT) FROM #{table} WHERE key LIKE '%' || ? || '%' LIMIT 20",
           [prefix]
         ) do
      {:ok, rows} -> {:ok, Enum.map(rows, &List.first/1)}
      error -> error
    end
  end

  defp lookup_key(conn, table, key) do
    # CAST(value AS TEXT): see query_keys/3 — numeric-affinity columns may
    # hold INTEGER values that must reach elisp as strings.
    case query_rows(conn, "SELECT CAST(value AS TEXT) FROM #{table} WHERE key = ?", [key]) do
      {:ok, [[value]]} -> {:ok, value}
      _ -> :error
    end
  end

  defp execute(conn, sql, params) do
    with {:ok, stmt} <- Exqlite.Sqlite3.prepare(conn, sql),
         :ok <- Exqlite.Sqlite3.bind(stmt, params),
         _ <- Exqlite.Sqlite3.step(conn, stmt),
         :ok <- Exqlite.Sqlite3.release(conn, stmt) do
      :ok
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
end
