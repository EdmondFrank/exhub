defmodule Exhub.BlinkSearch.Backends.KeyValueStore do
  @moduledoc """
  Key Value Store backend — read/get/del/update key-value pairs.

  Uses SQLite via Exqlite for persistent storage. Supports commands:
  - `set key value` — insert or update
  - `del key` — delete
  - plain key — lookup, decrypt, and copy value to the kill ring

  Configuration (pushed from Emacs via `update`):
  - `db_path` — path to the SQLite database file (default: `~/.emacs.d/blink-search-kv.db`)
  - `table` — table name (default: `blink_search_kv`)

  ## Encryption at rest

  Values are encrypted with AES-256-GCM through `Exhub.Crypto.AesGcm` — the
  same scheme and master password (`SECRET_VAULT_PASSWORD`) used by the Emacs
  password vault — and stored as:

      enc:v1:<base64(iv | tag | ciphertext)>

  Legacy plaintext rows are encrypted in place the first time the database is
  opened while a master password is configured. A marker row in
  `blink_search_kv_meta` makes that migration a no-op on later opens. When no
  master password is configured, `set` refuses to store anything (it never
  falls back to plaintext) and pre-existing plaintext rows remain readable.

  Keys stay in plaintext because lookup uses SQL `LIKE` substring matching.

  Note: values are bound to the current `SECRET_VAULT_PASSWORD`. Changing that
  password makes previously stored values undecryptable (fetching such a key
  reports an error instead of copying ciphertext).
  """

  use Exhub.BlinkSearch.Backend

  require Logger

  alias Exhub.BlinkSearch.Backend
  alias Exhub.Crypto.AesGcm

  @set_regex ~r/^set\s(\S+)\s(.+)$/
  @del_regex ~r/^del\s(\S+)$/

  @default_db_path Path.join([System.user_home!(), ".emacs.d", "blink-search-kv.db"])
  @default_table "blink_search_kv"

  # Encrypted values are stored as `enc:v1:` <> Base64(iv | tag | ciphertext).
  # The prefix distinguishes them from legacy plaintext rows and makes the
  # in-place migration idempotent.
  @cipher_prefix "enc:v1:"

  # Per-table migration marker for plaintext -> encrypted conversion.
  @meta_table "blink_search_kv_meta"
  @encryption_version 1
  @migration_batch 100

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
    case encode_value(value) do
      {:ok, stored} ->
        store_value(conn, table, key, stored)

      {:error, reason} ->
        # Never leak the value; only the key and the failure reason.
        Exhub.send_message(
          ~s|(message "[Blink-Search] Cannot store key #{Backend.escape_message(key)}: #{Backend.escape_message(reason)}")|
        )
    end
  end

  defp store_value(conn, table, key, stored) do
    case lookup_value(conn, table, key) do
      {:ok, _} ->
        result = execute(conn, "UPDATE #{table} SET value = ? WHERE key = ?", [stored, key])
        report_store(result, "Updated", key)

      :error ->
        result = execute(conn, "INSERT INTO #{table} (key, value) VALUES (?, ?)", [key, stored])
        report_store(result, "Inserted", key)
    end
  end

  defp report_store(:ok, verb, key) do
    Exhub.send_message(
      ~s|(message "[Blink-Search] #{verb} key-value (#{Backend.escape_message(key)}) successfully")|
    )
  end

  defp report_store(_error, _verb, key) do
    Exhub.send_message(
      ~s|(message "[Blink-Search] Failed to store key #{Backend.escape_message(key)}")|
    )
  end

  defp handle_del(conn, table, key) do
    case execute(conn, "DELETE FROM #{table} WHERE key = ?", [key]) do
      :ok ->
        Exhub.send_message(
          ~s|(message "[Blink-Search] Deleted #{Backend.escape_message(key)} successfully")|
        )

      _error ->
        Exhub.send_message(
          ~s|(message "[Blink-Search] Failed to delete #{Backend.escape_message(key)}")|
        )
    end
  end

  defp handle_get(conn, table, key) do
    with {:ok, stored} <- lookup_value(conn, table, key),
         {:ok, value} <- decode_value(stored) do
      Exhub.send_message(~s|(kill-new #{Backend.elisp_quote(value)})|)

      Exhub.send_message(
        ~s|(message "[Blink-Search] Copied value for key: #{Backend.escape_message(key)}")|
      )
    else
      :error ->
        Exhub.send_message(
          ~s|(message "[Blink-Search] Key not found: #{Backend.escape_message(key)}")|
        )

      {:error, reason} ->
        Exhub.send_message(
          ~s|(message "[Blink-Search] Cannot decrypt key #{Backend.escape_message(key)}: #{Backend.escape_message(reason)}")|
        )
    end
  end

  # ── Encryption envelope ─────────────────────────────────────────────

  defp encode_value(value) do
    case AesGcm.encrypt(value) do
      {:ok, encoded} -> {:ok, @cipher_prefix <> encoded}
      {:error, _reason} = error -> error
    end
  end

  # Legacy plaintext row: written before encryption was introduced, or the
  # migration has not run yet because no master password is configured.
  defp decode_value(@cipher_prefix <> encoded), do: AesGcm.decrypt(encoded)
  defp decode_value(value), do: {:ok, value}

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
    with :ok <-
           execute(
             conn,
             "CREATE TABLE IF NOT EXISTS #{table} (key TEXT PRIMARY KEY, value TEXT NOT NULL)",
             []
           ),
         :ok <- ensure_meta_table(conn) do
      # A failed migration must not break search or lookups: legacy rows stay
      # readable as plaintext and the next open retries.
      _ = migrate_encryption(conn, table)
      :ok
    end
  end

  defp ensure_meta_table(conn) do
    execute(
      conn,
      "CREATE TABLE IF NOT EXISTS #{@meta_table} (table_name TEXT PRIMARY KEY, encryption_version INTEGER NOT NULL)",
      []
    )
  end

  # ── Plaintext -> encrypted migration ────────────────────────────────

  defp migrate_encryption(conn, table) do
    cond do
      not AesGcm.configured?() -> :ok
      migration_current?(conn, table) -> :ok
      true -> run_migration(conn, table)
    end
  end

  defp migration_current?(conn, table) do
    case query_rows(
           conn,
           "SELECT encryption_version FROM #{@meta_table} WHERE table_name = ?",
           [table]
         ) do
      {:ok, [[version]]} -> to_string(version) == to_string(@encryption_version)
      _ -> false
    end
  end

  defp run_migration(conn, table) do
    case Exqlite.Sqlite3.execute(conn, "BEGIN IMMEDIATE") do
      :ok ->
        case migrate_batch(conn, table, 0) do
          {:ok, count} -> commit_migration(conn, table, count)
          error -> rollback_migration(conn, error)
        end

      error ->
        error
    end
  end

  defp commit_migration(conn, table, count) do
    with :ok <- mark_migrated(conn, table),
         :ok <- Exqlite.Sqlite3.execute(conn, "COMMIT") do
      if count > 0 do
        Logger.info("[Blink-Search] Encrypted #{count} legacy key-value row(s) in #{table}")
      end

      :ok
    else
      error -> rollback_migration(conn, error)
    end
  end

  defp rollback_migration(conn, error) do
    _ = Exqlite.Sqlite3.execute(conn, "ROLLBACK")
    # Never log row values — only the failure reason.
    Logger.warning(
      "[Blink-Search] Key-value encryption migration failed; will retry on next open: #{inspect(error)}"
    )

    error
  end

  # Encrypt every remaining plaintext row, in batches, so a large table never
  # has to be materialized in memory. NULL values (allowed by legacy schemas)
  # are skipped — there is nothing to protect.
  defp migrate_batch(conn, table, count) do
    case query_rows(
           conn,
           "SELECT key, CAST(value AS TEXT) FROM #{table} " <>
             "WHERE value IS NOT NULL AND CAST(value AS TEXT) NOT LIKE ? LIMIT #{@migration_batch}",
           [@cipher_prefix <> "%"]
         ) do
      {:ok, []} ->
        {:ok, count}

      {:ok, rows} ->
        case encrypt_rows(conn, table, rows) do
          :ok -> migrate_batch(conn, table, count + length(rows))
          error -> error
        end

      error ->
        error
    end
  end

  defp encrypt_rows(conn, table, rows) do
    Enum.reduce_while(rows, :ok, fn [key, value], :ok ->
      with {:ok, stored} <- encode_value(value),
           :ok <- execute(conn, "UPDATE #{table} SET value = ? WHERE key = ?", [stored, key]) do
        {:cont, :ok}
      else
        error -> {:halt, error}
      end
    end)
  end

  defp mark_migrated(conn, table) do
    execute(
      conn,
      "INSERT OR REPLACE INTO #{@meta_table} (table_name, encryption_version) VALUES (?, ?)",
      [table, @encryption_version]
    )
  end

  # ── Queries ─────────────────────────────────────────────────────────

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

  defp lookup_value(conn, table, key) do
    # CAST(value AS TEXT): see query_keys/3 — numeric-affinity columns may
    # hold INTEGER values that must reach Elixir as strings.
    case query_rows(conn, "SELECT CAST(value AS TEXT) FROM #{table} WHERE key = ?", [key]) do
      {:ok, [[value]]} when is_binary(value) -> {:ok, value}
      _ -> :error
    end
  end

  defp execute(conn, sql, params) do
    with {:ok, stmt} <- Exqlite.Sqlite3.prepare(conn, sql),
         :ok <- Exqlite.Sqlite3.bind(stmt, params) do
      result =
        case Exqlite.Sqlite3.step(conn, stmt) do
          :done -> :ok
          :busy -> {:error, :busy}
          {:row, _row} -> {:error, :unexpected_row}
          {:error, _reason} = error -> error
        end

      _ = Exqlite.Sqlite3.release(conn, stmt)
      result
    end
  end

  defp query_rows(conn, sql, params) do
    with {:ok, stmt} <- Exqlite.Sqlite3.prepare(conn, sql),
         :ok <- Exqlite.Sqlite3.bind(stmt, params) do
      result = collect_rows(conn, stmt, [])
      :ok = Exqlite.Sqlite3.release(conn, stmt)
      result
    end
  end

  defp collect_rows(conn, stmt, acc) do
    case Exqlite.Sqlite3.step(conn, stmt) do
      {:row, row} -> collect_rows(conn, stmt, [row | acc])
      :done -> {:ok, Enum.reverse(acc)}
      :busy -> {:error, :busy}
      {:error, _reason} = error -> error
    end
  end
end
