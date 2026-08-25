defmodule Exhub.BlinkSearch.Backends.KeyValueStoreTest do
  use ExUnit.Case, async: true

  alias Exhub.BlinkSearch.Backends.KeyValueStore

  # Exhub.send_message/1 dispatches over Exhub.Registry; under --no-start we
  # start a bare registry so dispatch is a no-op instead of raising.
  setup do
    start_supervised!({Registry, keys: :unique, name: Exhub.Registry})
    :ok
  end

  # ── Helpers ──────────────────────────────────────────────────────────

  defp tmp_db_path do
    Path.join(System.tmp_dir!(), "blink_search_kv_test_#{System.unique_integer([:positive])}.db")
  end

  defp default_state do
    %{db_path: tmp_db_path(), table: "test_kv"}
  end

  defp create_table(conn, table) do
    {:ok, stmt} =
      Exqlite.Sqlite3.prepare(
        conn,
        "CREATE TABLE IF NOT EXISTS #{table} (key TEXT PRIMARY KEY, value TEXT NOT NULL)"
      )

    :ok = Exqlite.Sqlite3.bind(stmt, [])
    _ = Exqlite.Sqlite3.step(conn, stmt)
    :ok = Exqlite.Sqlite3.release(conn, stmt)
  end

  defp insert_key(conn, table, key, value) do
    {:ok, stmt} = Exqlite.Sqlite3.prepare(conn, "INSERT INTO #{table} (key, value) VALUES (?, ?)")
    :ok = Exqlite.Sqlite3.bind(stmt, [key, value])
    _ = Exqlite.Sqlite3.step(conn, stmt)
    :ok = Exqlite.Sqlite3.release(conn, stmt)
  end

  defp read_value(conn, table, key) do
    {:ok, stmt} = Exqlite.Sqlite3.prepare(conn, "SELECT value FROM #{table} WHERE key = ?")
    :ok = Exqlite.Sqlite3.bind(stmt, [key])
    {:row, [value]} = Exqlite.Sqlite3.step(conn, stmt)
    :done = Exqlite.Sqlite3.step(conn, stmt)
    :ok = Exqlite.Sqlite3.release(conn, stmt)
    value
  end

  defp key_count(conn, table, key) do
    {:ok, stmt} = Exqlite.Sqlite3.prepare(conn, "SELECT COUNT(*) FROM #{table} WHERE key = ?")
    :ok = Exqlite.Sqlite3.bind(stmt, [key])
    {:row, [count]} = Exqlite.Sqlite3.step(conn, stmt)
    :ok = Exqlite.Sqlite3.release(conn, stmt)
    count
  end

  defp open_db(path) do
    {:ok, conn} = Exqlite.Sqlite3.open(path)
    conn
  end

  # ── update/2 ─────────────────────────────────────────────────────────

  describe "update/2" do
    test "sets db_path and table from config list" do
      state = KeyValueStore.update(["/custom/path.db", "custom_table"], %{})

      assert state.db_path == "/custom/path.db"
      assert state.table == "custom_table"
    end

    test "preserves existing state keys" do
      state = KeyValueStore.update(["/path.db", "t"], %{existing: :value})

      assert state.existing == :value
      assert state.db_path == "/path.db"
      assert state.table == "t"
    end

    test "ignores invalid config" do
      state = %{existing: :value}
      assert KeyValueStore.update([], state) == state
      assert KeyValueStore.update(["only_path"], state) == state
      assert KeyValueStore.update([123, "table"], state) == state
    end
  end

  # ── search_match/2 ───────────────────────────────────────────────────

  describe "search_match/2" do
    test "returns empty list for empty database" do
      state = default_state()
      assert KeyValueStore.search_match("foo", state) == []
    end

    test "returns matching keys by substring" do
      state = default_state()
      conn = open_db(state.db_path)
      create_table(conn, state.table)
      insert_key(conn, state.table, "alpha", "1")
      insert_key(conn, state.table, "apple", "2")
      insert_key(conn, state.table, "beta", "3")
      insert_key(conn, state.table, "apricot", "4")
      :ok = Exqlite.Sqlite3.close(conn)

      results = KeyValueStore.search_match("ap", state)

      refute "alpha" in results
      assert "apple" in results
      assert "apricot" in results
      refute "beta" in results
    end

    test "finds keys where term appears mid-string (legacy Python behavior)" do
      state = default_state()
      conn = open_db(state.db_path)
      create_table(conn, state.table)
      insert_key(conn, state.table, "compass-rabbitmq", "1")
      insert_key(conn, state.table, "compass-redis-pass", "2")
      :ok = Exqlite.Sqlite3.close(conn)

      results = KeyValueStore.search_match("rabbit", state)

      assert "compass-rabbitmq" in results
      refute "compass-redis-pass" in results
    end

    test "returns string keys for numeric-affinity legacy tables (snails.db schema)" do
      # The real ~/.emacs.d/priv/snails.db table is
      # `CREATE TABLE kvstore ("key" string PRIMARY KEY UNIQUE, value string)`.
      # "string" is not a SQLite TEXT type — it yields NUMERIC affinity, so
      # numeric-looking keys are stored as INTEGER and Exqlite returns them
      # as Elixir integers, which crashed candidate_text/1 during rendering.
      state = default_state()
      conn = open_db(state.db_path)

      # Drop first: guards against any earlier implicit schema creation.
      {:ok, drop_stmt} = Exqlite.Sqlite3.prepare(conn, "DROP TABLE IF EXISTS #{state.table}")
      :ok = Exqlite.Sqlite3.bind(drop_stmt, [])
      _ = Exqlite.Sqlite3.step(conn, drop_stmt)
      :ok = Exqlite.Sqlite3.release(conn, drop_stmt)

      # Exact legacy DDL from ~/.emacs.d/priv/snails.db.
      {:ok, stmt} =
        Exqlite.Sqlite3.prepare(
          conn,
          ~s{CREATE TABLE #{state.table} ("key" string PRIMARY KEY UNIQUE, value string)}
        )

      :ok = Exqlite.Sqlite3.bind(stmt, [])
      _ = Exqlite.Sqlite3.step(conn, stmt)
      :ok = Exqlite.Sqlite3.release(conn, stmt)

      insert_key(conn, state.table, 2_210_506_045, "some-secret")
      insert_key(conn, state.table, "compass-admin", "other")
      :ok = Exqlite.Sqlite3.close(conn)

      results = KeyValueStore.search_match("", state)

      # Integer key must surface as a binary, not crash rendering.
      assert "2210506045" in results
      assert is_binary("2210506045")
      assert "compass-admin" in results

      # Get action on the numeric key must not raise either.
      assert :ok = KeyValueStore.do_action("2210506045", state)
    end

    test "returns command candidates when prefix matches set regex" do
      state = default_state()
      results = KeyValueStore.search_match("set foo bar", state)

      assert "set foo bar" in results
    end

    test "returns command candidates when prefix matches del regex" do
      state = default_state()
      results = KeyValueStore.search_match("del foo", state)

      assert "del foo" in results
    end

    test "limits results to 20 keys" do
      state = default_state()
      conn = open_db(state.db_path)
      create_table(conn, state.table)

      for i <- 1..30 do
        insert_key(conn, state.table, "key_#{i}", "#{i}")
      end

      :ok = Exqlite.Sqlite3.close(conn)

      results = KeyValueStore.search_match("key_", state)
      assert length(results) == 20
    end

    test "returns empty list when database file does not exist" do
      state = %{db_path: "/nonexistent/path/test.db", table: "test_kv"}
      assert KeyValueStore.search_match("foo", state) == []
    end
  end

  # ── do_action/2 ──────────────────────────────────────────────────────

  describe "do_action/2 — set" do
    test "inserts a new key-value pair" do
      state = default_state()
      assert :ok = KeyValueStore.do_action("set mykey myvalue", state)

      conn = open_db(state.db_path)
      value = read_value(conn, state.table, "mykey")
      :ok = Exqlite.Sqlite3.close(conn)

      assert value == "myvalue"
    end

    test "updates an existing key-value pair" do
      state = default_state()
      conn = open_db(state.db_path)
      create_table(conn, state.table)
      insert_key(conn, state.table, "mykey", "oldvalue")
      :ok = Exqlite.Sqlite3.close(conn)

      assert :ok = KeyValueStore.do_action("set mykey newvalue", state)

      conn = open_db(state.db_path)
      value = read_value(conn, state.table, "mykey")
      :ok = Exqlite.Sqlite3.close(conn)

      assert value == "newvalue"
    end

    test "handles special characters in key and value" do
      state = default_state()
      key = "key-with_special/chars"
      value = "value with spaces and $pecial @chars!"

      assert :ok = KeyValueStore.do_action("set #{key} #{value}", state)

      conn = open_db(state.db_path)
      stored = read_value(conn, state.table, key)
      :ok = Exqlite.Sqlite3.close(conn)

      assert stored == value
    end
  end

  describe "do_action/2 — del" do
    test "deletes an existing key" do
      state = default_state()
      conn = open_db(state.db_path)
      create_table(conn, state.table)
      insert_key(conn, state.table, "todelete", "value")
      :ok = Exqlite.Sqlite3.close(conn)

      assert :ok = KeyValueStore.do_action("del todelete", state)

      conn = open_db(state.db_path)
      count = key_count(conn, state.table, "todelete")
      :ok = Exqlite.Sqlite3.close(conn)

      assert count == 0
    end

    test "handles deleting non-existent key gracefully" do
      state = default_state()
      assert :ok = KeyValueStore.do_action("del nonexistent", state)
    end
  end

  describe "do_action/2 — get" do
    test "copies value for existing key (via kill-new message)" do
      state = default_state()
      conn = open_db(state.db_path)
      create_table(conn, state.table)
      insert_key(conn, state.table, "greeting", "Hello, World!")
      :ok = Exqlite.Sqlite3.close(conn)

      assert :ok = KeyValueStore.do_action("greeting", state)
    end

    test "handles non-existent key gracefully" do
      state = default_state()
      assert :ok = KeyValueStore.do_action("nonexistent", state)
    end
  end
end
