defmodule Exhub.LspBridge.LedgerTest do
  use ExUnit.Case, async: false

  alias Exhub.LspBridge.Ledger

  @error %{
    "severity" => 1,
    "message" => "expected type 'u8'",
    "source" => "zls",
    "range" => %{
      "start" => %{"line" => 3, "character" => 22},
      "end" => %{"line" => 3, "character" => 40}
    }
  }

  @warning %{
    "severity" => 2,
    "message" => "unused variable",
    "source" => "zls",
    "range" => %{
      "start" => %{"line" => 9, "character" => 4},
      "end" => %{"line" => 9, "character" => 10}
    }
  }

  setup do
    unless Process.whereis(Ledger), do: start_supervised!({Ledger, []})
    Ledger.reset()
    :ok
  end

  test "records counts and keeps error diagnostics" do
    assert :ok = Ledger.record("/tmp/a.zig", [@error, @warning])

    entry = Ledger.get("/tmp/a.zig")

    assert entry.count == 2
    assert entry.errors == 1
    assert entry.warnings == 1
    assert [%{"message" => "expected type 'u8'"}] = entry.diagnostics
  end

  test "summary aggregates every entry" do
    :ok = Ledger.record("/tmp/a.zig", [@error])
    :ok = Ledger.record("/tmp/b.zig", [@warning])

    summary = Ledger.summary()

    assert summary.files == 2
    assert summary.errors == 1
    assert summary.warnings == 1
    assert is_integer(summary.updated_at)
  end

  test "clear forgets one file, reset forgets everything" do
    :ok = Ledger.record("/tmp/a.zig", [@error])
    :ok = Ledger.record("/tmp/b.zig", [@warning])

    :ok = Ledger.clear("/tmp/a.zig")

    assert Ledger.get("/tmp/a.zig") == nil
    assert Ledger.get("/tmp/b.zig") != nil

    :ok = Ledger.reset()
    assert Ledger.all() == []
  end

  test "session updates are recorded as they arrive" do
    send(Ledger, {:lsp_diagnostics_update, "/tmp/c.zig", [@error], 1})
    :ok = :sys.get_state(Ledger) && :ok

    assert %{errors: 1} = Ledger.get("/tmp/c.zig")
  end

  test "unrelated messages are ignored" do
    send(Ledger, {:lsp_notification, "zls", "window/logMessage", %{}})

    assert %{} = :sys.get_state(Ledger)
    assert Ledger.all() == []
  end

  test "a clean result replaces the previous errors" do
    :ok = Ledger.record("/tmp/a.zig", [@error])
    :ok = Ledger.record("/tmp/a.zig", [])

    assert %{errors: 0, count: 0, diagnostics: []} = Ledger.get("/tmp/a.zig")
  end
end
