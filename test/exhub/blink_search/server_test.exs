defmodule Exhub.BlinkSearch.ServerTest do
  use ExUnit.Case, async: false

  alias Exhub.BlinkSearch.Server

  # The named Server GenServer is shared, so these tests must not run in
  # parallel with each other or with other suites starting the server.
  setup do
    # Bare registry so Exhub.send_message dispatch reaches our spy (or is a
    # no-op) instead of raising under --no-start.
    start_supervised!({Registry, keys: :unique, name: Exhub.Registry})
    start_supervised!(Server)

    Application.put_env(:exhub, :blink_search_debounce_ms, 10)
    on_exit(fn -> Application.delete_env(:exhub, :blink_search_debounce_ms) end)

    :ok
  end

  # ── Helpers ──────────────────────────────────────────────────────────

  # Spies on renders: registers under "socket_handler" so
  # Exhub.send_message/1 forwards {:send_to_emacs, msg} to it.
  defp start_spy do
    parent = self()

    {:ok, _spy} =
      Task.start_link(fn ->
        Registry.register(Exhub.Registry, "socket_handler", nil)
        spy_loop(parent)
      end)

    :ok
  end

  defp spy_loop(parent) do
    receive do
      {:send_to_emacs, msg} ->
        send(parent, {:render, msg})
        spy_loop(parent)

      :stop ->
        :ok
    end
  end

  defp collect_renders(timeout, acc \\ []) do
    receive do
      {:render, msg} -> collect_renders(timeout, [msg | acc])
    after
      timeout -> Enum.reverse(acc)
    end
  end

  defp wait_until(fun, deadline_ms \\ 2_000) do
    start = System.monotonic_time(:millisecond)

    if fun.() do
      :ok
    else
      if System.monotonic_time(:millisecond) - start >= deadline_ms do
        flunk("wait_until timed out")
      end

      Process.sleep(5)
      wait_until(fun, deadline_ms)
    end
  end

  # ── Debounce / coalescing ────────────────────────────────────────────

  test "coalesces a keystroke burst into one dispatch of the latest keyword" do
    start_spy()
    Server.update_backend("Buffer List", ["alpha-foo", "alpha-baz"])
    Application.put_env(:exhub, :blink_search_debounce_ms, 1_000)

    Server.search("foo", 20, ["Buffer List"])
    Server.search("baz", 20, ["Buffer List"])

    # Both casts processed; nothing dispatched while the debounce is armed
    wait_until(fn -> match?(%{keyword: "baz"}, Server.get_state().pending_search) end)
    refute_receive {:render, _}, 200
    assert Server.get_state().search_tasks == []

    # The timer fires once and dispatches only the latest keyword
    assert_receive {:render, msg}, 2_000
    renders = [msg | collect_renders(200)]

    assert Enum.all?(renders, &String.contains?(&1, "alpha-baz"))
    refute Enum.any?(renders, &String.contains?(&1, "alpha-foo"))
  end

  test "holds the search pending until the debounce timer fires" do
    Application.put_env(:exhub, :blink_search_debounce_ms, 5_000)

    Server.search("held", 20, ["Buffer List"])

    # Casts are processed before our call in mailbox order
    state = Server.get_state()
    assert %{keyword: "held"} = state.pending_search
    assert state.search_tasks == []

    # Lower the debounce so the "latest" search dispatches quickly (the cast
    # reads the env when it arms its timer).
    Application.put_env(:exhub, :blink_search_debounce_ms, 10)

    Server.search("latest", 20, ["Buffer List"])
    assert %{keyword: "latest"} = Server.get_state().pending_search

    wait_until(fn -> Server.get_state().pending_search == nil end)
    assert Server.get_state().pending_search == nil
    assert Server.get_state().search_tasks != []
  end

  test "a superseding search kills and clears in-flight backend tasks" do
    Application.put_env(:exhub, :blink_search_debounce_ms, 0)
    Server.update_backend("Buffer List", ["x"])

    Server.search("a", 20, ["Buffer List"])
    wait_until(fn -> Server.get_state().search_tasks != [] end)

    # Kill + clear happen synchronously in the cast
    Server.search("b", 20, ["Buffer List"])
    assert Server.get_state().search_tasks == []
  end

  test "kill_search_tasks/1 terminates in-flight tasks" do
    {:ok, pid} = Task.start(fn -> Process.sleep(30_000) end)
    assert Process.alive?(pid)

    Server.kill_search_tasks([pid])

    refute Process.alive?(pid)
  end

  test "clean/0 kills in-flight tasks and drops pending searches" do
    Application.put_env(:exhub, :blink_search_debounce_ms, 5_000)

    Server.search("pending", 20, ["Buffer List"])
    assert %{keyword: "pending"} = Server.get_state().pending_search

    Server.clean()
    state = Server.get_state()
    assert state.pending_search == nil
    assert state.search_tasks == []
  end
end
