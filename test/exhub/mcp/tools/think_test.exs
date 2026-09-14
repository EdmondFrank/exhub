defmodule Exhub.MCP.Tools.ThinkTest do
  use ExUnit.Case, async: false

  alias Anubis.Server.Context
  alias Anubis.Server.Frame
  alias Anubis.Server.Response
  alias Exhub.MCP.ScratchpadStore
  alias Exhub.MCP.Tools.Plan
  alias Exhub.MCP.Tools.Scratchpad
  alias Exhub.MCP.Tools.Think

  @store __MODULE__.Store
  @table :think_test_scratchpad

  setup_all do
    # Point the Scratchpad facade at a locally started store so these tests stay
    # pure (--no-start) and never touch the supervision tree.
    previous = Application.get_env(:exhub, Scratchpad, [])
    Application.put_env(:exhub, Scratchpad, store: @store)

    {:ok, pid} = ScratchpadStore.start_link(name: @store, table: @table)

    on_exit(fn ->
      Application.put_env(:exhub, Scratchpad, previous)
      if Process.alive?(pid), do: GenServer.stop(pid)
    end)

    :ok
  end

  defp frame_for(session_id) do
    %Frame{context: %Context{session_id: session_id}}
  end

  defp recorded(%Response{content: [%{"text" => json}]}) do
    JSON.decode!(json)
  end

  describe "think/2 scratchpad accumulation" do
    test "second call in the same session reports both thoughts" do
      sid = "sess-think-#{System.unique_integer([:positive])}"

      {:reply, r1, _frame} = Think.execute(%{thought: "step one"}, frame_for(sid))
      {:reply, r2, _frame} = Think.execute(%{thought: "step two"}, frame_for(sid))

      assert recorded(r1)["recorded"] == 1
      assert recorded(r2)["recorded"] == 2
      assert recorded(r2)["scratchpad"] == ["step one", "step two"]
    end

    test "different sessions are isolated" do
      a = "sess-a-#{System.unique_integer([:positive])}"
      b = "sess-b-#{System.unique_integer([:positive])}"

      {:reply, _, _} = Think.execute(%{thought: "a1"}, frame_for(a))
      {:reply, rb, _} = Think.execute(%{thought: "b1"}, frame_for(b))

      assert recorded(rb)["recorded"] == 1
      assert recorded(rb)["scratchpad"] == ["b1"]
    end

    test "missing session id degrades to a shared bucket instead of crashing" do
      {:reply, r, _frame} = Think.execute(%{thought: "orphan"}, %Frame{})
      assert is_map(recorded(r))
    end
  end

  describe "plan/2 shares the mechanism under its own key" do
    test "plans accumulate independently from thoughts" do
      sid = "sess-plan-#{System.unique_integer([:positive])}"

      {:reply, _, _} = Think.execute(%{thought: "think x"}, frame_for(sid))
      {:reply, p1, _} = Plan.execute(%{plan: "step 1"}, frame_for(sid))
      {:reply, p2, _} = Plan.execute(%{plan: "step 2"}, frame_for(sid))

      assert recorded(p1)["recorded"] == 1
      assert recorded(p2)["recorded"] == 2
      assert recorded(p2)["scratchpad"] == ["step 1", "step 2"]
    end
  end
end
