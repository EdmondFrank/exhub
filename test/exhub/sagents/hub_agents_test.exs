defmodule Exhub.Sagents.HubAgentsTest do
  use ExUnit.Case, async: false

  setup do
    start_supervised!(Exhub.Sagents.Hub)
    :ok
  end

  test "list_agents describes the capabilities of every profile" do
    agents = Exhub.Sagents.Hub.list_agents()

    assert Enum.map(agents, & &1.name) == [
             "assistant",
             "coder",
             "genclaw",
             "researcher",
             "toonflow"
           ]

    assert Enum.all?(agents, &is_boolean(&1.running))
  end

  test "MCP tool groups carry their tool counts" do
    coder = profile("coder")

    assert coder.tool_groups |> Enum.map(& &1.name) == ["desktop", "web-tools"]
    assert Enum.all?(coder.tool_groups, &(is_integer(&1.tool_count) and &1.tool_count > 0))
    assert coder.native_tools == []
  end

  test "middleware is listed with HITL interrupts" do
    coder = profile("coder")

    assert Enum.map(coder.middleware, & &1.name) == [
             "TodoList",
             "FileSystem",
             "Summarization",
             "PatchToolCalls",
             "HumanInTheLoop"
           ]

    assert Enum.find(coder.middleware, &(&1.name == "HumanInTheLoop")).interrupts ==
             ["execute_command", "write_file"]
  end

  test "native-only profiles report their in-process tools" do
    genclaw = profile("genclaw")

    assert genclaw.tool_groups == []
    assert "t2i" in genclaw.native_tools
    assert Enum.map(genclaw.middleware, & &1.name) == ["Perception", "CompletionGuard"]
  end

  defp profile(name), do: Enum.find(Exhub.Sagents.Hub.list_agents(), &(&1.name == name))
end
