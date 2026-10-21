defmodule Exhub.MCP.Tools.Lua.CodeModeTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Lua.CodeMode

  describe "registration" do
    test "is exposed by the built-in `lua` server" do
      assert Exhub.MCP.Hub.BuiltInRegistry.built_in?("lua")

      tools = Exhub.MCP.Hub.BuiltInRegistry.list_tools("lua")
      assert tools |> Enum.map(& &1["name"]) |> Enum.sort() == ["code_mode", "help"]

      tool = Enum.find(tools, &(&1["name"] == "code_mode"))
      assert tool["server"] == "lua"
      assert tool["inputSchema"]["required"] == ["code"]
      assert is_binary(tool["description"])
    end

    test "description is on-demand by default: discovery pointers, no catalog" do
      text = CodeMode.description()

      assert text =~ "Lua 5.3"
      assert text =~ "retrieve_tools"
      assert text =~ "help()"
      refute text =~ "Available namespaces"
    end

    test "name/0 is code_mode" do
      assert CodeMode.name() == "code_mode"
    end
  end

  # The error branches answer before touching ClientManager, so they run without
  # the application booted.
  describe "execute/2 validation" do
    test "rejects an empty code string" do
      frame = %{}
      assert {:reply, resp, ^frame} = CodeMode.execute(%{code: ""}, frame)
      assert resp.isError
    end

    test "rejects a missing code argument" do
      frame = %{}
      assert {:reply, resp, ^frame} = CodeMode.execute(%{}, frame)
      assert resp.isError
    end

    test "rejects a non-positive timeout" do
      frame = %{}
      assert {:reply, resp, ^frame} = CodeMode.execute(%{code: "return 1", timeout: 0}, frame)
      assert resp.isError
    end
  end
end
