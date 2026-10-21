defmodule Exhub.MCP.Tools.Lua.HelpTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Hub.BuiltInRegistry
  alias Exhub.MCP.Tools.Lua.Help

  describe "registration" do
    test "is exposed by the built-in `lua` server alongside code_mode" do
      assert BuiltInRegistry.built_in?("lua")

      tools = BuiltInRegistry.list_tools("lua")
      assert tools |> Enum.map(& &1["name"]) |> Enum.sort() == ["code_mode", "help"]

      tool = Enum.find(tools, &(&1["name"] == "help"))
      assert tool["server"] == "lua"
      assert is_binary(tool["description"])
      # `tool` is optional, so nothing is required.
      assert tool["inputSchema"]["required"] in [nil, []]
    end

    test "name/0 is help" do
      assert Help.name() == "help"
    end

    test "description points at the in-sandbox global" do
      assert Help.description() =~ "help()"
    end
  end
end
