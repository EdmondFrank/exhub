defmodule Exhub.MCP.MacUse.ToolsMetadataTest do
  use ExUnit.Case, async: true

  @tools [
    {Exhub.MCP.Tools.MacUse.ListApps, "list_apps", []},
    {Exhub.MCP.Tools.MacUse.Snapshot, "snapshot", []},
    {Exhub.MCP.Tools.MacUse.Click, "click", [:selector]},
    {Exhub.MCP.Tools.MacUse.DblClick, "dblclick", [:selector]},
    {Exhub.MCP.Tools.MacUse.Input, "input", [:selector, :text]},
    {Exhub.MCP.Tools.MacUse.Fill, "fill", [:selector, :text]},
    {Exhub.MCP.Tools.MacUse.Press, "press", [:keys]},
    {Exhub.MCP.Tools.MacUse.Hover, "hover", [:selector]},
    {Exhub.MCP.Tools.MacUse.Focus, "focus", [:selector]},
    {Exhub.MCP.Tools.MacUse.ScrollTo, "scroll_to", [:selector]},
    {Exhub.MCP.Tools.MacUse.Scroll, "scroll", [:selector, :direction, :amount]},
    {Exhub.MCP.Tools.MacUse.Screenshot, "screenshot", []},
    {Exhub.MCP.Tools.MacUse.GetAttribute, "get_attribute", [:selector, :attribute]},
    {Exhub.MCP.Tools.MacUse.Wait, "wait", []},
    {Exhub.MCP.Tools.MacUse.Activate, "activate", []},
    {Exhub.MCP.Tools.MacUse.Mouse, "mouse", [:action]},
    {Exhub.MCP.Tools.MacUse.Keyboard, "keyboard", [:action, :text]}
  ]

  for {module, expected_name, _required_fields} <- @tools do
    describe "#{inspect(module)}" do
      test "has correct name" do
        assert unquote(module).name() == unquote(expected_name)
      end

      test "has non-empty description" do
        desc = unquote(module).description()
        assert is_binary(desc)
        assert String.length(desc) > 0
      end
    end
  end

  describe "tool count" do
    test "MacUseServer registers exactly 17 tool components" do
      # Verify the module exists and has the expected tool components
      # by checking the module attribute that Anubis uses
      tools = Exhub.MCP.MacUseServer.__components__()
      assert length(tools) == 17
    end
  end

  describe "all tool names are unique" do
    test "no duplicate tool names" do
      names = Enum.map(@tools, fn {mod, _, _} -> mod.name() end)
      assert length(names) == length(Enum.uniq(names))
    end
  end

  describe "all tool names match module convention" do
    test "tool names contain no prefix" do
      for {module, expected_name, _} <- @tools do
        refute String.starts_with?(expected_name, "mac_"),
               "#{inspect(module)} has mac_ prefix"
      end
    end
  end
end
