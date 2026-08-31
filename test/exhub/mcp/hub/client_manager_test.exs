defmodule Exhub.MCP.Hub.ClientManagerTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Hub.ClientManager

  describe "normalize_tool_name/2" do
    test "strips a matching #{inspect("server__")} prefix" do
      assert ClientManager.normalize_tool_name("gitee", "gitee__create_enterprise_issue") ==
               "create_enterprise_issue"
    end

    test "leaves bare tool names untouched" do
      assert ClientManager.normalize_tool_name("gitee", "create_enterprise_issue") ==
               "create_enterprise_issue"
    end

    test "does not strip a prefix belonging to a different server" do
      assert ClientManager.normalize_tool_name("github", "gitee__create_enterprise_issue") ==
               "gitee__create_enterprise_issue"
    end

    test "keeps names where a genuine double underscore is part of the tool name" do
      assert ClientManager.normalize_tool_name("gitee", "list__all") == "list__all"
    end

    test "does not reduce to an empty name" do
      assert ClientManager.normalize_tool_name("gitee", "gitee__") == "gitee__"
    end

    test "passes through non-string inputs" do
      assert ClientManager.normalize_tool_name("gitee", nil) == nil
    end
  end
end
