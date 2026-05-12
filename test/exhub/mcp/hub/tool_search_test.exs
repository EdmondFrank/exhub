defmodule Exhub.MCP.Hub.ToolSearchTest do
  use ExUnit.Case, async: true
  alias Exhub.MCP.Hub.ToolSearch

  test "build_index/1 creates inverted index from tools" do
    tools = [
      %{"server" => "desktop", "name" => "read_file", "description" => "Read file contents"},
      %{"server" => "desktop", "name" => "write_file", "description" => "Write file contents"},
      %{"server" => "github", "name" => "get_repo", "description" => "Get repository info"}
    ]

    index = ToolSearch.build_index(tools)
    assert is_tuple(index)
    assert tuple_size(index) == 2
  end

  test "search/3 returns ranked results" do
    tools = [
      %{"server" => "desktop", "name" => "read_file", "description" => "Read file contents"},
      %{"server" => "desktop", "name" => "write_file", "description" => "Write file contents"}
    ]

    index = ToolSearch.build_index(tools)
    results = ToolSearch.search(index, "read file", limit: 2)
    assert length(results) > 0
    assert hd(results)["name"] == "read_file"
  end

  test "search/3 returns empty list for no matches" do
    index = ToolSearch.build_index([])
    assert ToolSearch.search(index, "nonexistent") == []
  end
end
