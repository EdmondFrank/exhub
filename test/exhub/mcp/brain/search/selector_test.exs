defmodule Exhub.MCP.Brain.Search.SelectorTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.Search.Selector

  test "select/1 returns keyword for tag queries" do
    assert Selector.select("tag:project/active") == "keyword"
  end

  test "select/1 returns recency for recency-indicating queries" do
    assert Selector.select("recent meeting notes") == "recency"
    assert Selector.select("latest release notes") == "recency"
    assert Selector.select("newest blog posts") == "recency"
    assert Selector.select("new ideas") == "recency"
  end

  test "select/1 returns semantic for conversational (>= 4 word) queries" do
    assert Selector.select("how do we handle authentication and login flows") == "semantic"
    assert Selector.select("explain the architecture and data flow of the system") == "semantic"
  end

  test "select/1 returns keyword for short single-word queries" do
    assert Selector.select("groceries") == "keyword"
    assert Selector.select("meeting") == "keyword"
  end

  test "select/1 returns balanced for plain multi-word queries" do
    assert Selector.select("meeting notes") == "balanced"
    assert Selector.select("todo list") == "balanced"
  end

  test "select/1 treats tag queries before recency keywords" do
    assert Selector.select("tag:recent") == "keyword"
  end

  test "semantic_query?/1 only when a query looks conversational" do
    assert Selector.semantic_query?("how do we handle authentication and login flows")
    refute Selector.semantic_query?("groceries")
    refute Selector.semantic_query?("meeting notes")
  end
end