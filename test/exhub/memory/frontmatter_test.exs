defmodule Exhub.Memory.FrontmatterTest do
  use ExUnit.Case, async: true

  alias Exhub.Memory.Frontmatter

  test "round-trips scalars, tags, JSON maps and nested lists" do
    meta = %{
      "memory_id" => "memory_abc",
      "status" => "approved",
      "title" => "Fix: run tests first",
      "tags" => ["project/exhub", "area/mcp"],
      "evaluation" => %{"promoted" => true, "mean" => 0.7},
      "evidence" => [%{"session" => "s1", "events" => [14, 19]}]
    }

    content = "---\n" <> Frontmatter.encode(meta) <> "\n---\n\nBody line 1\nBody line 2"
    {decoded, body} = Frontmatter.decode(content)

    assert decoded["memory_id"] == "memory_abc"
    assert decoded["status"] == "approved"
    assert decoded["title"] == "Fix: run tests first"
    assert decoded["tags"] == ["project/exhub", "area/mcp"]
    assert decoded["evaluation"]["promoted"] == true
    assert decoded["evaluation"]["mean"] == 0.7
    assert decoded["evidence"] == [%{"session" => "s1", "events" => [14, 19]}]
    assert body == "Body line 1\nBody line 2"
  end

  test "decodes content with no frontmatter as an empty map and full body" do
    assert {%{}, "just a body"} = Frontmatter.decode("just a body")
  end

  test "drops nil and empty values from the encoded frontmatter" do
    encoded = Frontmatter.encode(%{"a" => "one", "b" => nil, "c" => ""})
    assert encoded == "a: one"
  end

  test "preserves the string type of numeric-looking values" do
    encoded = Frontmatter.encode(%{"n" => "1"})
    {decoded, _} = Frontmatter.decode("---\n" <> encoded <> "\n---\n\n")
    assert decoded["n"] == "1"
  end

  test "JSON-quotes values that would otherwise be ambiguous" do
    encoded = Frontmatter.encode(%{"tags" => ["a", "b"], "title" => "true"})
    {decoded, _} = Frontmatter.decode("---\n" <> encoded <> "\n---\n\n")
    assert decoded["tags"] == ["a", "b"]
    assert decoded["title"] == "true"
  end
end
