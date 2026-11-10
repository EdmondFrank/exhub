defmodule Exhub.Sagents.MarkdownTest do
  use ExUnit.Case, async: true

  alias Exhub.Sagents.Markdown

  test "renders headings, lists and emphasis" do
    html = Markdown.to_html("# Title\n\n- one\n- two\n\nSome **bold** and *italic*.")

    assert html =~ "<h1>Title</h1>"
    assert html =~ "<ul>\n<li>one</li>\n<li>two</li>\n</ul>"
    assert html =~ "<strong>bold</strong>"
    assert html =~ "<em>italic</em>"
  end

  test "renders fenced code with a language class" do
    html = Markdown.to_html("```elixir\n1 + 1\n```")

    assert html =~ ~s(<code class="language-elixir">)
    assert html =~ "1 + 1"
  end

  test "renders tables and autolinks" do
    html = Markdown.to_html("| a | b |\n|---|---|\n| 1 | 2 |\n\nsee https://example.com")

    assert html =~ "<table>"
    assert html =~ "<td>1</td>"
    assert html =~ ~s(<a href="https://example.com">)
  end

  test "drops raw HTML instead of passing it through" do
    html = Markdown.to_html("<script>alert(1)</script>\n\nsafe")

    refute html =~ "<script"
    refute html =~ "alert(1)"
    assert html =~ "<p>safe</p>"
  end

  test "blank and missing text renders nothing" do
    assert Markdown.to_html("") == ""
    assert Markdown.to_html("   \n") == ""
    assert Markdown.to_html(nil) == ""
  end

  test "stringifies non-binary input" do
    assert Markdown.to_html(42) == "<p>42</p>"
  end
end
