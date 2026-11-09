defmodule Exhub.ResponseHandlers.ExhubFileTest do
  use ExUnit.Case, async: true

  alias Exhub.ResponseHandlers.ExhubFile

  describe "render_markdown/1" do
    test "renders mermaid fences as base64 SVG images" do
      html = ExhubFile.render_markdown("```mermaid\nflowchart LR\n  A-->B-->C\n```")

      assert html =~ ~s(<div class="mdex-mermex">)
      assert html =~ ~s(<img src="data:image/svg+xml;base64,)
      refute html =~ "<button"
      refute html =~ "mdex-mermex-toolbar"
    end

    test "keeps regular Markdown rendering" do
      html = ExhubFile.render_markdown("# Hi\n\n**bold**")

      assert html =~ "<h1>Hi</h1>"
      assert html =~ "<strong>bold</strong>"
    end

    test "falls back to a code block when the diagram is invalid" do
      html = ExhubFile.render_markdown("```mermaid\nthis is not a diagram\n```")

      refute html =~ "data:image/svg+xml"
      assert html =~ "<code"
    end

    test "sanitizes raw HTML while keeping diagrams" do
      html =
        ExhubFile.render_markdown(
          "<script>alert(1)</script>\n\n```mermaid\nflowchart LR\n  A-->B\n```"
        )

      refute html =~ "<script>"
      assert html =~ "data:image/svg+xml;base64,"
    end
  end
end
