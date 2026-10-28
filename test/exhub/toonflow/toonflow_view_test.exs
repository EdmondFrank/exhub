defmodule Exhub.Router.ToonflowViewTest do
  use ExUnit.Case, async: true

  alias Exhub.Router.ToonflowView

  describe "render_index/1" do
    test "lists projects with counts and links" do
      projects = [
        %{
          "name" => "demo",
          "counts" => %{"chapters" => 2, "images" => 3, "videos" => 1, "output" => 1},
          "meta" => %{"description" => "a demo"}
        }
      ]

      html = ToonflowView.render_index(projects)

      assert html =~ "<!DOCTYPE html>"
      assert html =~ ~s(href="/toonflow/projects/demo")
      assert html =~ "a demo"
      assert html =~ "2 chapters"
      assert html =~ "3 images"
    end

    test "invites creation when there are no projects" do
      html = ToonflowView.render_index([])
      assert html =~ "No projects yet"
      assert html =~ "/toonflow/api/projects"
    end

    test "defaults to an empty list" do
      assert ToonflowView.render_index() =~ "No projects yet"
    end
  end

  describe "render_project/1" do
    test "embeds the project, the stage list and the websocket wiring" do
      html = ToonflowView.render_project("demo")

      assert html =~ ~s(data-project="demo")
      assert html =~ "/toonflow/ws?project="

      assert html =~
               ~s(const STAGES = ["novel","events","script","assets","storyboard","keyframes","images","videos","voices","assemble"];)

      assert html =~ "/toonflow/api/projects/"
    end

    test "renders an editable inspector with per-kind re-render buttons" do
      html = ToonflowView.render_project("demo")

      assert html =~ ~s(id="insp-form")
      assert html =~ "fieldInput(shot, 'dialogue', 'Dialogue', 2)"
      assert html =~ "const CAMERA_HINTS = ["
      assert html =~ "hintsFor = (name) =>"
      assert html =~ "Keyframe prompts"
      assert html =~ ~s(data-kind="video")
      assert html =~ ~s(data-kind="voice")
      assert html =~ ~s(id="insp-stale")
      assert html =~ "/toonflow/api/projects/"
      assert html =~ "/render"
    end

    test "renders the one-click AI assist controls" do
      html = ToonflowView.render_project("demo")

      assert html =~ "AI 助手"
      assert html =~ "const DIRECTOR_MODEL ="
      assert html =~ "const AI_ACTIONS ="
      assert html =~ ~s|data-action="${esc(action)}"|
      assert html =~ "aiAssist(shot, row.dataset.field"
      assert html =~ "/assist"
    end

    test "escapes the project name" do
      html = ToonflowView.render_project(~s(a"b<c))

      refute html =~ ~s(data-project="a"b<c")
      assert html =~ "a&quot;b&lt;c"
    end
  end
end
