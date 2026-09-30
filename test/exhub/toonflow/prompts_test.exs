defmodule Exhub.Toonflow.PromptsTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Prompts

  describe "render_string/2" do
    test "substitutes placeholders" do
      assert Prompts.render_string("你好 {{name}}，{{missing}}", %{"name" => "世界"}) ==
               "你好 世界，{{missing}}"
    end

    test "stringifies non-binary values" do
      assert Prompts.render_string("n={{n}}", %{"n" => 3}) == "n=3"
    end
  end

  describe "render/2" do
    test "loads and renders a shipped template" do
      assert {:ok, text} = Prompts.render("events.user", %{"title" => "第一章", "text" => "正文"})
      assert text =~ "第一章"
      assert text =~ "正文"
    end

    test "errors on a missing template" do
      assert {:error, :enoent} = Prompts.render("does_not_exist", %{})
    end
  end
end
