defmodule Exhub.Toonflow.NovelTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Novel

  describe "split_chapters/1" do
    test "splits on Chinese chapter headings" do
      text = """
      第一章 风雨欲来
      林昭站在祖宅门前。

      第二章 逐出
      族长宣布将林昭逐出家门。
      """

      assert [
               %{"title" => "第一章 风雨欲来", "text" => first},
               %{"title" => "第二章 逐出", "text" => second}
             ] = Novel.split_chapters(text)

      assert first =~ "祖宅门前"
      assert second =~ "逐出家门"
    end

    test "splits on English chapter headings" do
      text = "Chapter 1 The Storm\nRain fell.\n\nChapter 2 The Fall\nHe left."

      assert [%{"title" => "Chapter 1 The Storm"}, %{"title" => "Chapter 2 The Fall"}] =
               Novel.split_chapters(text)
    end

    test "splits on Markdown headings" do
      text = "# Prologue\nA.\n\n## Act One\nB."
      assert [%{"title" => "Prologue"}, %{"title" => "Act One"}] = Novel.split_chapters(text)
    end

    test "keeps text without headings as a single chapter" do
      assert [%{"title" => "全文", "text" => "just prose\nmore prose"}] =
               Novel.split_chapters("just prose\nmore prose")
    end

    test "blank text yields no chapters" do
      assert [] = Novel.split_chapters("   \n  \n")
    end

    test "does not split on an inline mention" do
      assert [%{"title" => "全文"}] = Novel.split_chapters("他排在第一章节后面。")
    end
  end
end
