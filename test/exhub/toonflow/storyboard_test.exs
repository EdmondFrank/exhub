defmodule Exhub.Toonflow.StoryboardTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Storyboard

  describe "parse_storyboard/1" do
    test "parses fenced shots and assigns sequential idx" do
      raw =
        "```json\n" <>
          ~s({"shots":[{"scene":"祖宅","shot_desc":"林昭立于门前","size":"中景","lighting":"冷光","motion":"推","characters":["林昭"],"prompt":"青衫少年立于门前"},{"scene":"祖宅","shot_desc":"族长发怒"}]}) <>
          "\n```"

      assert {:ok, [first, second]} = Storyboard.parse_storyboard(raw)
      assert first["idx"] == 1
      assert first["scene"] == "祖宅"
      assert first["size"] == "中景"
      assert first["motion"] == "推"
      assert first["characters"] == ["林昭"]
      assert second["idx"] == 2
      assert second["characters"] == []
    end

    test "accepts a bare list and Chinese field aliases" do
      raw = ~s([{"场景":"街市","景别":"远景","运镜":"摇"}])

      assert {:ok, [shot]} = Storyboard.parse_storyboard(raw)
      assert shot["scene"] == "街市"
      assert shot["size"] == "远景"
      assert shot["motion"] == "摇"
    end

    test "errors when no shots are present" do
      assert {:error, {:missing, "shots"}} = Storyboard.parse_storyboard(~s({"foo":[]}))
      assert {:error, :invalid_payload} = Storyboard.parse_storyboard(nil)
    end
  end

  describe "shot_prompt/2" do
    test "composes from fields when no prompt is given" do
      shot = %{
        "scene" => "祖宅",
        "shot_desc" => "林昭立于门前",
        "size" => "中景",
        "motion" => "推",
        "lighting" => "冷光"
      }

      assert Storyboard.shot_prompt(shot) == "祖宅，林昭立于门前，中景，推，冷光"
    end

    test "prefers an explicit prompt and appends character appearances" do
      shot = %{"prompt" => "青衫少年立于门前", "characters" => ["林昭", "无名"]}
      characters = [%{"name" => "林昭", "appearance" => "青衫少年"}]

      assert Storyboard.shot_prompt(shot, characters) == "青衫少年立于门前；林昭（青衫少年）"
    end
  end
end
