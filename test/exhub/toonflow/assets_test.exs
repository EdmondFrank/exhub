defmodule Exhub.Toonflow.AssetsTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Assets

  describe "parse_assets/1" do
    test "parses fenced JSON with characters, scenes and props" do
      raw =
        "```json\n" <>
          ~s({"characters":[{"name":"林昭","role":"主角","appearance":"青衫少年"}],"scenes":[{"name":"祖宅","description":"雨夜"}],"props":[{"name":"玉佩","description":"信物"}]}) <>
          "\n```"

      assert {:ok, assets} = Assets.parse_assets(raw)

      assert [%{"name" => "林昭", "appearance" => "青衫少年", "role" => "主角"} | _] =
               assets["characters"]

      assert Enum.map(assets["scenes"], & &1["name"]) == ["祖宅"]
      assert [%{"name" => "玉佩", "description" => "信物"}] = assets["props"]
    end

    test "accepts a bare top-level list as the character list" do
      assert {:ok, %{"characters" => [%{"name" => "甲"}], "scenes" => [], "props" => []}} =
               Assets.parse_assets(~s([{"name":"甲"}]))
    end

    test "recovers JSON embedded in prose (CJK-safe byte slicing)" do
      raw = ~s(好的，以下是结果：{"characters":[{"name":"阿九","appearance":"红衣"}]} 完成。)

      assert {:ok, %{"characters" => [%{"name" => "阿九", "appearance" => "红衣"}]}} =
               Assets.parse_assets(raw)
    end

    test "normalizes missing fields" do
      assert {:ok, %{"characters" => [%{"name" => "未命名"}], "scenes" => []}} =
               Assets.parse_assets(~s({"characters":[{}]}))
    end

    test "errors on input with no JSON" do
      assert {:error, :invalid_json} = Assets.parse_assets("这里没有 JSON")
      assert {:error, :invalid_payload} = Assets.parse_assets(nil)
    end
  end
end
