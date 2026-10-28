defmodule Exhub.Toonflow.RendererTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Renderer

  test "kinds/0 lists the media the inspector can re-render" do
    assert Renderer.kinds() == ~w(keyframe image video voice)
  end

  describe "normalize_kinds/1" do
    test "accepts one name or a list" do
      assert Renderer.normalize_kinds("video") == ["video"]
      assert Renderer.normalize_kinds(["voice", "video"]) == ["video", "voice"]
    end

    test "expands all, drops unknown kinds and duplicates" do
      assert Renderer.normalize_kinds("all") == Renderer.kinds()
      assert Renderer.normalize_kinds(["video", "video", "nope"]) == ["video"]
      assert Renderer.normalize_kinds([:voice]) == ["voice"]
    end

    test "is empty for anything else" do
      assert Renderer.normalize_kinds(nil) == []
      assert Renderer.normalize_kinds(42) == []
      assert Renderer.normalize_kinds(%{}) == []
    end
  end
end
