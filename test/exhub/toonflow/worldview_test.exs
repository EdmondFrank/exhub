defmodule Exhub.Toonflow.WorldviewTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{Store, Worldview}

  describe "digest/1" do
    test "is empty with no notes" do
      assert Worldview.digest([]) == ""
    end

    test "lists the anchors with their titles" do
      digest =
        Worldview.digest([
          %{"title" => "世界观", "text" => "九州大陆，灵气复苏"},
          %{"title" => "画风", "text" => "国风水墨"}
        ])

      assert digest =~ "（世界观设定，必须保持一致）"
      assert digest =~ "- 世界观：九州大陆，灵气复苏"
      assert digest =~ "- 画风：国风水墨"
    end

    test "skips blank texts and defaults a missing title" do
      digest = Worldview.digest([%{"text" => "  "}, %{"text" => "无题设定"}])
      assert digest == "（世界观设定，必须保持一致）\n- 世界观：无题设定"
    end

    test "truncates a very long anchor" do
      digest = Worldview.digest([%{"title" => "设定", "text" => String.duplicate("长", 500)}])
      assert String.length(digest) < 450
    end
  end

  describe "notes" do
    setup do
      root =
        Path.join(System.tmp_dir!(), "toonflow_worldview_#{System.unique_integer([:positive])}")

      server = :"toonflow_worldview_store_#{System.unique_integer([:positive])}"
      {:ok, pid} = Store.start_link(root_dir: root, name: server)
      assert {:ok, _} = Store.create_project("demo", [], server)

      on_exit(fn ->
        if Process.alive?(pid), do: GenServer.stop(pid)
        File.rm_rf(root)
      end)

      %{server: server}
    end

    test "set/3 upserts one note per title and get/3 lists it", %{server: server} do
      assert {:ok, %{"kind" => "worldview", "title" => "世界观", "text" => "九州"}} =
               Worldview.set("demo", [text: "九州"], server)

      assert {:ok, [note]} = Worldview.get("demo", [], server)
      assert note["kind"] == Worldview.kind()
      assert note["title"] == "世界观"
      assert note["text"] == "九州"

      # Re-writing the same title replaces the note rather than duplicating it.
      assert {:ok, _} = Worldview.set("demo", [text: "八荒"], server)
      assert {:ok, [note]} = Worldview.get("demo", [], server)
      assert note["text"] == "八荒"

      # A second title is stored alongside it, and can be filtered.
      assert {:ok, _} = Worldview.set("demo", [title: "画风", text: "水墨"], server)
      assert {:ok, [_worldview, _style]} = Worldview.get("demo", [], server)

      assert {:ok, [%{"title" => "画风", "text" => "水墨"}]} =
               Worldview.get("demo", [title: "画风"], server)
    end

    test "append/3 folds the digest into a stage's instructions", %{server: server} do
      # With no note, instructions pass through untouched.
      assert Worldview.append("demo", "原有指令", server) == "原有指令"

      assert {:ok, _} = Worldview.set("demo", [text: "九州"], server)

      assert Worldview.append("demo", "原有指令", server) ==
               "原有指令\n\n（世界观设定，必须保持一致）\n- 世界观：九州"

      # ...and a nil instruction becomes just the digest.
      assert Worldview.append("demo", nil, server) == "（世界观设定，必须保持一致）\n- 世界观：九州"
    end

    test "digest_for/2 is empty when there is no note", %{server: server} do
      assert {:ok, ""} = Worldview.digest_for("demo", server)
      assert {:ok, _} = Worldview.set("demo", [text: "九州"], server)
      assert {:ok, digest} = Worldview.digest_for("demo", server)
      assert digest =~ "九州"
    end

    test "set/3 validates its text", %{server: server} do
      assert {:error, :missing_text} = Worldview.set("demo", [], server)
    end
  end
end
