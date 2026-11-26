defmodule Exhub.Toonflow.ReverseTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{Assets, DB, Reverse, Schema, Store}

  describe "prompt/1" do
    test "characters keep the look and the 人设, and forbid a single-frame pose" do
      prompt = Reverse.prompt("character")

      assert prompt =~ "人设"
      assert prompt =~ "不要描述动作"
      assert prompt =~ "镜头角度"
      assert prompt =~ "背景环境"
    end

    test "scenes describe the environment, not the frame" do
      prompt = Reverse.prompt("scene")

      assert prompt =~ "空间类型"
      assert prompt =~ "不要描述画面中的人物"
      assert prompt =~ "镜头机位"
      assert prompt =~ "分辨率"
    end

    test "props are described on their own" do
      assert Reverse.prompt("prop") =~ "材质与工艺"
    end
  end

  describe "clean/1" do
    test "strips fences, a label, quotes and newlines" do
      raw = "```\n描述： “成年男性，黑色短发。\n 冷峻。”\n```"
      assert Reverse.clean(raw) == "成年男性，黑色短发。 冷峻。"
    end

    test "returns nil for blank input or a non-string" do
      assert Reverse.clean("   ") == nil
      assert Reverse.clean(nil) == nil
    end

    test "caps the description length" do
      assert String.length(Reverse.clean(String.duplicate("瞳", 2000))) == 600
    end
  end

  describe "infer/4" do
    setup do
      root =
        Path.join(System.tmp_dir!(), "toonflow_reverse_#{System.unique_integer([:positive])}")

      server = :"toonflow_reverse_store_#{System.unique_integer([:positive])}"
      {:ok, pid} = Store.start_link(root_dir: root, name: server)
      assert {:ok, _} = Store.create_project("demo", [], server)

      previous = Application.get_env(:exhub, :toonflow_vision)
      Application.put_env(:exhub, :toonflow_vision, __MODULE__.VisionStub)

      on_exit(fn ->
        if Process.alive?(pid), do: GenServer.stop(pid)
        File.rm_rf(root)

        if previous,
          do: Application.put_env(:exhub, :toonflow_vision, previous),
          else: Application.delete_env(:exhub, :toonflow_vision)
      end)

      %{server: server}
    end

    test "writes the inferred text to a character's appearance", %{server: server} do
      ref = reference_file(server, "sheet.png")
      insert_character(server, "带土", "旧描述", [ref])

      assert {:ok, %{"kind" => "character", "text" => text, "path" => ^ref}} =
               Reverse.infer("demo", "character", "带土", [], server)

      assert text =~ "冷峻"
      assert {:ok, [character]} = Assets.list_characters("demo", [name: "带土"], server)
      assert character["appearance"] == text
    end

    test "writes the inferred text to a scene's description", %{server: server} do
      ref = reference_file(server, "scene.png")
      insert_scene(server, "祖宅", "旧描述", [ref])

      assert {:ok, %{"kind" => "scene", "text" => text}} =
               Reverse.infer("demo", "scene", "祖宅", [], server)

      assert {:ok, scene} = Assets.get_scene("demo", "祖宅", server)
      assert scene["description"] == text
    end

    test "errs without a sheet and leaves the text alone", %{server: server} do
      insert_character(server, "带土", "旧描述", [])

      assert {:error, :no_reference} = Reverse.infer("demo", "character", "带土", [], server)

      assert {:ok, [character]} = Assets.list_characters("demo", [name: "带土"], server)
      assert character["appearance"] == "旧描述"
    end

    test "a vision failure leaves the stored text alone", %{server: server} do
      ref = reference_file(server, "broken.png")
      insert_character(server, "带土", "旧描述", [ref])

      assert {:error, :boom} = Reverse.infer("demo", "character", "带土", [], server)

      assert {:ok, [character]} = Assets.list_characters("demo", [name: "带土"], server)
      assert character["appearance"] == "旧描述"
    end

    test "validates the subject", %{server: server} do
      assert {:error, :invalid_reference_kind} =
               Reverse.infer("demo", "npc", "带土", [], server)

      assert {:error, :missing_name} = Reverse.infer("demo", "character", "", [], server)
      assert {:error, :subject_not_found} = Reverse.infer("demo", "character", "无", [], server)
    end
  end

  defp reference_file(server, name) do
    {:ok, project} = Store.get_project("demo", server)
    dir = Path.join(project["root_dir"], "refs")
    File.mkdir_p!(dir)
    path = Path.join(dir, name)
    File.write!(path, "REF")
    path
  end

  defp insert_character(server, name, appearance, refs) do
    {:ok, _} =
      Store.run_project(
        "demo",
        fn conn ->
          DB.execute(
            conn,
            "INSERT INTO characters (id, name, appearance, refs_json, meta_json) VALUES (?, ?, ?, ?, ?)",
            ["chr_#{name}", name, appearance, Schema.encode_json(refs), nil]
          )

          {:ok, name}
        end,
        server
      )

    name
  end

  defp insert_scene(server, name, description, refs) do
    {:ok, _} =
      Store.run_project(
        "demo",
        fn conn ->
          DB.execute(
            conn,
            "INSERT INTO scenes (id, name, description, refs_json, meta_json) VALUES (?, ?, ?, ?, ?)",
            ["scn_#{name}", name, description, Schema.encode_json(refs), nil]
          )

          {:ok, name}
        end,
        server
      )

    name
  end
end

defmodule Exhub.Toonflow.ReverseTest.VisionStub do
  @moduledoc false
  @behaviour Exhub.Toonflow.Vision

  @impl true
  def analyze(path, _opts) do
    if path =~ "broken", do: {:error, :boom}, else: {:ok, "成年男性，黑色短发，冷峻。"}
  end
end
