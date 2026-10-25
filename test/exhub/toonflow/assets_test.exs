defmodule Exhub.Toonflow.AssetsTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{Assets, DB, Media, Schema, Store, Storyboard}

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

  describe "scene & prop library" do
    setup do
      root = Path.join(System.tmp_dir!(), "toonflow_assets_#{System.unique_integer([:positive])}")
      server = :"toonflow_assets_store_#{System.unique_integer([:positive])}"
      {:ok, pid} = Store.start_link(root_dir: root, name: server)
      assert {:ok, _} = Store.create_project("demo", [], server)

      previous = Application.get_env(:exhub, :toonflow_media_client)
      Application.put_env(:exhub, :toonflow_media_client, __MODULE__.StubClient)

      on_exit(fn ->
        if Process.alive?(pid), do: GenServer.stop(pid)
        File.rm_rf(root)

        if previous,
          do: Application.put_env(:exhub, :toonflow_media_client, previous),
          else: Application.delete_env(:exhub, :toonflow_media_client)
      end)

      %{server: server}
    end

    test "extract_assets persists scenes and props (idempotently)", %{server: server} do
      insert_script(server)

      previous = Application.get_env(:exhub, :toonflow_llm)
      Application.put_env(:exhub, :toonflow_llm, __MODULE__.LLMStub)

      on_exit(fn ->
        if previous,
          do: Application.put_env(:exhub, :toonflow_llm, previous),
          else: Application.delete_env(:exhub, :toonflow_llm)
      end)

      assert {:ok, summary} = Assets.extract_assets("demo", [], server)
      assert summary["characters"] == 1
      assert summary["scenes"] == 1
      assert summary["props"] == 1

      assert {:ok, [scene]} = Assets.list_scenes("demo", [], server)
      assert scene["name"] == "祖宅"
      assert scene["description"] == "雨夜"

      assert {:ok, [prop]} = Assets.list_props("demo", [], server)
      assert prop["name"] == "玉佩"
      assert prop["description"] == "信物"

      # Re-running upserts by name instead of duplicating.
      assert {:ok, _} = Assets.extract_assets("demo", [], server)
      assert {:ok, [_]} = Assets.list_scenes("demo", [], server)
      assert {:ok, [_]} = Assets.list_props("demo", [], server)
    end

    test "generate_reference stores the image on the subject's refs", %{server: server} do
      insert_scene(server, "祖宅", "雨夜庭院")

      assert {:ok, reference} =
               Assets.generate_reference("demo", [kind: "scene", name: "祖宅"], server)

      assert reference["kind"] == "scene"
      assert reference["name"] == "祖宅"
      assert reference["path"] =~ "/assets/images/ref_scene_"
      assert File.exists?(reference["path"])
      assert reference["refs"] == [reference["path"]]

      assert {:ok, scene} = Assets.get_scene("demo", "祖宅", server)
      assert scene["refs"] == [reference["path"]]
    end

    test "generate_reference validates its target", %{server: server} do
      assert {:error, :invalid_reference_kind} =
               Assets.generate_reference("demo", [name: "祖宅"], server)

      assert {:error, :missing_name} =
               Assets.generate_reference("demo", [kind: "scene"], server)

      assert {:error, :subject_not_found} =
               Assets.generate_reference("demo", [kind: "scene", name: "无"], server)
    end

    test "generate_reference needs a description, or an explicit prompt", %{server: server} do
      insert_scene(server, "空景", nil)

      assert {:error, :missing_description} =
               Assets.generate_reference("demo", [kind: "scene", name: "空景"], server)

      assert {:ok, reference} =
               Assets.generate_reference(
                 "demo",
                 [kind: "scene", name: "空景", prompt: "自定义场景"],
                 server
               )

      assert reference["kind"] == "scene"
    end
  end

  describe "wardrobe variations" do
    setup do
      root =
        Path.join(System.tmp_dir!(), "toonflow_variations_#{System.unique_integer([:positive])}")

      server = :"toonflow_variations_store_#{System.unique_integer([:positive])}"
      {:ok, pid} = Store.start_link(root_dir: root, name: server)
      assert {:ok, _} = Store.create_project("demo", [], server)

      previous = Application.get_env(:exhub, :toonflow_media_client)
      Application.put_env(:exhub, :toonflow_media_client, Exhub.Toonflow.AssetsTest.StubClient)

      on_exit(fn ->
        if Process.alive?(pid), do: GenServer.stop(pid)
        File.rm_rf(root)

        if previous,
          do: Application.put_env(:exhub, :toonflow_media_client, previous),
          else: Application.delete_env(:exhub, :toonflow_media_client)
      end)

      %{server: server}
    end

    test "generate_variation holds the face and records the look", %{server: server} do
      base = reference_file(server, "base.png")
      insert_character(server, "林昭", "青衫少年", [base])

      assert {:ok, variation} =
               Assets.generate_variation(
                 "demo",
                 [character: "林昭", name: "夜行衣", visual_prompt: "黑色夜行衣"],
                 server
               )

      assert variation["character"] == "林昭"
      assert variation["name"] == "夜行衣"
      assert variation["visual_prompt"] == "黑色夜行衣"
      assert variation["refs"] == [variation["path"]]
      assert File.exists?(variation["path"])
      # The prompt carries the keep-the-face instruction.
      assert File.read!(variation["path"]) =~ "仅更换服装"

      assert {:ok, [listed]} = Assets.list_variations("demo", [character: "林昭"], server)
      assert listed["id"] == variation["variation_id"]

      assert {:ok, variations_by_id} = Assets.variations_map("demo", server)
      assert Map.has_key?(variations_by_id, variation["variation_id"])

      # Re-rendering updates the same variation rather than duplicating it.
      assert {:ok, again} =
               Assets.generate_variation("demo", [character: "林昭", name: "夜行衣"], server)

      assert again["variation_id"] == variation["variation_id"]
      assert {:ok, [_]} = Assets.list_variations("demo", [], server)
    end

    test "generate_variation validates its target", %{server: server} do
      assert {:error, :missing_character} =
               Assets.generate_variation("demo", [name: "夜行衣"], server)

      assert {:error, :missing_name} =
               Assets.generate_variation("demo", [character: "林昭"], server)

      assert {:error, :character_not_found} =
               Assets.generate_variation("demo", [character: "无", name: "夜行衣"], server)
    end

    test "set_shot_variations pins a variation and refs_for_shot honours it", %{server: server} do
      base = reference_file(server, "base.png")
      insert_character(server, "林昭", "青衫少年", [base])
      shot_id = insert_shot(server, 1, ["林昭"])

      assert {:ok, variation} =
               Assets.generate_variation("demo", [character: "林昭", name: "夜行衣"], server)

      assert {:ok, %{"character_variations" => %{"林昭" => variation_id}}} =
               Storyboard.set_shot_variations("demo", shot_id, %{"林昭" => "夜行衣"}, server)

      assert variation_id == variation["variation_id"]

      assert {:ok, shot} = Storyboard.get_shot("demo", shot_id, server)
      assert {:ok, ctx} = context(server)
      assert Media.refs_for_shot(shot, ctx) == [variation["path"]]

      assert {:error, {:variation_not_found, "林昭", "无"}} =
               Storyboard.set_shot_variations("demo", shot_id, %{"林昭" => "无"}, server)

      assert {:ok, %{"character_variations" => %{}}} =
               Storyboard.set_shot_variations("demo", shot_id, %{}, server)
    end
  end

  defp insert_script(server) do
    {:ok, _} =
      Store.run_project(
        "demo",
        fn conn ->
          DB.execute(
            conn,
            "INSERT INTO scripts (id, chapter_id, version, format, content, meta_json, created_at) VALUES (?, ?, ?, ?, ?, ?, ?)",
            ["scr_1", nil, 1, "markdown", "# 第一场\n祖宅，夜。", "{}", "2026-01-01T00:00:00Z"]
          )

          {:ok, "scr_1"}
        end,
        server
      )

    "scr_1"
  end

  defp insert_scene(server, name, description) do
    {:ok, _} =
      Store.run_project(
        "demo",
        fn conn ->
          DB.execute(
            conn,
            "INSERT INTO scenes (id, name, description, refs_json, meta_json) VALUES (?, ?, ?, ?, ?)",
            ["scn_#{name}", name, description, Schema.encode_json([]), nil]
          )

          {:ok, name}
        end,
        server
      )

    name
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

  defp insert_shot(server, idx, characters) do
    id = "sht_#{idx}"

    {:ok, _} =
      Store.run_project(
        "demo",
        fn conn ->
          DB.execute(
            conn,
            """
            INSERT INTO shots (id, script_id, idx, scene, shot_desc, size, camera, lighting, motion, prompt, meta_json)
            VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
            """,
            [
              id,
              "scr_1",
              idx,
              "祖宅",
              "林昭立于门前",
              "中景",
              nil,
              "冷光",
              "推",
              "青衫少年立于门前",
              Schema.encode_json(%{"characters" => characters})
            ]
          )

          {:ok, id}
        end,
        server
      )

    id
  end

  defp context(server) do
    with {:ok, characters} <- Assets.list_characters("demo", [], server),
         {:ok, scenes} <- Assets.list_scenes("demo", [], server),
         {:ok, variations} <- Assets.variations_map("demo", server) do
      {:ok, %{"characters" => characters, "scenes" => scenes, "variations" => variations}}
    end
  end
end

defmodule Exhub.Toonflow.AssetsTest.LLMStub do
  @moduledoc false
  @behaviour Exhub.Toonflow.LLM

  @impl true
  def call_llm(_system, _user, _opts) do
    {:ok,
     ~s({"characters":[{"name":"林昭","role":"主角","appearance":"青衫少年"}],) <>
       ~s("scenes":[{"name":"祖宅","description":"雨夜"}],) <>
       ~s("props":[{"name":"玉佩","description":"信物"}]})}
  end
end

defmodule Exhub.Toonflow.AssetsTest.StubClient do
  @moduledoc false
  @behaviour Exhub.Toonflow.Media.Client

  @impl true
  def generate_image(prompt, opts) do
    path = Keyword.fetch!(opts, :out_path)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, "PNG:" <> prompt)

    {:ok,
     %{
       "path" => path,
       "url" => "stub://" <> prompt,
       "model" => opts[:model],
       "size" => opts[:size]
     }}
  end
end
