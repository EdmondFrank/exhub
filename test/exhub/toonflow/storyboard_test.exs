defmodule Exhub.Toonflow.StoryboardTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{DB, Media, Schema, Store, Storyboard}

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

    test "keeps the speaker (and the dialogue) in meta" do
      raw = ~s({"shots":[{"shot_desc":"林昭开口","dialogue":"我不走","speaker":"林昭"}]})

      assert {:ok, [shot]} = Storyboard.parse_storyboard(raw)
      assert shot["meta"]["speaker"] == "林昭"
      assert shot["meta"]["dialogue"] == "我不走"
    end

    test "errors when no shots are present" do
      assert {:error, {:missing, "shots"}} = Storyboard.parse_storyboard(~s({"foo":[]}))
      assert {:error, :invalid_payload} = Storyboard.parse_storyboard(nil)
    end
  end

  describe "characters_of/1" do
    test "reads the names from the top level or from meta" do
      assert Storyboard.characters_of(%{"characters" => ["林昭"]}) == ["林昭"]

      # `insert_shot/3` stores them in meta, so DB-loaded shots land here.
      assert Storyboard.characters_of(%{"meta" => %{"characters" => ["林昭"]}}) == ["林昭"]
      assert Storyboard.characters_of(%{}) == []
      assert Storyboard.characters_of(nil) == []
    end

    test "shot_prompt/2 still appends appearances for a DB-loaded shot" do
      shot = %{"prompt" => "青衫少年", "meta" => %{"characters" => ["林昭"]}}
      characters = [%{"name" => "林昭", "appearance" => "青衫少年"}]

      assert Storyboard.shot_prompt(shot, characters) == "青衫少年；林昭（青衫少年）"
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

      # The bare 推 is bounded, so the model cannot read it as a whole-scene zoom.
      assert Storyboard.shot_prompt(shot) ==
               "祖宅，林昭立于门前，中景，推，冷光；运镜：缓慢推近，幅度不超过画面的5%，人物大小基本不变"
    end

    test "appends the scene description when one is known" do
      shot = %{"prompt" => "青衫少年立于门前", "scene" => "祖宅", "characters" => []}
      scenes = [%{"name" => "祖宅", "description" => "雨夜庭院"}]

      assert Storyboard.shot_prompt(shot, [], scenes) == "青衫少年立于门前；场景：雨夜庭院"
    end

    test "prefers an explicit prompt and appends character appearances" do
      shot = %{"prompt" => "青衫少年立于门前", "characters" => ["林昭", "无名"]}
      characters = [%{"name" => "林昭", "appearance" => "青衫少年"}]

      assert Storyboard.shot_prompt(shot, characters) == "青衫少年立于门前；林昭（青衫少年）"
    end

    test "appends the 景别 lens, camera move and lighting when the prompt lacks them" do
      shot = %{
        "prompt" => "90赛璐璐，夜雾甲板，金发女与忍装少年对立",
        "size" => "中景",
        "motion" => "推",
        "lighting" => "青灰橙光"
      }

      prompt = Storyboard.shot_prompt(shot)
      assert prompt =~ "中景：人物膝盖以上入画"
      assert prompt =~ "运镜：缓慢推近，幅度不超过画面的5%"
      assert prompt =~ "光线：青灰橙光"
    end

    test "an explicit base keeps its own wording and gains the framing notes" do
      shot = %{"size" => "全景", "lighting" => "冷光"}

      prompt = Storyboard.shot_prompt(shot, [], [], base: "老盐举油灯", lock_framing: true)
      assert prompt =~ "老盐举油灯"
      assert prompt =~ "全景：人物全身入画"
      assert prompt =~ "光线：冷光"
      assert prompt =~ "禁止变焦"
    end

    test "lock_framing swaps the camera move for a fixed-camera clause" do
      shot = %{"prompt" => "夜雾甲板", "size" => "近景", "motion" => "推"}

      prompt = Storyboard.shot_prompt(shot, [], [], lock_framing: true)
      assert prompt =~ "近景：人物胸部以上入画"
      refute prompt =~ "运镜："
      assert prompt =~ "禁止变焦"
    end

    test "a 固定 运镜 renders the fixed-camera clause without lock_framing" do
      shot = %{"prompt" => "夜雾甲板", "motion" => "固定"}

      prompt = Storyboard.shot_prompt(shot)
      assert prompt =~ "固定机位一镜到底"
      refute prompt =~ "运镜："
    end
  end

  describe "fixed_camera?/1" do
    test "treats 固定/静止 and a missing 运镜 as a fixed camera" do
      assert Storyboard.fixed_camera?(%{"motion" => "固定"})
      assert Storyboard.fixed_camera?(%{"motion" => "静止不动"})
      assert Storyboard.fixed_camera?(%{})
      assert Storyboard.fixed_camera?(nil)
    end

    test "recognises a camera move" do
      refute Storyboard.fixed_camera?(%{"motion" => "推"})
      refute Storyboard.fixed_camera?(%{"motion" => "摇"})
    end
  end

  describe "put_shot_meta/4" do
    setup do
      root = Path.join(System.tmp_dir!(), "toonflow_meta_#{System.unique_integer([:positive])}")
      server = :"toonflow_meta_store_#{System.unique_integer([:positive])}"
      {:ok, pid} = Store.start_link(root_dir: root, name: server)
      assert {:ok, _} = Store.create_project("demo", [], server)

      on_exit(fn ->
        if Process.alive?(pid), do: GenServer.stop(pid)
        File.rm_rf(root)
      end)

      %{server: server}
    end

    test "merges a patch, and a nil value deletes the key", %{server: server} do
      shot_id = insert_shot(server, 1, %{"dialogue" => "我不走"})

      assert {:ok, %{"meta" => meta}} =
               Storyboard.put_shot_meta("demo", shot_id, %{"speaker" => "林昭"}, server)

      assert meta["speaker"] == "林昭"
      assert meta["dialogue"] == "我不走"

      assert {:ok, _} = Storyboard.put_shot_meta("demo", shot_id, %{"speaker" => nil}, server)

      assert {:ok, shot} = Storyboard.get_shot("demo", shot_id, server)
      assert Map.get(shot["meta"], "speaker") == nil
      assert shot["meta"]["dialogue"] == "我不走"
    end

    test "errors for an unknown shot", %{server: server} do
      assert {:error, :shot_not_found} = Storyboard.put_shot_meta("demo", "nope", %{}, server)
    end
  end

  describe "split_shot/3" do
    setup do
      root = Path.join(System.tmp_dir!(), "toonflow_split_#{System.unique_integer([:positive])}")
      server = :"toonflow_split_store_#{System.unique_integer([:positive])}"
      {:ok, pid} = Store.start_link(root_dir: root, name: server)
      assert {:ok, _} = Store.create_project("demo", [], server)

      previous = Application.get_env(:exhub, :toonflow_llm)
      Application.put_env(:exhub, :toonflow_llm, __MODULE__.SplitLLM)

      on_exit(fn ->
        if Process.alive?(pid), do: GenServer.stop(pid)
        File.rm_rf(root)

        if previous,
          do: Application.put_env(:exhub, :toonflow_llm, previous),
          else: Application.delete_env(:exhub, :toonflow_llm)
      end)

      %{server: server}
    end

    test "replaces a shot with sub-shots in place and resequences", %{server: server} do
      first = insert_shot(server, 1, %{})

      target =
        insert_shot(server, 2, %{
          "keyframe_start_prompt" => "开门首帧",
          "keyframe_end_prompt" => "门内尾帧"
        })

      insert_shot(server, 3, %{})

      # The replaced shot's generated assets go away with it.
      assert {:ok, _} =
               Media.insert_asset(
                 "demo",
                 [shot_id: target, kind: "image", path: "/tmp/x.png"],
                 server
               )

      assert {:ok, summary} = Storyboard.split_shot("demo", [shot_id: target], server)
      assert summary["shot_id"] == target
      assert summary["sub_shots"] == 2
      assert length(summary["created"]) == 2

      assert {:ok, shots} = Storyboard.list_shots("demo", [], server)
      assert Enum.map(shots, & &1["idx"]) == [1, 2, 3, 4]
      assert Enum.map(shots, & &1["id"]) |> Enum.member?(target) == false
      assert Enum.at(shots, 0)["id"] == first

      [_head, a, b, _last] = shots

      # Inherited scene, cast and the outer keyframe anchors.
      assert a["scene"] == "祖宅"
      assert Storyboard.characters_of(a) == ["林昭"]
      assert a["shot_desc"] == "林昭推门"
      assert a["meta"]["visual_focus"] == "手推门"
      assert a["meta"]["keyframe_start_prompt"] == "开门首帧"
      assert b["meta"]["keyframe_end_prompt"] == "门内尾帧"
      assert Map.get(a["meta"], "keyframe_end_prompt") == nil
      assert Map.get(b["meta"], "keyframe_start_prompt") == nil

      assert {:error, :shot_not_found} = Storyboard.get_shot("demo", target, server)
      assert {:ok, []} = Media.list_assets("demo", [shot_id: target], server)
    end

    test "validates its target", %{server: server} do
      assert {:error, :missing_shot_id} = Storyboard.split_shot("demo", [], server)
      assert {:error, :shot_not_found} = Storyboard.split_shot("demo", [shot_id: "nope"], server)
    end
  end

  defp insert_shot(server, idx, meta) do
    id = "sht_split#{idx}"

    {:ok, _id} =
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
              "林昭推门而入",
              "中景",
              nil,
              "冷光",
              "推",
              "青衫少年立于门前",
              Schema.encode_json(Map.put(meta, "characters", ["林昭"]))
            ]
          )

          {:ok, id}
        end,
        server
      )

    id
  end
end

defmodule Exhub.Toonflow.StoryboardTest.SplitLLM do
  @moduledoc false
  @behaviour Exhub.Toonflow.LLM

  @impl true
  def call_llm(_system, _user, _opts) do
    {:ok,
     ~s({"shots":[{"shot_desc":"林昭推门","size":"中景","motion":"推","visual_focus":"手推门","prompt":"门前"},) <>
       ~s({"shot_desc":"门缓缓打开","size":"近景","motion":"移","visual_focus":"门缝","prompt":"门内"}]})}
  end
end
