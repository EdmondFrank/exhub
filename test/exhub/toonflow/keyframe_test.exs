defmodule Exhub.Toonflow.KeyframeTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{DB, Keyframe, Media, Schema, Store}

  describe "pure helpers" do
    test "normalize_type/1 accepts start/end and rejects anything else" do
      assert Keyframe.normalize_type("start") == "start"
      assert Keyframe.normalize_type(" END ") == "end"
      assert Keyframe.normalize_type("bogus") == nil
      assert Keyframe.normalize_type(nil) == nil
    end

    test "normalize_direction/1 accepts the two continuity directions" do
      assert Keyframe.normalize_direction("prev_end") == "prev_end"
      assert Keyframe.normalize_direction("Next_Start") == "next_start"
      assert Keyframe.normalize_direction("sideways") == nil
      assert Keyframe.normalize_direction(42) == nil
    end

    test "asset_kind/1 prefixes the type" do
      assert Keyframe.asset_kind("start") == "keyframe_start"
      assert Keyframe.asset_kind("end") == "keyframe_end"
    end

    test "keyframe_prompt/2 reads the shot meta and blanks empties" do
      shot = %{"meta" => %{"keyframe_start_prompt" => " 开门 ", "keyframe_end_prompt" => ""}}

      assert Keyframe.keyframe_prompt(shot, "start") == " 开门 "
      assert Keyframe.keyframe_prompt(shot, "end") == nil
      assert Keyframe.keyframe_prompt(%{}, "start") == nil
      assert Keyframe.keyframe_prompt(nil, "start") == nil
    end

    test "fit_argv crops the frame to the delivery aspect" do
      argv = Keyframe.fit_argv("ffmpeg", "/o/k.png", "/o/k.fit.png", "16:9")
      filter = Enum.at(argv, Enum.find_index(argv, &(&1 == "-vf")) + 1)

      # min(iw, ih * 16/9) wide — a 1536x1024 keyframe becomes 1536x864
      assert filter =~ "trunc(min(iw\\,ih*16/9)/2)*2"
      assert filter =~ "trunc(min(ih\\,iw*9/16)/2)*2"
      assert List.last(argv) == "/o/k.fit.png"
    end

    test "fit_argv leaves the frame alone when the aspect is unknown" do
      assert Keyframe.fit_argv("ffmpeg", "/o/k.png", "/o/k.fit.png", nil) ==
               ["ffmpeg", "-y", "-i", "/o/k.png", "/o/k.fit.png"]

      assert Keyframe.fit_argv("ffmpeg", "/o/k.png", "/o/k.fit.png", "adaptive") ==
               ["ffmpeg", "-y", "-i", "/o/k.png", "/o/k.fit.png"]
    end

    test "adjacent/2 returns the neighbouring shots" do
      shots = [%{"id" => "a"}, %{"id" => "b"}, %{"id" => "c"}]

      assert {nil, %{"id" => "b"}} = Keyframe.adjacent(shots, "a")
      assert {%{"id" => "a"}, %{"id" => "c"}} = Keyframe.adjacent(shots, "b")
      assert {%{"id" => "b"}, nil} = Keyframe.adjacent(shots, "c")
      assert {nil, nil} = Keyframe.adjacent(shots, "missing")
    end

    test "continuity_attrs/3 relabels the source frame" do
      source = %{"id" => "ast_1", "prompt" => "青衫少年", "meta" => %{"model" => "m"}}

      attrs = Keyframe.continuity_attrs(source, "keyframe_start", "/tmp/x.png")

      assert attrs["kind"] == "keyframe_start"
      assert attrs["path"] == "/tmp/x.png"
      assert attrs["prompt"] == "青衫少年"
      assert attrs["meta"]["copied_from"] == "ast_1"
      assert attrs["meta"]["model"] == "m"
    end
  end

  describe "generation & continuity" do
    setup do
      root =
        Path.join(System.tmp_dir!(), "toonflow_keyframe_#{System.unique_integer([:positive])}")

      server = :"toonflow_keyframe_store_#{System.unique_integer([:positive])}"
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

    test "generate/3 records a keyframe asset with its own filename", %{server: server} do
      shot_id = insert_shot(server, 1, %{"keyframe_start_prompt" => "门前远景"})

      assert {:ok, asset} = Keyframe.generate("demo", [shot_id: shot_id, type: "start"], server)
      assert asset["kind"] == "keyframe_start"
      # The shot's 景别 reaches the keyframe prompt so the pair that drives the
      # `fl2va` interpolation is composed at a consistent scale.
      assert asset["prompt"] =~ "门前远景"
      assert asset["prompt"] =~ "中景：人物膝盖以上入画"
      assert asset["path"] =~ "/assets/images/#{shot_id}_start.png"
      assert File.exists?(asset["path"])
      assert asset["meta"]["status"] == "completed"
    end

    test "generate/3 falls back to the shot prompt when meta has none", %{server: server} do
      shot_id = insert_shot(server, 1, %{})

      assert {:ok, asset} = Keyframe.generate("demo", [shot_id: shot_id, type: "start"], server)
      assert asset["kind"] == "keyframe_start"
      assert asset["prompt"] =~ "青衫少年立于门前"
    end

    test "generate/3 validates its target", %{server: server} do
      shot_id = insert_shot(server, 1, %{})

      assert {:error, :missing_shot_id} = Keyframe.generate("demo", [type: "start"], server)

      assert {:error, :invalid_keyframe_type} =
               Keyframe.generate("demo", [shot_id: shot_id, type: "sideways"], server)

      assert {:error, :shot_not_found} =
               Keyframe.generate("demo", [shot_id: "nope", type: "start"], server)
    end

    test "list_for_shot/3 reports each frame independently", %{server: server} do
      shot_id = insert_shot(server, 1, %{})

      assert {:ok, %{"start" => nil, "end" => nil}} =
               Keyframe.list_for_shot("demo", shot_id, server)

      assert {:ok, _} = Keyframe.generate("demo", [shot_id: shot_id, type: "start"], server)

      assert {:ok, %{"start" => start, "end" => nil}} =
               Keyframe.list_for_shot("demo", shot_id, server)

      assert start["kind"] == "keyframe_start"
    end

    test "copy/3 duplicates the previous shot's end frame as this shot's start", %{
      server: server
    } do
      first = insert_shot(server, 1, %{})
      second = insert_shot(server, 2, %{})

      assert {:ok, source} = Keyframe.generate("demo", [shot_id: first, type: "end"], server)

      assert {:ok, copied} =
               Keyframe.copy("demo", [shot_id: second, direction: "prev_end"], server)

      assert copied["kind"] == "keyframe_start"
      assert copied["path"] =~ "/assets/images/#{second}_start.png"
      assert copied["prompt"] == source["prompt"]
      assert copied["meta"]["copied_from"] == source["asset_id"]
      assert File.read!(copied["path"]) == File.read!(source["path"])
    end

    test "copy/3 errors when the neighbour has no frame", %{server: server} do
      first = insert_shot(server, 1, %{})
      second = insert_shot(server, 2, %{})

      assert {:error, :no_source_keyframe} =
               Keyframe.copy("demo", [shot_id: second, direction: "prev_end"], server)

      assert {:error, :no_adjacent_shot} =
               Keyframe.copy("demo", [shot_id: first, direction: "prev_end"], server)

      assert {:error, :invalid_direction} =
               Keyframe.copy("demo", [shot_id: second, direction: "up"], server)
    end

    test "set_prompt/5 updates the shot meta and survives a round trip", %{server: server} do
      shot_id = insert_shot(server, 1, %{})

      assert {:ok, %{"type" => "end", "prompt" => "族人怒视"}} =
               Keyframe.set_prompt("demo", shot_id, "end", "族人怒视", server)

      assert {:ok, shot} = Exhub.Toonflow.Storyboard.get_shot("demo", shot_id, server)
      assert Keyframe.keyframe_prompt(shot, "end") == "族人怒视"

      # Setting a blank prompt clears the key.
      assert {:ok, %{"prompt" => nil}} =
               Keyframe.set_prompt("demo", shot_id, "end", "", server)

      assert {:ok, shot} = Exhub.Toonflow.Storyboard.get_shot("demo", shot_id, server)
      assert Keyframe.keyframe_prompt(shot, "end") == nil
    end

    test "previous_shot/3 and next_shot/3 walk the script", %{server: server} do
      first = insert_shot(server, 1, %{})
      second = insert_shot(server, 2, %{})

      assert {:ok, nil} = Keyframe.previous_shot("demo", first, server)
      assert {:ok, %{"id" => ^second}} = Keyframe.next_shot("demo", first, server)
      assert {:ok, %{"id" => ^first}} = Keyframe.previous_shot("demo", second, server)
      assert {:ok, nil} = Keyframe.next_shot("demo", second, server)
    end
  end

  describe "select_candidate/3" do
    setup do
      root = Path.join(System.tmp_dir!(), "toonflow_cand_#{System.unique_integer([:positive])}")
      server = :"toonflow_cand_store_#{System.unique_integer([:positive])}"
      {:ok, pid} = Store.start_link(root_dir: root, name: server)
      assert {:ok, _} = Store.create_project("demo", [], server)

      previous = Application.get_env(:exhub, :toonflow_media_client)
      Application.put_env(:exhub, :toonflow_media_client, Exhub.Toonflow.KeyframeTest.StubClient)

      on_exit(fn ->
        if Process.alive?(pid), do: GenServer.stop(pid)
        File.rm_rf(root)

        if previous,
          do: Application.put_env(:exhub, :toonflow_media_client, previous),
          else: Application.delete_env(:exhub, :toonflow_media_client)
      end)

      %{server: server}
    end

    test "normalize_cell/1 accepts whole and numeric strings" do
      assert Keyframe.normalize_cell(nil) == nil
      assert Keyframe.normalize_cell("whole") == nil
      assert Keyframe.normalize_cell(" WHOLE ") == nil
      assert Keyframe.normalize_cell(" 4 ") == 4
      assert Keyframe.normalize_cell(7) == 7
      assert Keyframe.normalize_cell("nope") == nil
    end

    test "promotes the whole grid when no cell is given", %{server: server} do
      shot_id = insert_shot(server, 1, %{})
      assert {:ok, _} = Media.generate_candidates("demo", [shot_id: shot_id], server)

      assert {:ok, asset} =
               Keyframe.select_candidate("demo", [shot_id: shot_id, type: "start"], server)

      assert asset["kind"] == "keyframe_start"
      assert asset["path"] =~ "/assets/images/#{shot_id}_start.png"
      assert asset["meta"]["cell"] == nil
      assert asset["meta"]["grid"] == %{"cols" => 3, "rows" => 3}
      assert asset["meta"]["status"] == "completed"

      # It is now the shot's start frame, so `videos` will pick it up.
      assert {:ok, %{"start" => start}} = Keyframe.list_for_shot("demo", shot_id, server)
      assert start["id"] == asset["asset_id"]
    end

    test "validates its target and the grid", %{server: server} do
      shot_id = insert_shot(server, 1, %{})

      assert {:error, :missing_shot_id} =
               Keyframe.select_candidate("demo", [type: "start"], server)

      assert {:error, :invalid_keyframe_type} =
               Keyframe.select_candidate("demo", [shot_id: shot_id, type: "sideways"], server)

      assert {:error, :no_candidates} =
               Keyframe.select_candidate("demo", [shot_id: shot_id, type: "start"], server)

      assert {:error, :candidate_not_found} =
               Keyframe.select_candidate(
                 "demo",
                 [shot_id: shot_id, type: "start", candidates_id: "nope"],
                 server
               )
    end

    test "rejects a cell outside the grid", %{server: server} do
      shot_id = insert_shot(server, 1, %{})
      assert {:ok, _} = Media.generate_candidates("demo", [shot_id: shot_id], server)

      assert {:error, {:invalid_cell, 9, 3, 3}} =
               Keyframe.select_candidate(
                 "demo",
                 [shot_id: shot_id, type: "start", cell: "9"],
                 server
               )
    end
  end

  defp insert_shot(server, idx, meta) do
    id = "sht_test#{idx}"

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
              "林昭立于门前",
              "中景",
              nil,
              "冷光",
              "推",
              "青衫少年立于门前",
              Schema.encode_json(meta)
            ]
          )

          {:ok, id}
        end,
        server
      )

    id
  end
end

defmodule Exhub.Toonflow.KeyframeTest.StubClient do
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
