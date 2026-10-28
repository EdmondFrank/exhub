defmodule Exhub.Toonflow.RendererTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{DB, Renderer, Schema, Store}

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

  # `Voice.generate_voice/3` answers `{:skip, :no_dialogue}` for a shot with no
  # line. `report/5` used to have no clause for it, so the re-render task died
  # with a `CaseClauseError` — no terminal event, and the canvas' badge ticked
  # on until the next prune.
  test "a voice re-render with nothing to voice reports a skip, not a crash" do
    root = Path.join(System.tmp_dir!(), "toonflow_render_#{System.unique_integer([:positive])}")
    server = :"toonflow_render_store_#{System.unique_integer([:positive])}"
    {:ok, pid} = Store.start_link(root_dir: root, name: server)

    previous_tf = Application.get_env(:exhub, :toonflow)
    Application.put_env(:exhub, :toonflow, %{"media" => %{"tts_narration" => false}})

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf(root)

      if previous_tf,
        do: Application.put_env(:exhub, :toonflow, previous_tf),
        else: Application.delete_env(:exhub, :toonflow)
    end)

    assert {:ok, _} = Store.create_project("demo", [], server)
    assert :ok = insert_shot("demo", "sht_1", server)

    assert {:ok, results} = Renderer.run("demo", "sht_1", "voice", server: server)
    assert results == %{"voice" => {:skip, :no_dialogue}}
  end

  defp insert_shot(project, id, server) do
    Store.run_project(
      project,
      fn conn ->
        DB.execute(
          conn,
          "INSERT INTO shots (id, script_id, idx, scene, shot_desc, size, camera, lighting, motion, prompt, meta_json) " <>
            "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
          [
            id,
            "scr_1",
            1,
            "祖宅",
            "林昭立于门前",
            "中景",
            nil,
            nil,
            nil,
            "p",
            Schema.encode_json(%{})
          ]
        )
      end,
      server
    )
  end
end
