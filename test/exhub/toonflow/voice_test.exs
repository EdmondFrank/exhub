defmodule Exhub.Toonflow.VoiceTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{DB, Schema, Store, Voice}

  setup do
    root = Path.join(System.tmp_dir!(), "toonflow_voice_#{System.unique_integer([:positive])}")
    server = :"toonflow_voice_store_#{System.unique_integer([:positive])}"
    {:ok, pid} = Store.start_link(root_dir: root, name: server)

    previous = Application.get_env(:exhub, :toonflow_voice_client)
    previous_tf = Application.get_env(:exhub, :toonflow)
    Application.put_env(:exhub, :toonflow_voice_client, Exhub.Toonflow.VoiceTest.StubClient)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf(root)

      if previous,
        do: Application.put_env(:exhub, :toonflow_voice_client, previous),
        else: Application.delete_env(:exhub, :toonflow_voice_client)

      if previous_tf,
        do: Application.put_env(:exhub, :toonflow, previous_tf),
        else: Application.delete_env(:exhub, :toonflow)
    end)

    %{server: server}
  end

  test "voice_path picks the extension from the format (pure)" do
    assert Voice.voice_path("/tmp/p", "sht_1", "mp3") == "/tmp/p/assets/audio/sht_1.mp3"
    assert Voice.voice_path("/tmp/p", "sht_1", "wav") == "/tmp/p/assets/audio/sht_1.wav"
    assert Voice.voice_path("/tmp/p", "a/b", nil) == "/tmp/p/assets/audio/a_b.mp3"
  end

  test "explicit text records an audio asset", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)

    assert {:ok, asset} = Voice.generate_voice("demo", [text: "你好"], server)
    assert asset["kind"] == "audio"
    assert asset["url"] == "stub://你好"
    assert asset["path"] =~ "/assets/audio/"
    assert File.exists?(asset["path"])
  end

  test "passes the configured TeleTTS defaults through to the client", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)

    assert {:ok, asset} = Voice.generate_voice("demo", [text: "你好"], server)
    assert asset["meta"]["model"] == "TeleTTS-Bidirectional-Mandarin"
    assert asset["meta"]["voice"] == "guangzhounv"
  end

  test "resolves the dialogue from a shot's meta", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)
    assert :ok = insert_shot("demo", "sht_1", %{"dialogue" => "我不走"}, server)

    assert {:ok, asset} = Voice.generate_voice("demo", [shot_id: "sht_1"], server)
    assert asset["prompt"] == "我不走"
  end

  test "no text or shot errors", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)
    assert {:error, :missing_text} = Voice.generate_voice("demo", [], server)
  end

  test "propagates client errors", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)
    assert {:error, {:voice_failed, :boom}} = Voice.generate_voice("demo", [text: "boom"], server)
  end

  describe "per-character voices" do
    test "meta.speaker picks the mapped voice and tuning", %{server: server} do
      put_toonflow(%{
        "media" => %{"voice_map" => %{"鸣人" => %{"voice" => "lucas", "speech_rate" => 1.05}}}
      })

      assert {:ok, _} = Store.create_project("demo", [], server)

      assert :ok =
               insert_shot("demo", "sht_1", %{"dialogue" => "我不走", "speaker" => "鸣人"}, server)

      assert {:ok, asset} = Voice.generate_voice("demo", [shot_id: "sht_1"], server)
      assert asset["meta"]["voice"] == "lucas"
      assert asset["meta"]["speech_rate"] == 1.05
      assert asset["meta"]["speaker"] == "鸣人"
    end

    test "a string voice_map entry is just a voice name", %{server: server} do
      put_toonflow(%{"media" => %{"voice_map" => %{"老盐" => "shanghainv"}}})
      assert {:ok, _} = Store.create_project("demo", [], server)
      assert :ok = insert_shot("demo", "sht_1", %{"dialogue" => "哎", "speaker" => "老盐"}, server)

      assert {:ok, asset} = Voice.generate_voice("demo", [shot_id: "sht_1"], server)
      assert asset["meta"]["voice"] == "shanghainv"
    end

    test "infers the speaker from a shot's only character", %{server: server} do
      put_toonflow(%{"media" => %{"voice_map" => %{"小铃" => "dongbeinv"}}})
      assert {:ok, _} = Store.create_project("demo", [], server)

      assert :ok =
               insert_shot(
                 "demo",
                 "sht_1",
                 %{"dialogue" => "姐姐", "characters" => ["小铃"]},
                 server
               )

      assert {:ok, asset} = Voice.generate_voice("demo", [shot_id: "sht_1"], server)
      assert asset["meta"]["voice"] == "dongbeinv"
      assert asset["meta"]["speaker"] == "小铃"
    end

    test "an explicit voice wins over the voice map", %{server: server} do
      put_toonflow(%{"media" => %{"voice_map" => %{"小铃" => "dongbeinv"}}})
      assert {:ok, _} = Store.create_project("demo", [], server)
      assert :ok = insert_shot("demo", "sht_1", %{"dialogue" => "嗨", "speaker" => "小铃"}, server)

      assert {:ok, asset} =
               Voice.generate_voice("demo", [shot_id: "sht_1", voice: "lucas"], server)

      assert asset["meta"]["voice"] == "lucas"
    end

    test "an unmapped speaker falls back to media.tts_voice", %{server: server} do
      put_toonflow(%{"media" => %{"voice_map" => %{"小铃" => "dongbeinv"}}})
      assert {:ok, _} = Store.create_project("demo", [], server)
      assert :ok = insert_shot("demo", "sht_1", %{"dialogue" => "喂", "speaker" => "路人"}, server)

      assert {:ok, asset} = Voice.generate_voice("demo", [shot_id: "sht_1"], server)
      assert asset["meta"]["voice"] == "guangzhounv"
    end
  end

  describe "narration" do
    test "a shot with no dialogue is skipped", %{server: server} do
      assert {:ok, _} = Store.create_project("demo", [], server)
      assert :ok = insert_shot("demo", "sht_1", %{}, server)

      assert {:skip, :no_dialogue} = Voice.generate_voice("demo", [shot_id: "sht_1"], server)
    end

    test "tts_narration voices the shot description instead", %{server: server} do
      put_toonflow(%{"media" => %{"tts_narration" => true}})
      assert {:ok, _} = Store.create_project("demo", [], server)
      assert :ok = insert_shot("demo", "sht_1", %{}, server)

      assert {:ok, asset} = Voice.generate_voice("demo", [shot_id: "sht_1"], server)
      assert asset["prompt"] == "林昭立于门前"
    end
  end

  defp put_toonflow(config), do: Application.put_env(:exhub, :toonflow, config)

  defp insert_shot(project, id, meta, server) do
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
            Schema.encode_json(meta)
          ]
        )
      end,
      server
    )
  end
end

defmodule Exhub.Toonflow.VoiceTest.StubClient do
  @moduledoc false
  @behaviour Exhub.Toonflow.Voice.Client

  @impl true
  def generate_voice("boom", _opts), do: {:error, :boom}

  def generate_voice(text, opts) do
    path = Keyword.fetch!(opts, :out_path)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, "AUDIO")

    {:ok,
     %{
       "path" => path,
       "url" => "stub://" <> text,
       "model" => opts[:model],
       "voice" => opts[:voice],
       "speaker" => opts[:speaker],
       "speech_rate" => opts[:speech_rate],
       "volume" => opts[:volume],
       "output_format" => opts[:output_format],
       "segments" => 1
     }
     |> Enum.reject(fn {_key, value} -> is_nil(value) end)
     |> Map.new()}
  end
end
