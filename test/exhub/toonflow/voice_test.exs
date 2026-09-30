defmodule Exhub.Toonflow.VoiceTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{DB, Schema, Store, Voice}

  setup do
    root = Path.join(System.tmp_dir!(), "toonflow_voice_#{System.unique_integer([:positive])}")
    server = :"toonflow_voice_store_#{System.unique_integer([:positive])}"
    {:ok, pid} = Store.start_link(root_dir: root, name: server)

    previous = Application.get_env(:exhub, :toonflow_voice_client)
    Application.put_env(:exhub, :toonflow_voice_client, Exhub.Toonflow.VoiceTest.StubClient)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf(root)

      if previous,
        do: Application.put_env(:exhub, :toonflow_voice_client, previous),
        else: Application.delete_env(:exhub, :toonflow_voice_client)
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

  test "passes the CosyVoice2 / alloy defaults through to the client", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)

    assert {:ok, asset} = Voice.generate_voice("demo", [text: "你好"], server)
    assert asset["meta"]["model"] == "CosyVoice2"
    assert asset["meta"]["voice"] == "alloy"
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
       "output_format" => opts[:output_format],
       "segments" => 1
     }}
  end
end
