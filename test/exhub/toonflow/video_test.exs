defmodule Exhub.Toonflow.VideoTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{Store, Video}

  setup do
    root = Path.join(System.tmp_dir!(), "toonflow_video_#{System.unique_integer([:positive])}")
    server = :"toonflow_video_store_#{System.unique_integer([:positive])}"
    {:ok, pid} = Store.start_link(root_dir: root, name: server)

    previous = Application.get_env(:exhub, :toonflow_video_client)
    Application.put_env(:exhub, :toonflow_video_client, Exhub.Toonflow.VideoTest.StubClient)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf(root)

      if previous,
        do: Application.put_env(:exhub, :toonflow_video_client, previous),
        else: Application.delete_env(:exhub, :toonflow_video_client)
    end)

    %{server: server}
  end

  test "video_path is project-local and sanitized" do
    assert Video.video_path("/tmp/p", "sht_1") == "/tmp/p/assets/videos/sht_1.mp4"
    assert Video.video_path("/tmp/p", "a/b") == "/tmp/p/assets/videos/a_b.mp4"
  end

  test "from a free prompt records a clip (default task t2va)", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)

    assert {:ok, asset} = Video.generate_video("demo", [prompt: "一只猫"], server)
    assert asset["kind"] == "video"
    assert asset["url"] == "stub://t2va"
    assert asset["prompt"] == "一只猫"
    assert asset["path"] =~ "/assets/videos/"
    assert File.exists?(asset["path"])
    assert asset["meta"]["task"] == "t2va"
  end

  test "explicit fl2va requires a first frame", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)

    assert {:error, :missing_first_frame} =
             Video.generate_video("demo", [prompt: "x", task: "fl2va"], server)

    assert {:ok, asset} =
             Video.generate_video(
               "demo",
               [prompt: "x", task: "fl2va", first_frame: "/tmp/frame.png"],
               server
             )

    assert asset["url"] == "stub://fl2va"
  end

  test "no prompt or shot errors", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)
    assert {:error, :missing_prompt} = Video.generate_video("demo", [], server)
  end

  test "propagates client errors", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)

    assert {:error, {:video_failed, :boom}} =
             Video.generate_video("demo", [prompt: "boom"], server)
  end

  describe "clip_duration/1" do
    test "follows the line, not a fixed length" do
      # a 1 s line no longer gets a 6 s clip of dead air
      assert Video.clip_duration(0.87) == 4
      # a 7.7 s line is no longer truncated by a 6 s clip
      assert Video.clip_duration(7.74) == 9
    end

    test "clamps to what MiniMax-H3 accepts" do
      assert Video.clip_duration(0) == 4
      assert Video.clip_duration(20) == 15
    end
  end

  describe "probe_seconds/1" do
    test "reads a real file's duration" do
      with ffmpeg when is_binary(ffmpeg) <- System.find_executable("ffmpeg"),
           ffprobe when is_binary(ffprobe) <- System.find_executable("ffprobe") do
        dir = Path.join(System.tmp_dir!(), "toonflow_probe_#{System.unique_integer([:positive])}")
        File.mkdir_p!(dir)
        on_exit(fn -> File.rm_rf(dir) end)
        wav = Path.join(dir, "half.wav")

        {_, 0} =
          System.cmd(
            ffmpeg,
            ~w(-y -f lavfi -i anullsrc=r=16000:cl=mono -t 0.5) ++ [wav],
            stderr_to_stdout: true
          )

        assert {:ok, seconds} = Video.probe_seconds(wav)
        assert_in_delta seconds, 0.5, 0.1
      else
        _ -> :ok
      end
    end

    test "is :error for a file it cannot read" do
      assert :error = Video.probe_seconds("/nonexistent/nope.wav")
    end
  end

  describe "dialogue_seconds/1" do
    test "estimates Mandarin speech from the character count" do
      assert Video.dialogue_seconds("他救不活了。") > 1.0
      assert Video.dialogue_seconds("我从来不丢下同伴！") > Video.dialogue_seconds("姐姐……")
    end

    test "is zero without dialogue" do
      assert Video.dialogue_seconds(nil) == 0.0
      assert Video.dialogue_seconds("") == 0.0
    end
  end

  describe "poll_opts/1" do
    test "defaults to a budget long enough for a MiniMax-H3 clip" do
      assert Video.Default.poll_opts() == [interval_ms: 10_000, max_attempts: 110]
    end

    test "reads the media overrides" do
      config = %{"video_poll_interval_ms" => 5_000, "video_poll_attempts" => 3}
      assert Video.Default.poll_opts(config) == [interval_ms: 5_000, max_attempts: 3]
    end
  end
end

defmodule Exhub.Toonflow.VideoTest.StubClient do
  @moduledoc false
  @behaviour Exhub.Toonflow.Video.Client

  @impl true
  def generate_video("boom", _opts), do: {:error, :boom}

  def generate_video(prompt, opts) do
    path = Keyword.fetch!(opts, :out_path)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, "MP4")

    {:ok,
     %{
       "path" => path,
       "url" => "stub://" <> to_string(opts[:task]),
       "model" => opts[:model],
       "task" => opts[:task],
       "task_id" => "t1",
       "duration_seconds" => 6,
       "aspect_ratio" => "16:9",
       "prompt" => prompt
     }}
  end
end
