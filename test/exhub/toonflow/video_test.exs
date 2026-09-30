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
