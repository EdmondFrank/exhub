defmodule Exhub.Toonflow.PipelineTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{
    Assets,
    Events,
    Media,
    Memory,
    Novel,
    Pipeline,
    Script,
    Store,
    Storyboard,
    Video,
    Voice
  }

  setup do
    root = Path.join(System.tmp_dir!(), "toonflow_test_#{System.unique_integer([:positive])}")
    server = :"toonflow_test_store_#{System.unique_integer([:positive])}"

    {:ok, pid} = Store.start_link(root_dir: root, name: server)

    previous = Application.get_env(:exhub, :toonflow_llm)
    Application.put_env(:exhub, :toonflow_llm, Exhub.Toonflow.PipelineTest.LLMStub)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf(root)

      if previous,
        do: Application.put_env(:exhub, :toonflow_llm, previous),
        else: Application.delete_env(:exhub, :toonflow_llm)
    end)

    %{server: server}
  end

  test "novel → chapters → events → script → new version", %{server: server} do
    assert {:ok, _} = Store.create_project("demo", [], server)

    text = """
    第一章 风雨欲来
    林昭站在祖宅门前，天色阴沉。

    第二章 逐出
    族长宣布将林昭逐出家门。
    """

    assert {:ok, novel} = Novel.add_novel("demo", [text: text, title: "风雨"], server)
    assert novel["chapter_count"] == 2
    assert novel["novel_id"] =~ "nov_"

    assert {:ok, chapters} = Novel.list_chapters("demo", [], server)
    assert Enum.map(chapters, & &1["title"]) == ["第一章 风雨欲来", "第二章 逐出"]
    assert Enum.all?(chapters, &(&1["text_length"] > 0))

    first = hd(chapters)

    assert {:ok, summary} = Events.extract_events("demo", [chapter_id: first["id"]], server)
    assert summary["event_count"] == 1
    assert summary["errors"] == []

    assert {:ok, [event]} = Events.list_events("demo", [], server)
    assert event["kind"] == "conflict"
    assert event["chapter_id"] == first["id"]

    assert {:ok, v1} = Script.generate_script("demo", [chapter_id: first["id"]], server)
    assert v1["version"] == 1
    assert v1["content"] =~ "林昭"

    assert {:ok, v2} =
             Script.update_script(
               "demo",
               [script_id: v1["script_id"], content: "# 改", note: "trim"],
               server
             )

    assert v2["version"] == 2

    assert {:ok, got} = Script.get_script("demo", [script_id: v1["script_id"]], server)
    assert got["content"] == v1["content"]

    assert {:ok, latest} = Script.get_script("demo", [chapter_id: first["id"]], server)
    assert latest["version"] == 2

    assert {:ok, note} = Memory.add_note("demo", [kind: "style", title: "基调", text: "悬疑"], server)
    assert note["kind"] == "style"
    assert {:ok, [loaded]} = Memory.list_notes("demo", [], server)
    assert loaded["title"] == "基调"
    assert loaded["meta"] == %{}
  end

  test "script → assets → storyboard → image", %{server: server} do
    assert {:ok, _} = Store.create_project("demo-story", [], server)

    text = """
    第一章 风雨
    林昭站在祖宅门前，天色阴沉。
    """

    assert {:ok, _novel} = Novel.add_novel("demo-story", [text: text, title: "风雨"], server)
    assert {:ok, _events} = Events.extract_events("demo-story", [], server)
    assert {:ok, script} = Script.generate_script("demo-story", [], server)

    assert {:ok, extracted} =
             Assets.extract_assets("demo-story", [script_id: script["script_id"]], server)

    assert extracted["characters"] == 1

    assert {:ok, [character]} = Assets.list_characters("demo-story", [], server)
    assert character["name"] == "林昭"
    assert character["appearance"] == "青衫少年"

    assert {:ok, board} =
             Storyboard.generate_storyboard(
               "demo-story",
               [script_id: script["script_id"]],
               server
             )

    assert board["shot_count"] == 1

    assert {:ok, [shot]} = Storyboard.list_shots("demo-story", [], server)
    assert shot["size"] == "中景"
    assert shot["id"] =~ "sht_"

    previous_client = Application.get_env(:exhub, :toonflow_media_client)
    Application.put_env(:exhub, :toonflow_media_client, Exhub.Toonflow.PipelineTest.MediaStub)

    on_exit(fn ->
      if previous_client,
        do: Application.put_env(:exhub, :toonflow_media_client, previous_client),
        else: Application.delete_env(:exhub, :toonflow_media_client)
    end)

    assert {:ok, image} = Media.generate_image("demo-story", [shot_id: shot["id"]], server)
    assert image["kind"] == "image"
    assert File.exists?(image["path"])
    assert image["prompt"] =~ "青衫少年"

    previous_video = Application.get_env(:exhub, :toonflow_video_client)
    previous_voice = Application.get_env(:exhub, :toonflow_voice_client)
    Application.put_env(:exhub, :toonflow_video_client, Exhub.Toonflow.PipelineTest.VideoStub)
    Application.put_env(:exhub, :toonflow_voice_client, Exhub.Toonflow.PipelineTest.VoiceStub)

    on_exit(fn ->
      if previous_video,
        do: Application.put_env(:exhub, :toonflow_video_client, previous_video),
        else: Application.delete_env(:exhub, :toonflow_video_client)

      if previous_voice,
        do: Application.put_env(:exhub, :toonflow_voice_client, previous_voice),
        else: Application.delete_env(:exhub, :toonflow_voice_client)
    end)

    assert {:ok, video} = Video.generate_video("demo-story", [shot_id: shot["id"]], server)
    assert video["kind"] == "video"
    assert video["meta"]["task"] == "fl2va"
    assert File.exists?(video["path"])

    assert {:ok, voice} = Voice.generate_voice("demo-story", [shot_id: shot["id"]], server)
    assert voice["kind"] == "audio"
    assert voice["prompt"] == "林昭立于门前"
    assert File.exists?(voice["path"])

    assert {:ok, assets} = Media.list_assets("demo-story", [], server)
    assert assets |> Enum.map(& &1["kind"]) |> Enum.sort() == ["audio", "image", "video"]
  end

  test "run_project surfaces an unknown project", %{server: server} do
    assert {:error, :not_found} = Store.run_project("nope", fn _conn -> {:ok, nil} end, server)
  end

  test "add_novel with no source errors", %{server: server} do
    assert {:ok, _} = Store.create_project("demo2", [], server)
    assert {:error, :missing_source} = Novel.add_novel("demo2", [], server)
  end

  describe "Pipeline" do
    test "stages/0 lists the canonical order" do
      assert Pipeline.stages() ==
               ~w(novel events script assets storyboard images videos voices assemble)
    end

    test "runs a subset of stages, reports them, then resume skips them", %{server: server} do
      assert {:ok, _} = Store.create_project("pipe", [], server)

      previous = %{
        media: Application.get_env(:exhub, :toonflow_media_client),
        video: Application.get_env(:exhub, :toonflow_video_client),
        voice: Application.get_env(:exhub, :toonflow_voice_client)
      }

      Application.put_env(:exhub, :toonflow_media_client, Exhub.Toonflow.PipelineTest.MediaStub)
      Application.put_env(:exhub, :toonflow_video_client, Exhub.Toonflow.PipelineTest.VideoStub)
      Application.put_env(:exhub, :toonflow_voice_client, Exhub.Toonflow.PipelineTest.VoiceStub)

      on_exit(fn ->
        restore(:toonflow_media_client, previous.media)
        restore(:toonflow_video_client, previous.video)
        restore(:toonflow_voice_client, previous.voice)
      end)

      text = """
      第一章 风雨
      林昭站在祖宅门前，天色阴沉。
      """

      stages = ~w(novel events script assets storyboard images)

      assert {:ok, report} =
               Pipeline.run("pipe", [text: text, title: "风雨", stages: stages], server)

      assert report["completed"] == length(stages)
      assert report["errors"] == []
      assert Enum.map(report["stages"], & &1["stage"]) == stages

      assert Enum.find(report["stages"], &(&1["stage"] == "storyboard"))["detail"]["shot_count"] ==
               1

      assert Enum.find(report["stages"], &(&1["stage"] == "images"))["detail"]["generated"] == 1

      # A job row was recorded for the run.
      assert {:ok, [job | _]} = Exhub.Toonflow.Jobs.list("pipe", [type: "pipeline"], server)
      assert job["status"] == "success"

      # resume skips every stage whose output already exists.
      assert {:ok, resumed} = Pipeline.resume("pipe", [stages: stages], server)
      statuses = Map.new(resumed["stages"], fn s -> {s["stage"], s["status"]} end)
      assert statuses["novel"] == "skipped"
      assert statuses["script"] == "skipped"
      assert statuses["storyboard"] == "skipped"
      assert statuses["images"] == "skipped"

      assert {:ok, plan} = Pipeline.plan("pipe", [], server)
      plan_statuses = Map.new(plan["stages"], fn s -> {s["stage"], s["status"]} end)
      assert plan_statuses["script"] == "done"
      assert plan_statuses["images"] == "done"
      assert plan_statuses["videos"] == "ready"
      assert plan["next"] == "videos"
    end

    test "stops at the first failing stage", %{server: server} do
      assert {:ok, _} = Store.create_project("pipe-err", [], server)

      assert {:error, report} = Pipeline.run("pipe-err", [stages: ~w(novel events)], server)
      assert report["stage"] == "novel"
      assert report["reason"] == :missing_source
    end
  end

  defp restore(key, nil), do: Application.delete_env(:exhub, key)
  defp restore(key, value), do: Application.put_env(:exhub, key, value)
end

defmodule Exhub.Toonflow.PipelineTest.LLMStub do
  @moduledoc false
  @behaviour Exhub.Toonflow.LLM

  @events ~s({"events":[{"kind":"conflict","summary":"林昭被逐出家门","characters":["林昭"],"location":"祖宅","importance":5}]})

  @assets ~s({"characters":[{"name":"林昭","role":"主角","appearance":"青衫少年"}],"scenes":[{"name":"祖宅","description":"雨夜"}],"props":[]})

  @shots ~s({"shots":[{"scene":"祖宅","shot_desc":"林昭立于门前","size":"中景","lighting":"冷光","motion":"推","characters":["林昭"],"prompt":"青衫少年立于祖宅门前"}]})

  @impl true
  def call_llm(_system, user, _opts) do
    cond do
      String.contains?(user, "事件图 JSON") -> {:ok, @events}
      String.contains?(user, "角色与场景 JSON") -> {:ok, @assets}
      String.contains?(user, "分镜 JSON") -> {:ok, @shots}
      true -> {:ok, "# 第一场\n\n祖宅，夜。\n\n林昭：我不走。"}
    end
  end
end

defmodule Exhub.Toonflow.PipelineTest.VideoStub do
  @moduledoc false
  @behaviour Exhub.Toonflow.Video.Client

  @impl true
  def generate_video(prompt, opts) do
    path = Keyword.fetch!(opts, :out_path)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, "MP4")

    {:ok,
     %{
       "path" => path,
       "url" => "stub://video",
       "model" => opts[:model],
       "task" => opts[:task],
       "task_id" => "t1",
       "duration_seconds" => 6,
       "aspect_ratio" => "16:9",
       "prompt" => prompt
     }}
  end
end

defmodule Exhub.Toonflow.PipelineTest.VoiceStub do
  @moduledoc false
  @behaviour Exhub.Toonflow.Voice.Client

  @impl true
  def generate_voice(text, opts) do
    path = Keyword.fetch!(opts, :out_path)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, "AUDIO")

    {:ok,
     %{
       "path" => path,
       "url" => "stub://voice",
       "model" => opts[:model],
       "speaker" => opts[:speaker],
       "output_format" => opts[:output_format],
       "segments" => 1,
       "prompt" => text
     }}
  end
end

defmodule Exhub.Toonflow.PipelineTest.MediaStub do
  @moduledoc false
  @behaviour Exhub.Toonflow.Media.Client

  @impl true
  def generate_image(prompt, opts) do
    path = Keyword.fetch!(opts, :out_path)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, "PNG")

    {:ok,
     %{
       "path" => path,
       "url" => "stub://image",
       "model" => opts[:model],
       "size" => opts[:size],
       "prompt" => prompt
     }}
  end
end
