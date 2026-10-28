defmodule Exhub.Toonflow.Video do
  @moduledoc """
  Video generation for Toonflow (Phase 3).

  `generate_video/3` resolves a prompt and (optionally) a first-frame image for
  a storyboard shot, calls the configured `Exhub.Toonflow.Video.Client`, saves
  the clip under the project's `assets/videos/`, and records an `assets` row of
  kind `video`.

  The client is injectable — `Application.put_env(:exhub, :toonflow_video_client,
  Mod)` — so tests never hit the network. The default
  (`Exhub.Toonflow.Video.Default`) submits to the shared MoArk async video
  endpoint and polls for the result.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Assets, Config, Media, Store, Storyboard}

  @default_model "MiniMax-H3"

  @doc "The configured video client implementation module."
  @spec video_client() :: module()
  def video_client,
    do: Application.get_env(:exhub, :toonflow_video_client, Exhub.Toonflow.Video.Default)

  @doc """
  Generate a video clip for a shot (or from a free `prompt`).

  `opts`: `:shot_id`, `:prompt`, `:model`, `:task` (`\"t2va\"` | `\"fl2va\"`),
  `:duration_seconds`, `:aspect_ratio`, `:seed`, `:first_frame`, `:last_frame`,
  `:on_progress` (a `(attempt, payload)` fun handed to `HTTP.poll/4`, so a
  minutes-long clip can report live progress).
  When `:task` is unset it defaults to `fl2va` if the shot already has a frame,
  else `t2va`. Frames resolve from the shot's assets: the first is the
  `keyframe_start` when present, else the latest `image`; the last is
  `keyframe_end` (only for a fixed-camera shot, and only while
  `media.last_frame` is enabled). `:first_frame` / `:last_frame` override both,
  and `fl2va` needs a first frame. Without `:duration_seconds` the clip is cut
  to the shot's dialogue (see `clip_duration/1`). Returns `{:ok, asset}` or
  `{:error, reason}`.
  """
  @spec generate_video(String.t(), keyword(), GenServer.server()) ::
          {:ok, map()} | {:error, term()}
  def generate_video(project, opts \\ [], server \\ Store) do
    shot_id = Toonflow.blank(Keyword.get(opts, :shot_id))
    prompt = Toonflow.blank(Keyword.get(opts, :prompt))

    with {:ok, meta} <- Store.get_project(project, server),
         {:ok, shot, characters} <- load_shot(project, shot_id, server),
         {:ok, task, first_frame, last_frame} <- resolve_frames(project, shot, opts, server),
         {:ok, task, last_frame, endpoint} <-
           endpoint_guard(task, shot, last_frame, Keyword.get(opts, :last_frame)),
         {:ok, final_prompt} <- resolve_prompt(prompt, shot, characters, task, last_frame) do
      model = Keyword.get(opts, :model) || media_config()["video_model"] || @default_model
      key = shot_id || Toonflow.new_id("vid")
      out_path = video_path(meta["root_dir"], key)
      duration = Keyword.get(opts, :duration_seconds) || shot_duration(project, shot, server)

      client_opts =
        [model: model, task: task, out_path: out_path]
        |> put_opt(:duration_seconds, duration)
        |> put_opt(:aspect_ratio, Keyword.get(opts, :aspect_ratio) || Config.aspect_ratio())
        |> put_opt(:seed, Keyword.get(opts, :seed))
        |> put_opt(:first_frame, first_frame)
        |> put_opt(:last_frame, last_frame)
        |> put_opt(:on_progress, Keyword.get(opts, :on_progress))

      case video_client().generate_video(final_prompt, client_opts) do
        {:ok, result} -> record(project, shot_id, final_prompt, result, endpoint, server)
        {:error, reason} -> {:error, {:video_failed, reason}}
      end
    end
  end

  # --- pacing ---

  # MiniMax-H3 accepts 4–15 s. A fixed length is what makes an episode drag: a
  # 1 s line inside a 6 s clip leaves 5 s of dead air, and a 7 s line inside the
  # same clip is cut off. The clip follows its line instead.
  @min_clip_seconds 4
  @max_clip_seconds 15
  # The clip also carries the line's lead-in and the breath before the cut.
  @clip_tail_seconds 1.2
  # Mandarin TTS speaks roughly 4 characters per second (measured on MoArk).
  @chars_per_second 4.2

  @doc """
  Spoken seconds estimated from a line of dialogue. Pure.

  Counts CJK characters (one syllable each) and latin/digit words (two), since
  a clip has to be scheduled before its voice exists (`videos` runs ahead of
  `voices` in the pipeline).
  """
  @spec dialogue_seconds(String.t() | nil) :: float()
  def dialogue_seconds(text) when is_binary(text) do
    case spoken_units(text) do
      0 -> 0.0
      units -> units / @chars_per_second + 0.6
    end
  end

  def dialogue_seconds(_text), do: 0.0

  @doc ~S"""
  Clip length for a line of `speech_seconds`: the line plus a tail, rounded up
  and clamped to what MiniMax-H3 accepts (4–15 s). Pure.
  """
  @spec clip_duration(number()) :: pos_integer()
  def clip_duration(speech_seconds) when is_number(speech_seconds) do
    speech_seconds
    |> Kernel.+(@clip_tail_seconds)
    |> Float.ceil()
    |> round()
    |> max(@min_clip_seconds)
    |> min(@max_clip_seconds)
  end

  # The clip length for a shot: its real voice track when one already exists
  # (a re-render), else the estimate from its dialogue. A free prompt with no
  # shot keeps the provider default.
  defp shot_duration(_project, nil, _server), do: nil

  defp shot_duration(project, shot, server) do
    speech =
      case speech_seconds(project, shot, server) do
        {:ok, seconds} when seconds > 0 -> seconds
        _ -> dialogue_seconds(dialogue(shot))
      end

    clip_duration(speech)
  end

  defp speech_seconds(project, shot, server) do
    case Media.latest_asset(project, shot["id"], "audio", server) do
      {:ok, %{"path" => path}} when is_binary(path) -> probe_seconds(path)
      _ -> :error
    end
  end

  defp dialogue(shot) do
    meta = (shot && shot["meta"]) || %{}
    Enum.find([meta["dialogue"], meta["台词"]], &(is_binary(&1) and String.trim(&1) != ""))
  end

  defp spoken_units(text) do
    cjk = length(Regex.scan(~r/[\x{3400}-\x{4DBF}\x{4E00}-\x{9FFF}\x{F900}-\x{FAFF}]/u, text))
    words = length(Regex.scan(~r/[A-Za-z0-9]+/u, text))
    cjk + words * 2
  end

  @doc """
  Duration of a media file in seconds, via `ffprobe`. `:error` when the file is
  missing or unreadable, so callers can fall back to an estimate.
  """
  @spec probe_seconds(String.t()) :: {:ok, float()} | :error
  def probe_seconds(path) when is_binary(path) do
    argv = [
      ffprobe_path(),
      "-v",
      "error",
      "-show_entries",
      "format=duration",
      "-of",
      "csv=p=0",
      path
    ]

    case run(argv) do
      {0, output} ->
        case Float.parse(String.trim(output)) do
          {seconds, _rest} when seconds > 0 -> {:ok, seconds}
          _ -> :error
        end

      _ ->
        :error
    end
  end

  defp ffprobe_path do
    (Config.get("assembly", %{}) || %{})["ffprobe_path"] || "ffprobe"
  end

  defp run(argv) do
    # `Exile` needs its supervision tree; tests that run with `--no-start` must
    # not crash the caller.
    _ = Application.ensure_all_started(:exile)

    {out, code} =
      Exile.stream(argv, stderr: :consume, env: Exhub.MCP.Desktop.Helpers.clean_env())
      |> Enum.reduce({"", 0}, fn
        {:stdout, data}, {o, c} -> {o <> data, c}
        {:exit, {:status, c}}, {o, _c} -> {o, c}
        {:exit, :epipe}, {o, c} -> {o, c}
        _other, acc -> acc
      end)

    {code || 0, out}
  rescue
    _e -> {nil, ""}
  end

  # --- endpoint guard ---

  # `fl2va` honours the first frame but not the last: on a measured episode the
  # opening frame matched its keyframe at 32 dB PSNR while the final frame landed
  # at 21 dB — the model drifts off a distant end frame, and that drift reads as
  # an unintended cut inside the take. A camera move makes it worse: the prompt
  # asks for one thing while the end frame pins another. So only a fixed-camera
  # shot keeps its end frame; a moving shot interpolates from the first frame
  # alone (MiniMax accepts `fl2va` with a single frame), which is image-to-video.
  defp endpoint_guard(task, _shot, last_frame, explicit) when is_binary(explicit),
    do: {:ok, task, last_frame, %{}}

  defp endpoint_guard(task, _shot, nil, _explicit), do: {:ok, task, nil, %{}}

  defp endpoint_guard(task, shot, last_frame, _explicit) do
    if Storyboard.fixed_camera?(shot) do
      {:ok, task, last_frame, %{}}
    else
      {:ok, task, nil, %{"last_frame_dropped" => "camera_move"}}
    end
  end

  @doc "Local output path for a generated clip (pure)."
  @spec video_path(String.t(), String.t()) :: String.t()
  def video_path(project_dir, key),
    do: Path.join([project_dir, "assets", "videos", Toonflow.file_key(key) <> ".mp4"])

  # --- prompt / frame resolution ---

  defp load_shot(_project, nil, _server), do: {:ok, nil, []}

  defp load_shot(project, shot_id, server) do
    with {:ok, shot} <- Storyboard.get_shot(project, shot_id, server),
         {:ok, characters} <- Assets.list_characters(project, [], server) do
      {:ok, shot, characters}
    end
  end

  defp resolve_prompt(prompt, _shot, _characters, _task, _last) when is_binary(prompt),
    do: {:ok, prompt}

  defp resolve_prompt(_prompt, nil, _characters, _task, _last), do: {:error, :missing_prompt}

  # Frame the prompt against what the model is actually given. Two pinned frames
  # interpolate to a fixed composition, so a camera move would fight them; a
  # fixed-camera shot wants the same clause regardless. Every other shot carries
  # its 运镜 (bounded by `Storyboard`), which is what gives the cut its energy.
  defp resolve_prompt(_prompt, shot, characters, task, last_frame) do
    lock? = task == "fl2va" and (not is_nil(last_frame) or Storyboard.fixed_camera?(shot))

    case Storyboard.shot_prompt(shot, characters, [], lock_framing: lock?) do
      "" -> {:error, :missing_prompt}
      prompt -> {:ok, prompt}
    end
  end

  defp resolve_frames(project, shot, opts, server) do
    case normalize_task(Keyword.get(opts, :task)) do
      "t2va" -> {:ok, "t2va", nil, nil}
      "fl2va" -> resolve_fl2va(project, shot, opts, server)
      _ -> default_task(project, shot, opts, server)
    end
  end

  defp default_task(project, shot, opts, server) do
    with {:ok, first, last} <- shot_frames(project, shot, opts, server) do
      if first, do: {:ok, "fl2va", first, last}, else: {:ok, "t2va", nil, nil}
    end
  end

  defp resolve_fl2va(project, shot, opts, server) do
    with {:ok, resolved_first, last} <- shot_frames(project, shot, opts, server) do
      case Toonflow.blank(Keyword.get(opts, :first_frame)) || resolved_first do
        nil -> {:error, :missing_first_frame}
        first -> {:ok, "fl2va", first, last}
      end
    end
  end

  defp shot_frames(_project, nil, _opts, _server), do: {:ok, nil, nil}

  defp shot_frames(project, shot, opts, server) do
    with {:ok, first} <- first_frame(project, shot, opts, server),
         {:ok, last} <- last_frame(project, shot, opts, server) do
      {:ok, first, last}
    end
  end

  # The first frame prefers a generated keyframe, falling back to the plain
  # frame image (Phase 2 behaviour) so fl2va still works without keyframes.
  defp first_frame(project, shot, opts, server) do
    case Toonflow.blank(Keyword.get(opts, :first_frame)) do
      nil -> asset_path(project, shot["id"], ~w(keyframe_start image), server)
      path -> {:ok, path}
    end
  end

  defp last_frame(project, shot, opts, server) do
    case Toonflow.blank(Keyword.get(opts, :last_frame)) do
      nil -> generated_last_frame(project, shot, server)
      path -> {:ok, path}
    end
  end

  defp generated_last_frame(project, shot, server) do
    if last_frame_enabled?() do
      asset_path(project, shot["id"], ~w(keyframe_end), server)
    else
      {:ok, nil}
    end
  end

  defp asset_path(project, shot_id, kinds, server) do
    Enum.reduce_while(kinds, {:ok, nil}, fn kind, _acc ->
      case Media.latest_asset(project, shot_id, kind, server) do
        {:ok, %{"path" => path}} when is_binary(path) -> {:halt, {:ok, path}}
        {:ok, _} -> {:cont, {:ok, nil}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp last_frame_enabled?, do: media_config()["last_frame"] != false

  defp normalize_task(task) when is_binary(task), do: String.trim(task)
  defp normalize_task(_task), do: nil

  # --- persistence ---

  defp record(project, shot_id, prompt, result, endpoint, server) do
    meta =
      result
      |> Map.take(["model", "task", "task_id", "duration_seconds", "aspect_ratio"])
      |> Map.merge(endpoint)

    Media.insert_asset(
      project,
      [
        shot_id: shot_id,
        kind: "video",
        path: result["path"],
        url: result["url"],
        prompt: prompt,
        meta: meta
      ],
      server
    )
  end

  defp put_opt(opts, _key, nil), do: opts
  defp put_opt(opts, key, value), do: Keyword.put(opts, key, value)

  defp media_config, do: Config.get("media", %{}) || %{}
end

defmodule Exhub.Toonflow.Video.Client do
  @moduledoc "Behaviour for Toonflow video generation backends."

  @callback generate_video(prompt :: String.t(), opts :: keyword()) ::
              {:ok, map()} | {:error, term()}
end

defmodule Exhub.Toonflow.Video.Default do
  @moduledoc """
  Default video client — MoArk async video generation (`MiniMax-H3`).

  Reuses the pure helpers from `Exhub.MCP.Tools.VideoGen`
  (`resolve_frames/1`, `validate_params/1`, `build_body/1`, `interpret_poll/1`)
  so the request body and task classification stay identical to the `video_gen`
  tool; only submission, polling and download live here.
  """

  @behaviour Exhub.Toonflow.Video.Client

  alias Exhub.MCP.Tools.VideoGen
  alias Exhub.Toonflow.{Config, HTTP}

  @submit_url HTTP.api_base() <> "/async/videos/generations"

  # A MiniMax-H3 clip routinely takes 3–8 minutes, so the shared 55 × 10s
  # budget (~9m10s) times out mid-run on real shots. Both knobs are configurable
  # under `media` so a slower account can widen them without a code change.
  @default_poll_interval_ms 10_000
  @default_poll_attempts 110

  @doc """
  The polling options for an async video task (pure).

  Reads `media.video_poll_interval_ms` / `media.video_poll_attempts`, falling
  back to 10 s / 110 attempts (~18 minutes).
  """
  @spec poll_opts(map()) :: keyword()
  def poll_opts(media_config \\ %{}) do
    [
      interval_ms: Map.get(media_config, "video_poll_interval_ms") || @default_poll_interval_ms,
      max_attempts: Map.get(media_config, "video_poll_attempts") || @default_poll_attempts
    ]
  end

  @impl true
  def generate_video(prompt, opts) do
    out_path = Keyword.fetch!(opts, :out_path)

    params = %{
      prompt: prompt,
      model: Keyword.get(opts, :model, "MiniMax-H3"),
      task: Keyword.get(opts, :task, "t2va"),
      duration_seconds: Keyword.get(opts, :duration_seconds),
      aspect_ratio: Keyword.get(opts, :aspect_ratio),
      first_frame: Keyword.get(opts, :first_frame),
      last_frame: Keyword.get(opts, :last_frame),
      seed: Keyword.get(opts, :seed)
    }

    with {:ok, key} <- HTTP.api_key(),
         {:ok, resolved} <- VideoGen.resolve_frames(params),
         {:ok, validated} <- VideoGen.validate_params(resolved),
         {:ok, task_id} <- HTTP.submit(@submit_url, VideoGen.build_body(validated), key),
         {:ok, result} <-
           HTTP.poll(
             task_id,
             key,
             &VideoGen.interpret_poll/1,
             Keyword.put(
               poll_opts(Config.get("media", %{}) || %{}),
               :on_progress,
               Keyword.get(opts, :on_progress)
             )
           ),
         {:ok, url} <- video_url(result),
         {:ok, data} <- HTTP.download(url),
         :ok <- HTTP.save(data, out_path) do
      {:ok,
       %{
         "path" => out_path,
         "url" => url,
         "model" => validated.model,
         "task" => validated.task,
         "task_id" => task_id,
         "duration_seconds" => validated.duration_seconds,
         "aspect_ratio" => validated.aspect_ratio
       }}
    end
  end

  defp video_url(result) do
    case Map.get(result, "output") do
      %{"file_url" => url} when is_binary(url) and url != "" -> {:ok, url}
      _ -> {:error, {:no_video_url, result}}
    end
  end
end
