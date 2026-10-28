defmodule Exhub.Toonflow.Assemble do
  @moduledoc """
  Episode assembly for Toonflow (Phase 3).

  `assemble/3` concatenates a script's shot clips (in storyboard order) into a
  single video under `output/`, optionally mixes per-shot voiceover, and writes
  an `.srt` subtitle track derived from each shot's dialogue. `export/3` wraps
  `assemble/3` and packages the result under a requested filename.

  Clips are concatenated with `-c copy`, which requires identical stream
  geometry, so a clip rendered at another size is first re-encoded onto the
  first clip's frame (`normalize_argv/4`) — otherwise the episode would smear
  instead of letterboxing.

  Clips can instead be cross-dissolved (`dissolve_argv/5`) — each join is
  shortened by the transition, and `apply_transition/2` moves the subtitle and
  voice timeline with it.

  The FFmpeg/FFprobe argv builders (`concat_argv/3`, `concat_audio_argv/3`,
  `dissolve_argv/5`, `mix_audio_argv/5`, `silent_mix_argv/4`, `subtitle_argv/4`,
  `probe_argv/2`, `probe_audio_argv/2`, `probe_size_argv/2`, `normalize_argv/4`,
  `concat_list/1`, `srt/1`, `cues/1`, `parse_duration/1`, `parse_size/1`,
  `apply_transition/2`, `timestamp/1`) are pure and unit-tested; execution
  shells out via `Exile`.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Config, Media, Script, Store, Storyboard}

  # ffmpeg audio filter that strips a clip's leading/trailing silence (keeping
  # 50 ms at each edge), inserts a short lead-in, then pads with silence — the
  # `-t <shot_duration>` output option cuts it back to the exact shot length.
  @voice_trim_filter "silenceremove=start_periods=1:start_silence=0.05:start_threshold=-45dB," <>
                       "areverse,silenceremove=start_periods=1:start_silence=0.05:start_threshold=-45dB,areverse"

  @voice_lead_in_ms 150

  # The clips' own audio (fog, wind, footsteps) is the only room tone the episode
  # has; under the voices at this gain it keeps a cut from dropping into silence.
  @ambience_gain 0.15

  @doc """
  Assemble a script's clips into an episode.

  `opts`: `:script_id` (default: the latest), `:scene` (limit to one scene),
  `:subtitles` (soft-mux the generated `.srt`; default from
  `assembly.subtitles`), `:mix_audio` (mix per-shot voiceover; default false),
  `:transition` (cross-dissolve seconds between clips; default from
  `assembly.transition_seconds`, `0` disables), `:name` (artifact stem, default
  the script id). Every shot must already have a generated video clip.
  `:mix_audio` keeps each clip's own audio as a low ambient bed under the voices.
  """
  @spec assemble(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def assemble(project, opts \\ [], server \\ Store) do
    script_id = Toonflow.blank(Keyword.get(opts, :script_id))
    scene = Toonflow.blank(Keyword.get(opts, :scene))
    subtitles = Keyword.get(opts, :subtitles, assembly_config()["subtitles"] != false)
    mix_audio = Keyword.get(opts, :mix_audio, false)

    transition =
      transition_seconds(Keyword.get(opts, :transition, assembly_config()["transition_seconds"]))

    with {:ok, meta} <- Store.get_project(project, server),
         {:ok, script} <- fetch_script(project, script_id, server),
         {:ok, shots} <-
           Storyboard.list_shots(project, [script_id: script["id"], scene: scene], server),
         {:ok, clips} <- collect_clips(project, shots, server) do
      name = Toonflow.blank(Keyword.get(opts, :name)) || script["id"]
      build(project, meta["root_dir"], name, clips, subtitles, mix_audio, transition, server)
    end
  end

  @doc """
  Assemble and package an episode under `opts[:filename]` (default: the
  assembled name). Returns a manifest of the produced artifacts.
  """
  @spec export(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def export(project, opts \\ [], server \\ Store) do
    filename = Toonflow.blank(Keyword.get(opts, :filename))

    assemble_opts =
      opts
      |> Keyword.put(:name, filename_stem(filename) || Toonflow.blank(Keyword.get(opts, :name)))

    with {:ok, built} <- assemble(project, assemble_opts, server),
         {:ok, meta} <- Store.get_project(project, server),
         {:ok, assets} <- Media.list_assets(project, [], server) do
      out_dir = Path.join(meta["root_dir"], "output")
      target = if filename, do: Path.join(out_dir, ensure_ext(filename)), else: built["video"]

      with :ok <- maybe_copy(built["video"], target) do
        {:ok,
         %{
           "video" => target,
           "subtitles" => built["subtitles"],
           "output_dir" => out_dir,
           "clips" => built["clips"],
           "duration" => built["duration"],
           "assets" => length(assets)
         }}
      end
    end
  end

  # --- build pipeline ---

  defp build(project, dir, name, clips, subtitles, mix_audio, transition, server) do
    out_dir = Path.join(dir, "output")
    File.mkdir_p!(out_dir)

    list_path = Path.join(out_dir, ".#{name}.concat.txt")
    video_path = Path.join(out_dir, "#{name}.mp4")
    srt_path = Path.join(out_dir, "#{name}.srt")
    ffmpeg = ffmpeg_path()

    with :ok <- check_tool(ffmpeg, :ffmpeg),
         {:ok, paths, normalized} <- normalize_clips(ffmpeg, out_dir, name, clips),
         :ok <- File.write(list_path, concat_list(paths)),
         {:ok, entries} <- with_durations(clips),
         {mode, argv, entries} <-
           sequence(ffmpeg, paths, list_path, entries, transition, video_path),
         {:ok, _} <- run(argv),
         :ok <- File.write(srt_path, srt(cues(entries))) do
      {final, audio} =
        maybe_mix(project, ffmpeg, video_path, entries, mix_audio, out_dir, name, server)

      final =
        if subtitles and File.read!(srt_path) != "" do
          remux_subtitles(ffmpeg, final, srt_path, out_dir, name)
        else
          final
        end

      {:ok,
       %{
         "name" => name,
         "video" => final,
         "subtitles" => srt_path,
         "clips" => length(clips),
         "duration" => total_duration(entries),
         "audio" => audio,
         "normalized" => normalized,
         "transition" => mode,
         "output_dir" => out_dir
       }}
    end
  end

  # Concatenation alone leaves every join hard, and a cut between two
  # independently generated compositions is exactly where an episode reads as
  # stitched. A short cross-dissolve softens the join; it needs audio in every
  # clip because the audio is acrossfaded at the same offsets, so picture and
  # sound stay in sync. Anything else falls back to a straight concat.
  defp sequence(ffmpeg, paths, list_path, entries, transition, out) do
    durations = Enum.map(entries, & &1["duration"])

    if transition > 0 and length(paths) > 1 and Enum.all?(paths, &has_audio?/1) do
      entries = apply_transition(entries, transition)
      {:dissolve, dissolve_argv(ffmpeg, paths, durations, transition, out), entries}
    else
      {:concat, concat_argv(ffmpeg, list_path, out), entries}
    end
  end

  defp has_audio?(path) do
    case execute(probe_audio_argv(ffprobe_path(), path)) do
      {0, output} -> String.trim(output) != ""
      _ -> false
    end
  end

  defp transition_seconds(value) when is_number(value) and value > 0, do: value * 1.0
  defp transition_seconds(_value), do: 0.0

  defp ambience_gain do
    case assembly_config()["ambience_gain"] do
      value when is_number(value) and value >= 0 -> value * 1.0
      _ -> @ambience_gain
    end
  end

  # `concat_argv/3` copies streams, which requires every clip to share the same
  # frame size and pixel aspect. One clip rendered at another size (provider
  # default change, a retry, a vertical shot) would otherwise corrupt the whole
  # episode — frames smear instead of being letterboxed — so clips that differ
  # from the first are re-encoded onto its geometry.
  defp normalize_clips(ffmpeg, out_dir, name, clips) do
    ffprobe = ffprobe_path()

    with {:ok, {width, height, _square?}} <- probe_geometry(ffprobe, hd(clips)["path"]) do
      target = {even(width), even(height)}

      {paths, normalized} =
        clips
        |> Enum.with_index()
        |> Enum.reduce({[], []}, fn {clip, index}, {paths, normalized} ->
          path = clip["path"]

          case probe_geometry(ffprobe, path) do
            {:ok, geometry} ->
              if matches_target?(geometry, target) do
                {paths ++ [path], normalized}
              else
                out = Path.join(out_dir, ".#{name}.norm#{index}.mp4")

                case run(normalize_argv(ffmpeg, path, out, target)) do
                  {:ok, _output} ->
                    {paths ++ [out], normalized ++ [normalized_entry(clip, geometry, target)]}

                  {:error, _reason} ->
                    {paths ++ [path], normalized}
                end
              end

            {:error, _reason} ->
              {paths ++ [path], normalized}
          end
        end)

      {:ok, paths, normalized}
    else
      {:error, _reason} -> {:ok, Enum.map(clips, & &1["path"]), []}
    end
  end

  defp probe_geometry(ffprobe, path) do
    case execute(probe_size_argv(ffprobe, path)) do
      {0, output} -> parse_size(output)
      {code, output} -> {:error, {:probe_failed, code, String.slice(output, 0, 500)}}
    end
  end

  defp matches_target?({width, height, true}, {width, height}), do: true
  defp matches_target?(_geometry, _target), do: false

  defp normalized_entry(clip, {width, height, _square?}, {target_width, target_height}) do
    %{
      "shot" => clip["shot"]["idx"],
      "path" => clip["path"],
      "from" => "#{width}x#{height}",
      "to" => "#{target_width}x#{target_height}"
    }
  end

  # libx264/yuv420p needs even dimensions.
  defp even(value) when is_integer(value) and rem(value, 2) == 1, do: value - 1
  defp even(value), do: value

  defp collect_clips(project, shots, server) do
    case Enum.reduce_while(shots, {[], []}, fn shot, {clips, missing} ->
           case Media.latest_asset(project, shot["id"], "video", server) do
             {:ok, %{"path" => path} = asset} when is_binary(path) ->
               if File.exists?(path) do
                 {:cont,
                  {clips ++ [%{"shot" => shot, "path" => path, "asset" => asset}], missing}}
               else
                 {:cont, {clips, missing ++ [shot["idx"]]}}
               end

             {:ok, _} ->
               {:cont, {clips, missing ++ [shot["idx"]]}}

             {:error, reason} ->
               {:halt, {:error, reason}}
           end
         end) do
      {:error, reason} -> {:error, reason}
      {[], _missing} -> {:error, :no_video_clips}
      {clips, []} -> {:ok, clips}
      {_clips, missing} -> {:error, {:missing_clips, Enum.sort(missing)}}
    end
  end

  defp maybe_mix(_project, _ffmpeg, video_path, _entries, false, _out_dir, _name, _server),
    do: {video_path, nil}

  defp maybe_mix(project, ffmpeg, video_path, entries, true, out_dir, name, server) do
    audio_list = Path.join(out_dir, ".#{name}.audio.txt")
    audio_path = Path.join(out_dir, ".#{name}.mix.m4a")
    mixed = Path.join(out_dir, ".#{name}.mixed.mp4")

    with {:ok, voices} <- voice_clips(project, entries, server),
         {:ok, segments} <- build_segments(ffmpeg, voices, out_dir, name),
         :ok <- File.write(audio_list, concat_list(segments)),
         {:ok, _} <- run(concat_audio_argv(ffmpeg, audio_list, audio_path)),
         {:ok, _} <- run(mix_argv(ffmpeg, video_path, audio_path, mixed)) do
      _ = File.rename(mixed, video_path)
      {video_path, segments}
    else
      {:error, reason} -> {video_path, {:skipped, reason}}
    end
  end

  defp mix_argv(ffmpeg, video, audio, out) do
    if has_audio?(video) do
      mix_audio_argv(ffmpeg, video, audio, out, ambience_gain: ambience_gain())
    else
      silent_mix_argv(ffmpeg, video, audio, out)
    end
  end

  # The latest voice asset for each shot, paired with that shot's clip duration.
  defp voice_clips(project, entries, server) do
    Enum.reduce_while(entries, {:ok, []}, fn entry, {:ok, acc} ->
      shot = entry["shot"]

      case Media.latest_asset(project, shot["id"], "audio", server) do
        {:ok, %{"path" => path}} when is_binary(path) ->
          voice = %{"path" => path, "duration" => entry["duration"] || 0.0, "idx" => shot["idx"]}
          {:cont, {:ok, acc ++ [voice]}}

        {:ok, _} ->
          {:halt, {:error, {:missing_voice, shot["idx"]}}}

        {:error, reason} ->
          {:halt, {:error, reason}}
      end
    end)
  end

  # Trim each voice clip's leading/trailing silence and pad it to exactly the
  # shot's duration, so the concatenated track stays in sync with the video.
  defp build_segments(ffmpeg, voices, out_dir, name) do
    Enum.reduce_while(Enum.with_index(voices), {:ok, []}, fn {voice, index}, {:ok, acc} ->
      out = Path.join(out_dir, ".#{name}.voice#{index}.m4a")

      case run(voice_segment_argv(ffmpeg, voice["path"], voice["duration"], out)) do
        {:ok, _} -> {:cont, {:ok, acc ++ [out]}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  defp remux_subtitles(ffmpeg, video, srt, out_dir, name) do
    out = Path.join(out_dir, ".#{name}.subs.mp4")

    case run(subtitle_argv(ffmpeg, video, srt, out)) do
      {:ok, _} ->
        if File.rename(out, video) == :ok, do: video, else: video

      {:error, _} ->
        video
    end
  end

  defp with_durations(clips) do
    ffprobe = ffprobe_path()

    {:ok,
     Enum.map(clips, fn clip ->
       duration =
         case probe_duration(ffprobe, clip["path"]) do
           {:ok, value} -> value
           {:error, _} -> asset_duration(clip["asset"])
         end

       %{"shot" => clip["shot"], "duration" => duration}
     end)}
  end

  defp asset_duration(asset) do
    case asset["meta"] do
      %{"duration_seconds" => value} when is_number(value) -> value * 1.0
      _ -> 0.0
    end
  end

  defp total_duration(entries),
    do: entries |> Enum.map(& &1["duration"]) |> Enum.sum()

  # --- script lookup ---

  defp fetch_script(project, script_id, server) do
    opts = if script_id, do: [script_id: script_id], else: []

    case Script.get_script(project, opts, server) do
      {:ok, script} -> {:ok, script}
      {:error, :not_found} -> {:error, :no_script}
      {:error, reason} -> {:error, reason}
    end
  end

  # --- process execution ---

  defp run(argv) do
    case execute(argv) do
      {0, output} -> {:ok, output}
      {code, output} -> {:error, {:ffmpeg_failed, code, String.slice(output, 0, 2000)}}
    end
  end

  defp probe_duration(ffprobe, path) do
    case execute(probe_argv(ffprobe, path)) do
      {0, output} -> parse_duration(output)
      {code, output} -> {:error, {:probe_failed, code, String.slice(output, 0, 500)}}
    end
  end

  defp execute(argv) do
    {out, err, code} =
      Exile.stream(argv, stderr: :consume, env: Exhub.MCP.Desktop.Helpers.clean_env())
      |> Enum.reduce({"", "", 0}, fn
        {:stdout, data}, {o, e, c} -> {o <> data, e, c}
        {:stderr, data}, {o, e, c} -> {o, e <> data, c}
        {:exit, {:status, c}}, {o, e, _} -> {o, e, c}
        {:exit, :epipe}, {o, e, _} -> {o, e, 0}
        _other, acc -> acc
      end)

    {code || 0, out <> err}
  rescue
    e -> {nil, Exception.message(e)}
  end

  defp check_tool(exec, kind) do
    exists? =
      if Path.type(exec) == :absolute,
        do: File.exists?(exec),
        else: System.find_executable(exec) != nil

    if exists?, do: :ok, else: {:error, {:missing_tool, kind, exec}}
  end

  # --- pure argv / text builders ---

  @doc "ffmpeg argv to concatenate the clips listed in `list_path` into `out`."
  @spec concat_argv(String.t(), String.t(), String.t()) :: [String.t()]
  def concat_argv(ffmpeg, list_path, out),
    do: [ffmpeg, "-y", "-f", "concat", "-safe", "0", "-i", list_path, "-c", "copy", out]

  @doc "ffmpeg argv to concatenate audio clips (re-encoded to AAC) into `out`."
  @spec concat_audio_argv(String.t(), String.t(), String.t()) :: [String.t()]
  def concat_audio_argv(ffmpeg, list_path, out),
    do: [
      ffmpeg,
      "-y",
      "-f",
      "concat",
      "-safe",
      "0",
      "-i",
      list_path,
      "-c:a",
      "aac",
      "-b:a",
      "192k",
      out
    ]

  @doc "ffprobe argv listing a file's audio streams (empty output = none)."
  @spec probe_audio_argv(String.t(), String.t()) :: [String.t()]
  def probe_audio_argv(ffprobe, path),
    do: [
      ffprobe,
      "-v",
      "error",
      "-select_streams",
      "a",
      "-show_entries",
      "stream=codec_name",
      "-of",
      "csv=p=0",
      path
    ]

  @doc ~S"""
  ffmpeg argv that cross-dissolves `paths` — `durations` seconds each — into
  `out`, picture and sound together. Pure.

  `xfade` shortens each join by `fade` and `acrossfade` trims the audio by the
  same amount, so the tracks stay aligned; the output is `sum(durations) -
  (length - 1) * fade` long, which is what `apply_transition/2` gives the
  timeline the subtitles and voice segments are built from. Keep `fade` a whole
  number of frames — a frame-aligned value reproduces that length exactly, while
  e.g. 0.12 s at 24 fps rounds every join up and lengthens the episode.
  """
  @spec dissolve_argv(String.t(), [String.t()], [number()], number(), String.t()) :: [String.t()]
  def dissolve_argv(ffmpeg, paths, durations, fade, out) do
    count = length(paths)
    inputs = Enum.flat_map(paths, &["-i", &1])

    prepares =
      Enum.flat_map(0..(count - 1), fn index ->
        [
          "[#{index}:v]format=yuv420p,setsar=1[v#{index}]",
          "[#{index}:a]aformat=sample_rates=48000:channel_layouts=stereo[a#{index}]"
        ]
      end)

    {joins, video_label, audio_label, _end_at} =
      Enum.reduce(1..(count - 1), {[], "v0", "a0", hd(durations)}, fn index,
                                                                      {acc, prev_video,
                                                                       prev_audio, end_at} ->
        final? = index == count - 1
        video_out = if final?, do: "vout", else: "vx#{index}"
        audio_out = if final?, do: "aout", else: "ax#{index}"
        offset = end_at - fade

        step = [
          "[#{prev_video}][v#{index}]xfade=transition=fade:duration=#{format_seconds(fade)}:" <>
            "offset=#{format_seconds(offset)}[#{video_out}]",
          "[#{prev_audio}][a#{index}]acrossfade=d=#{format_seconds(fade)}:c1=tri:c2=tri[#{audio_out}]"
        ]

        {acc ++ step, video_out, audio_out, end_at + Enum.at(durations, index) - fade}
      end)

    [ffmpeg, "-y"]
    |> Kernel.++(inputs)
    |> Kernel.++([
      "-filter_complex",
      Enum.join(prepares ++ joins, ";"),
      "-map",
      "[#{video_label}]",
      "-map",
      "[#{audio_label}]",
      "-c:v",
      "libx264",
      "-crf",
      "18",
      "-preset",
      "veryfast",
      "-pix_fmt",
      "yuv420p",
      "-c:a",
      "aac",
      "-b:a",
      "192k",
      out
    ])
  end

  @doc ~S"""
  Shorten every entry but the last by the crossfade so the timeline matches the
  dissolved picture. Pure.
  """
  @spec apply_transition([map()], number()) :: [map()]
  def apply_transition(entries, fade) when is_number(fade) and fade > 0 do
    last = length(entries) - 1

    entries
    |> Enum.with_index()
    |> Enum.map(fn
      {entry, ^last} -> entry
      {entry, _index} -> %{entry | "duration" => max(entry["duration"] - fade, 0.1)}
    end)
  end

  def apply_transition(entries, _fade), do: entries

  @doc ~S"""
  ffmpeg argv to turn a voice clip into a segment that fills exactly `duration`
  seconds: leading/trailing silence is trimmed, a short lead-in is inserted, and
  the rest is padded with silence (or cut when the clip is longer).
  """
  @spec voice_segment_argv(String.t(), String.t(), number(), String.t()) :: [String.t()]
  def voice_segment_argv(ffmpeg, voice, duration, out) do
    limit =
      if is_number(duration) and duration > 0,
        do: ["-t", format_seconds(duration)],
        else: []

    [
      ffmpeg,
      "-y",
      "-i",
      voice,
      "-af",
      @voice_trim_filter <> ",adelay=#{@voice_lead_in_ms}:all=1,apad"
    ] ++ limit ++ ["-c:a", "aac", "-b:a", "192k", out]
  end

  @doc """
  ffmpeg argv to mix `audio` over the video's own audio track (video copied).

  The clip audio is not discarded: at `:ambience_gain` (default 0.15, ≈ −16 dB)
  it is the room tone that stops a cut from dropping into digital silence. Use
  `silent_mix_argv/4` for a video with no audio track.

  The explicit `-map`s matter: without them ffmpeg's default stream selection
  picks the video's *own* audio and silently drops the supplied track.
  """
  @spec mix_audio_argv(String.t(), String.t(), String.t(), String.t(), keyword()) :: [String.t()]
  def mix_audio_argv(ffmpeg, video, audio, out, opts \\ []) do
    gain = Keyword.get(opts, :ambience_gain, @ambience_gain)

    filter =
      "[0:a]volume=#{format_seconds(gain)}[amb];" <>
        "[amb][1:a]amix=inputs=2:duration=first:dropout_transition=0:normalize=0[mixed]"

    [
      ffmpeg,
      "-y",
      "-i",
      video,
      "-i",
      audio,
      "-filter_complex",
      filter,
      "-map",
      "0:v:0",
      "-map",
      "[mixed]",
      "-c:v",
      "copy",
      "-c:a",
      "aac",
      "-b:a",
      "192k",
      "-shortest",
      out
    ]
  end

  @doc """
  ffmpeg argv to give a silent video the `audio` track (video copied).

  Used when the clips carry no audio of their own, so there is no ambience to
  keep.
  """
  @spec silent_mix_argv(String.t(), String.t(), String.t(), String.t()) :: [String.t()]
  def silent_mix_argv(ffmpeg, video, audio, out),
    do: [
      ffmpeg,
      "-y",
      "-i",
      video,
      "-i",
      audio,
      "-map",
      "0:v:0",
      "-map",
      "1:a:0",
      "-c:v",
      "copy",
      "-c:a",
      "aac",
      "-shortest",
      out
    ]

  @doc "ffmpeg argv to soft-mux an SRT subtitle track into a video."
  @spec subtitle_argv(String.t(), String.t(), String.t(), String.t()) :: [String.t()]
  def subtitle_argv(ffmpeg, video, srt, out),
    do: [
      ffmpeg,
      "-y",
      "-i",
      video,
      "-i",
      srt,
      "-c",
      "copy",
      "-c:s",
      "mov_text",
      "-metadata:s:s:0",
      "language=chi",
      out
    ]

  @doc "ffprobe argv to read a media file's duration in seconds."
  @spec probe_argv(String.t(), String.t()) :: [String.t()]
  def probe_argv(ffprobe, path),
    do: [
      ffprobe,
      "-v",
      "error",
      "-show_entries",
      "format=duration",
      "-of",
      "default=nw=1:nk=1",
      path
    ]

  @doc "ffprobe argv to read a video stream's frame size and sample aspect ratio."
  @spec probe_size_argv(String.t(), String.t()) :: [String.t()]
  def probe_size_argv(ffprobe, path),
    do: [
      ffprobe,
      "-v",
      "error",
      "-select_streams",
      "v:0",
      "-show_entries",
      "stream=width,height,sample_aspect_ratio",
      "-of",
      "csv=p=0",
      path
    ]

  @doc """
  Parse `probe_size_argv/2` output into `{width, height, square_pixels?}`. Pure.

  ffprobe prints `1344,768,N/A` when the stream carries no sample aspect ratio,
  which means square pixels.
  """
  @spec parse_size(String.t()) ::
          {:ok, {pos_integer(), pos_integer(), boolean()}} | {:error, term()}
  def parse_size(text) when is_binary(text) do
    case String.split(String.trim(text), ",") do
      [width, height | rest] ->
        with {w, ""} <- Integer.parse(String.trim(width)),
             {h, ""} <- Integer.parse(String.trim(height)) do
          {:ok, {w, h, square_pixels?(List.first(rest))}}
        else
          _ -> {:error, :invalid_stream}
        end

      _ ->
        {:error, :invalid_stream}
    end
  end

  def parse_size(_text), do: {:error, :invalid_stream}

  defp square_pixels?(nil), do: true

  defp square_pixels?(sar) do
    case sar |> String.trim() |> String.split(":") do
      [num, den] -> num == den or num == "0" or den == "0"
      _ -> true
    end
  end

  @doc """
  ffmpeg argv to re-frame `input` onto `{width, height}`: scale to fit, pad the
  remainder (letterbox/pillarbox) and force square pixels so the clip can be
  concatenated with clips already at that size. Pure.
  """
  @spec normalize_argv(String.t(), String.t(), String.t(), {pos_integer(), pos_integer()}) ::
          [String.t()]
  def normalize_argv(ffmpeg, input, out, {width, height}) do
    filter =
      "scale=#{width}:#{height}:force_original_aspect_ratio=decrease," <>
        "pad=#{width}:#{height}:(ow-iw)/2:(oh-ih)/2:color=black,setsar=1"

    [
      ffmpeg,
      "-y",
      "-i",
      input,
      "-vf",
      filter,
      "-c:v",
      "libx264",
      "-crf",
      "18",
      "-preset",
      "veryfast",
      "-pix_fmt",
      "yuv420p",
      "-c:a",
      "aac",
      "-b:a",
      "192k",
      out
    ]
  end

  @doc "Contents of an ffmpeg concat-demuxer list file for absolute paths."
  @spec concat_list([String.t()]) :: String.t()
  def concat_list(paths) do
    paths
    |> Enum.map(fn path -> "file '" <> String.replace(path, "'", "'\\''") <> "'\n" end)
    |> IO.iodata_to_binary()
  end

  @doc "Parse `ffprobe` duration output into seconds."
  @spec parse_duration(String.t()) :: {:ok, float()} | {:error, term()}
  def parse_duration(text) when is_binary(text) do
    case Float.parse(String.trim(text)) do
      {value, _rest} when value > 0 -> {:ok, value}
      _ -> {:error, :invalid_duration}
    end
  end

  def parse_duration(_text), do: {:error, :invalid_duration}

  @doc """
  Build subtitle cues from shot entries (`[%{\"shot\" => shot, \"duration\" =>
  seconds}]`), accumulating start times. Shots without dialogue are skipped.
  """
  @spec cues([map()]) :: [map()]
  def cues(entries) do
    {cues, _time} =
      Enum.reduce(entries, {[], 0.0}, fn entry, {acc, time} ->
        duration = entry["duration"] || 0.0

        acc =
          case dialogue(entry["shot"]) do
            nil -> acc
            text -> acc ++ [%{"start" => time, "end" => time + duration, "text" => text}]
          end

        {acc, time + duration}
      end)

    cues
  end

  @doc "Render cues as an SRT document."
  @spec srt([map()]) :: String.t()
  def srt(cues) do
    cues
    |> Enum.with_index(1)
    |> Enum.map_join("\n", fn {cue, index} ->
      "#{index}\n#{timestamp(cue["start"])} --> #{timestamp(cue["end"])}\n#{cue["text"]}\n"
    end)
  end

  @doc "Format seconds as an SRT timestamp (`HH:MM:SS,mmm`)."
  @spec timestamp(number()) :: String.t()
  def timestamp(seconds) when is_number(seconds) do
    total = round(seconds * 1000)
    hours = div(total, 3_600_000)
    minutes = total |> rem(3_600_000) |> div(60_000)
    secs = total |> rem(60_000) |> div(1000)
    millis = rem(total, 1000)

    :io_lib.format("~2..0B:~2..0B:~2..0B,~3..0B", [hours, minutes, secs, millis])
    |> IO.iodata_to_binary()
  end

  # --- helpers ---

  defp dialogue(shot) do
    meta = (shot && shot["meta"]) || %{}
    first_blank([meta["dialogue"], meta["台词"], shot && shot["shot_desc"]])
  end

  defp first_blank(values),
    do: Enum.find(values, &(is_binary(&1) and String.trim(&1) != ""))

  defp filename_stem(nil), do: nil
  defp filename_stem(filename), do: Path.rootname(filename)

  defp ensure_ext(filename) do
    if String.ends_with?(String.downcase(filename), ".mp4"),
      do: filename,
      else: filename <> ".mp4"
  end

  defp maybe_copy(src, dest) when src == dest, do: :ok
  defp maybe_copy(src, dest), do: File.cp(src, dest)

  defp format_seconds(seconds) when is_number(seconds),
    do: :erlang.float_to_binary(seconds * 1.0, decimals: 3)

  defp assembly_config, do: Config.get("assembly", %{}) || %{}
  defp ffmpeg_path, do: assembly_config()["ffmpeg_path"] || "ffmpeg"
  defp ffprobe_path, do: assembly_config()["ffprobe_path"] || "ffprobe"
end
