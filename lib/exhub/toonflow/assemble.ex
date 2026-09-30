defmodule Exhub.Toonflow.Assemble do
  @moduledoc """
  Episode assembly for Toonflow (Phase 3).

  `assemble/3` concatenates a script's shot clips (in storyboard order) into a
  single video under `output/`, optionally mixes per-shot voiceover, and writes
  an `.srt` subtitle track derived from each shot's dialogue. `export/3` wraps
  `assemble/3` and packages the result under a requested filename.

  The FFmpeg/FFprobe argv builders (`concat_argv/3`, `concat_audio_argv/3`,
  `mix_audio_argv/4`, `subtitle_argv/4`, `probe_argv/2`, `concat_list/1`,
  `srt/1`, `cues/1`, `parse_duration/1`, `timestamp/1`) are pure and
  unit-tested; execution shells out via `Exile`.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Config, Media, Script, Store, Storyboard}

  # ffmpeg audio filter that strips a clip's leading/trailing silence (keeping
  # 50 ms at each edge), inserts a short lead-in, then pads with silence — the
  # `-t <shot_duration>` output option cuts it back to the exact shot length.
  @voice_trim_filter "silenceremove=start_periods=1:start_silence=0.05:start_threshold=-45dB," <>
                       "areverse,silenceremove=start_periods=1:start_silence=0.05:start_threshold=-45dB,areverse"

  @voice_lead_in_ms 150

  @doc """
  Assemble a script's clips into an episode.

  `opts`: `:script_id` (default: the latest), `:scene` (limit to one scene),
  `:subtitles` (soft-mux the generated `.srt`; default from
  `assembly.subtitles`), `:mix_audio` (mix per-shot voiceover; default false),
  `:name` (artifact stem, default the script id). Every shot must already have
  a generated video clip.
  """
  @spec assemble(String.t(), keyword(), GenServer.server()) :: {:ok, map()} | {:error, term()}
  def assemble(project, opts \\ [], server \\ Store) do
    script_id = Toonflow.blank(Keyword.get(opts, :script_id))
    scene = Toonflow.blank(Keyword.get(opts, :scene))
    subtitles = Keyword.get(opts, :subtitles, assembly_config()["subtitles"] != false)
    mix_audio = Keyword.get(opts, :mix_audio, false)

    with {:ok, meta} <- Store.get_project(project, server),
         {:ok, script} <- fetch_script(project, script_id, server),
         {:ok, shots} <-
           Storyboard.list_shots(project, [script_id: script["id"], scene: scene], server),
         {:ok, clips} <- collect_clips(project, shots, server) do
      name = Toonflow.blank(Keyword.get(opts, :name)) || script["id"]
      build(project, meta["root_dir"], name, clips, subtitles, mix_audio, server)
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

  defp build(project, dir, name, clips, subtitles, mix_audio, server) do
    out_dir = Path.join(dir, "output")
    File.mkdir_p!(out_dir)

    list_path = Path.join(out_dir, ".#{name}.concat.txt")
    video_path = Path.join(out_dir, "#{name}.mp4")
    srt_path = Path.join(out_dir, "#{name}.srt")
    ffmpeg = ffmpeg_path()

    with :ok <- check_tool(ffmpeg, :ffmpeg),
         :ok <- File.write(list_path, concat_list(Enum.map(clips, & &1["path"]))),
         {:ok, _} <- run(concat_argv(ffmpeg, list_path, video_path)),
         {:ok, entries} <- with_durations(clips),
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
         "output_dir" => out_dir
       }}
    end
  end

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
         {:ok, _} <- run(mix_audio_argv(ffmpeg, video_path, audio_path, mixed)) do
      _ = File.rename(mixed, video_path)
      {video_path, segments}
    else
      {:error, reason} -> {video_path, {:skipped, reason}}
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

  @doc """
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
  ffmpeg argv to replace a video's audio track with `audio` (video copied).

  The explicit `-map` matters: without it ffmpeg's default stream selection
  picks the video's *own* audio (input 0) and silently drops the supplied track.
  """
  @spec mix_audio_argv(String.t(), String.t(), String.t(), String.t()) :: [String.t()]
  def mix_audio_argv(ffmpeg, video, audio, out),
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
