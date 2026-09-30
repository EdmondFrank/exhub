defmodule Exhub.Toonflow.AssembleTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Assemble

  test "concat_argv builds an ffmpeg concat-demuxer command" do
    assert Assemble.concat_argv("ffmpeg", "/o/l.txt", "/o/a.mp4") ==
             [
               "ffmpeg",
               "-y",
               "-f",
               "concat",
               "-safe",
               "0",
               "-i",
               "/o/l.txt",
               "-c",
               "copy",
               "/o/a.mp4"
             ]
  end

  test "concat_audio_argv re-encodes to AAC" do
    assert Assemble.concat_audio_argv("ffmpeg", "/o/l.txt", "/o/a.m4a") ==
             [
               "ffmpeg",
               "-y",
               "-f",
               "concat",
               "-safe",
               "0",
               "-i",
               "/o/l.txt",
               "-c:a",
               "aac",
               "-b:a",
               "192k",
               "/o/a.m4a"
             ]
  end

  test "mix_audio_argv maps the video + supplied audio track explicitly" do
    assert Assemble.mix_audio_argv("ffmpeg", "/o/v.mp4", "/o/a.m4a", "/o/m.mp4") ==
             [
               "ffmpeg",
               "-y",
               "-i",
               "/o/v.mp4",
               "-i",
               "/o/a.m4a",
               "-map",
               "0:v:0",
               "-map",
               "1:a:0",
               "-c:v",
               "copy",
               "-c:a",
               "aac",
               "-shortest",
               "/o/m.mp4"
             ]
  end

  test "voice_segment_argv trims silence and pads to the shot duration" do
    assert Assemble.voice_segment_argv("ffmpeg", "/o/v.wav", 6.583333, "/o/seg0.m4a") ==
             [
               "ffmpeg",
               "-y",
               "-i",
               "/o/v.wav",
               "-af",
               "silenceremove=start_periods=1:start_silence=0.05:start_threshold=-45dB," <>
                 "areverse,silenceremove=start_periods=1:start_silence=0.05:start_threshold=-45dB," <>
                 "areverse,adelay=150:all=1,apad",
               "-t",
               "6.583",
               "-c:a",
               "aac",
               "-b:a",
               "192k",
               "/o/seg0.m4a"
             ]
  end

  test "voice_segment_argv omits -t when the duration is unknown" do
    argv = Assemble.voice_segment_argv("ffmpeg", "/o/v.wav", 0.0, "/o/seg0.m4a")
    refute "-t" in argv
  end

  test "subtitle_argv muxes an srt as mov_text" do
    assert Assemble.subtitle_argv("ffmpeg", "/o/v.mp4", "/o/s.srt", "/o/o.mp4") ==
             [
               "ffmpeg",
               "-y",
               "-i",
               "/o/v.mp4",
               "-i",
               "/o/s.srt",
               "-c",
               "copy",
               "-c:s",
               "mov_text",
               "-metadata:s:s:0",
               "language=chi",
               "/o/o.mp4"
             ]
  end

  test "probe_argv reads the duration" do
    assert Assemble.probe_argv("ffprobe", "/o/v.mp4") ==
             [
               "ffprobe",
               "-v",
               "error",
               "-show_entries",
               "format=duration",
               "-of",
               "default=nw=1:nk=1",
               "/o/v.mp4"
             ]
  end

  test "concat_list quotes paths and escapes single quotes" do
    assert Assemble.concat_list(["/a/b.mp4", "/a/it's.mp4"]) ==
             "file '/a/b.mp4'\nfile '/a/it'\\''s.mp4'\n"
  end

  test "parse_duration accepts a positive float and rejects junk" do
    assert Assemble.parse_duration("6.500000\n") == {:ok, 6.5}
    assert Assemble.parse_duration("N/A") == {:error, :invalid_duration}
    assert Assemble.parse_duration("0") == {:error, :invalid_duration}
    assert Assemble.parse_duration(nil) == {:error, :invalid_duration}
  end

  test "timestamp formats SRT timestamps" do
    assert Assemble.timestamp(0) == "00:00:00,000"
    assert Assemble.timestamp(65.25) == "00:01:05,250"
    assert Assemble.timestamp(3661.5) == "01:01:01,500"
  end

  test "srt renders a numbered cue list" do
    assert Assemble.srt([%{"start" => 0.0, "end" => 1.5, "text" => "你好"}]) ==
             "1\n00:00:00,000 --> 00:00:01,500\n你好\n"
  end

  test "cues accumulate start times and skip blank dialogue" do
    entries = [
      %{"shot" => %{"meta" => %{"dialogue" => "a"}, "shot_desc" => "d"}, "duration" => 2.0},
      %{"shot" => %{"meta" => %{}, "shot_desc" => "旁白"}, "duration" => 3.0},
      %{"shot" => %{"meta" => %{"dialogue" => "  "}, "shot_desc" => nil}, "duration" => 1.0}
    ]

    assert Assemble.cues(entries) == [
             %{"start" => 0.0, "end" => 2.0, "text" => "a"},
             %{"start" => 2.0, "end" => 5.0, "text" => "旁白"}
           ]
  end
end
