defmodule Exhub.RealtimeASR.TranscriberTest do
  use ExUnit.Case, async: true

  alias Exhub.RealtimeASR.Transcriber

  describe "to_pcm/2" do
    test "decodes a WAV file to 16 kHz mono s16le PCM (or reports ffmpeg missing)" do
      case System.find_executable("ffmpeg") do
        nil ->
          assert {:error, :ffmpeg_not_found} = Transcriber.to_pcm("/does/not/exist.wav")

        ffmpeg ->
          wav =
            Path.join(
              System.tmp_dir!(),
              "exhub_asr_test_#{System.unique_integer([:positive])}.wav"
            )

          {_, 0} =
            System.cmd(ffmpeg, [
              "-hide_banner",
              "-loglevel",
              "error",
              "-y",
              "-f",
              "lavfi",
              "-i",
              "sine=frequency=440:duration=1",
              "-ar",
              "44100",
              "-ac",
              "2",
              wav
            ])

          try do
            assert {:ok, pcm} = Transcriber.to_pcm(wav)
            assert byte_size(pcm) > 0
            # s16le → even byte count.
            assert rem(byte_size(pcm), 2) == 0
            # 1 s @ 16 kHz mono @ 16 bit ≈ 32 000 bytes.
            assert_in_delta byte_size(pcm), 32_000, 4_000
          after
            File.rm(wav)
          end
      end
    end

    test "returns an error tuple for a missing file" do
      assert match?({:error, _}, Transcriber.to_pcm("/does/not/exist.wav"))
    end
  end
end
