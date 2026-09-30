defmodule Exhub.TTS.SyncTest do
  use ExUnit.Case, async: true

  alias Exhub.TTS.Sync

  describe "defaults" do
    test "model, voice and predicates" do
      assert Sync.default_model() == "CosyVoice2"
      assert Sync.default_voice() == "alloy"
      assert Sync.model?("CosyVoice2")
      refute Sync.model?("CosyVoice3")
      refute Sync.model?("Qwen3-TTS")
      assert Sync.clone_model?("IndexTTS-2")
      assert Sync.clone_model?("GLM-TTS")
      refute Sync.clone_model?("CosyVoice2")
    end
  end

  describe "build_body/2" do
    test "applies the model/voice defaults" do
      assert Sync.build_body("你好") == %{
               "model" => "CosyVoice2",
               "input" => "你好",
               "voice" => "alloy"
             }
    end

    test "overrides model and voice" do
      b = Sync.build_body("hi", model: "ChatTTS", voice: "serena")
      assert b["model"] == "ChatTTS"
      assert b["voice"] == "serena"
    end

    test "adds the clone fields only when non-blank" do
      b =
        Sync.build_body("hi",
          model: "IndexTTS-2",
          prompt_audio_url: "https://example.com/ref.wav",
          prompt_text: "words"
        )

      assert b["prompt_audio_url"] == "https://example.com/ref.wav"
      assert b["prompt_text"] == "words"

      plain = Sync.build_body("hi", prompt_audio_url: "  ", prompt_text: nil)
      refute Map.has_key?(plain, "prompt_audio_url")
      refute Map.has_key?(plain, "prompt_text")
    end
  end

  describe "detect_format/1" do
    test "recognizes WAV (RIFF/WAVE)" do
      assert Sync.detect_format("RIFF" <> <<0, 0, 0, 0>> <> "WAVEfmt ") == "wav"
    end

    test "recognizes MP3 (ID3 tag and frame sync)" do
      assert Sync.detect_format("ID3\x04\x00") == "mp3"
      assert Sync.detect_format(<<0xFF, 0xFB, 0x90, 0x00>>) == "mp3"
      assert Sync.detect_format(<<0xFF, 0xF3, 0x80, 0xC4>>) == "mp3"
    end

    test "recognizes Ogg and FLAC" do
      assert Sync.detect_format("OggS" <> <<0, 0, 0, 0>>) == "ogg"
      assert Sync.detect_format("fLaC" <> <<0, 0, 0, 0>>) == "flac"
    end

    test "returns nil for unknown bytes" do
      assert Sync.detect_format("not audio") == nil
      assert Sync.detect_format(<<0, 1, 2, 3>>) == nil
    end
  end

  describe "with_format/2" do
    test "leaves a matching extension (case-insensitively)" do
      assert Sync.with_format("/tmp/a.wav", "wav") == "/tmp/a.wav"
      assert Sync.with_format("/tmp/a.WAV", "wav") == "/tmp/a.WAV"
    end

    test "rewrites a mismatched extension" do
      assert Sync.with_format("/tmp/a.mp3", "wav") == "/tmp/a.wav"
      assert Sync.with_format("/tmp/a.wav", "mp3") == "/tmp/a.mp3"
    end

    test "leaves the path alone for an unknown format" do
      assert Sync.with_format("/tmp/a.mp3", nil) == "/tmp/a.mp3"
      assert Sync.with_format("/tmp/a.mp3", "bin") == "/tmp/a.mp3"
    end
  end
end
