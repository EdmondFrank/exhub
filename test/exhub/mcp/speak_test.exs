defmodule Exhub.MCP.Tools.SpeakTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Speak

  describe "validate_params/1" do
    test "requires text" do
      assert {:error, msg} = Speak.validate_params(%{})
      assert msg =~ "`text` is required"

      assert {:error, _} = Speak.validate_params(%{text: "   "})
    end

    test "rejects an unknown provider" do
      assert {:error, msg} = Speak.validate_params(%{text: "hi", provider: "nope"})
      assert msg =~ "Invalid provider"
    end

    test "defaults to the sync provider with CosyVoice2" do
      assert {:ok, v} = Speak.validate_params(%{text: "hello"})
      assert v.provider == "sync"
      assert v.model == "CosyVoice2"
      assert v.voice == "alloy"
    end

    test "rejects an unknown sync model" do
      assert {:error, msg} = Speak.validate_params(%{text: "hi", model: "nope"})
      assert msg =~ "Invalid model"
    end

    test "accepts sync voice and clone params" do
      assert {:ok, v} =
               Speak.validate_params(%{
                 text: "hi",
                 model: "IndexTTS-2",
                 voice: "serena",
                 prompt_audio_url: "https://example.com/ref.wav",
                 prompt_text: "reference words"
               })

      assert v.model == "IndexTTS-2"
      assert v.voice == "serena"
      assert v.prompt_audio_url == "https://example.com/ref.wav"
      assert v.prompt_text == "reference words"
    end

    test "sync clone models require prompt_audio_url" do
      assert {:error, msg} = Speak.validate_params(%{text: "hi", model: "IndexTTS-2"})
      assert msg =~ "requires `prompt_audio_url`"
    end

    test "sync prompt_audio_url must be an http(s) URL" do
      assert {:error, msg} =
               Speak.validate_params(%{
                 text: "hi",
                 model: "GLM-TTS",
                 prompt_audio_url: "/tmp/a.wav"
               })

      assert msg =~ "`prompt_audio_url` must be an http(s) URL"
    end

    test "async applies the legacy defaults" do
      assert {:ok, v} = Speak.validate_params(%{text: "hello", provider: "async"})
      assert v.provider == "async"
      assert v.model == "Qwen3-TTS"
      assert v.speaker == "Vivian"
      assert v.output_format == "mp3"
      assert v.ref_audio == nil
    end

    test "async rejects an unknown model" do
      assert {:error, msg} =
               Speak.validate_params(%{text: "hi", provider: "async", model: "nope"})

      assert msg =~ "Invalid model"
    end

    test "async accepts an explicit speaker, language and instruction" do
      assert {:ok, v} =
               Speak.validate_params(%{
                 text: "hello",
                 provider: "async",
                 speaker: "Serena",
                 language: "English",
                 instruction: "calm narrator"
               })

      assert v.speaker == "Serena"
      assert v.language == "English"
      assert v.instruction == "calm narrator"
    end

    test "async requires ref_audio to be an http(s) URL" do
      assert {:error, msg} =
               Speak.validate_params(%{text: "hi", provider: "async", ref_audio: "/tmp/a.wav"})

      assert msg =~ "http(s) URL"
    end

    test "async requires ref_text alongside ref_audio" do
      assert {:error, msg} =
               Speak.validate_params(%{
                 text: "hi",
                 provider: "async",
                 ref_audio: "https://example.com/a.wav"
               })

      assert msg =~ "`ref_text` is required"
    end

    test "async accepts clone mode when ref_audio and ref_text are given" do
      assert {:ok, v} =
               Speak.validate_params(%{
                 text: "hi",
                 provider: "async",
                 ref_audio: "https://example.com/a.wav",
                 ref_text: "reference words"
               })

      assert v.ref_audio == "https://example.com/a.wav"
      assert v.ref_text == "reference words"
    end
  end

  describe "build_body/1" do
    defp body(params) do
      {:ok, validated} = Speak.validate_params(params)
      Speak.build_body(validated)
    end

    test "sync mode builds the OpenAI-compatible speech body" do
      b = body(%{text: "hello"})
      assert b == %{"model" => "CosyVoice2", "input" => "hello", "voice" => "alloy"}

      b2 = body(%{text: "hi", model: "ChatTTS", voice: "serena"})
      assert b2["model"] == "ChatTTS"
      assert b2["voice"] == "serena"
      refute Map.has_key?(b2, "prompt_audio_url")
    end

    test "sync clone mode adds prompt_audio_url / prompt_text only when set" do
      b =
        body(%{
          text: "hi",
          model: "IndexTTS-2",
          prompt_audio_url: "https://example.com/ref.wav",
          prompt_text: "reference words"
        })

      assert b["prompt_audio_url"] == "https://example.com/ref.wav"
      assert b["prompt_text"] == "reference words"
      refute Map.has_key?(b, "inputs")
    end

    test "async design mode sends a single input with speaker" do
      b = body(%{text: "hello", provider: "async", language: "Chinese"})

      assert b["model"] == "Qwen3-TTS"
      assert b["output_format"] == "mp3"

      assert [%{"prompt" => "hello", "speaker" => "Vivian", "language" => "Chinese"} = item] =
               b["inputs"]

      refute Map.has_key?(item, "prompt_audio_url")
    end

    test "async includes instruction in design mode only when set" do
      b = body(%{text: "hello", provider: "async", instruction: "soft whisper"})
      assert [%{"instruction" => "soft whisper"}] = b["inputs"]

      b2 = body(%{text: "hello", provider: "async"})
      assert [item] = b2["inputs"]
      refute Map.has_key?(item, "instruction")
    end

    test "async clone mode sends prompt_text/prompt_audio_url and no speaker" do
      b =
        body(%{
          text: "hello",
          provider: "async",
          ref_audio: "https://example.com/a.wav",
          ref_text: "reference words"
        })

      assert [
               %{
                 "prompt" => "hello",
                 "prompt_text" => "reference words",
                 "prompt_audio_url" => "https://example.com/a.wav"
               } = item
             ] = b["inputs"]

      refute Map.has_key?(item, "speaker")
    end
  end

  describe "interpret_poll/1" do
    test "pending while queued or running" do
      assert :pending == Speak.interpret_poll(%{"status" => "waiting"})
      assert :pending == Speak.interpret_poll(%{"status" => "in_progress"})
    end

    test "ok on success" do
      assert {:ok, %{"output" => %{"file_url" => "https://x/y.mp3"}}} =
               Speak.interpret_poll(%{
                 "status" => "success",
                 "output" => %{"file_url" => "https://x/y.mp3"}
               })
    end

    test "error on failure/cancelled" do
      assert {:error, msg} = Speak.interpret_poll(%{"status" => "failure"})
      assert msg =~ "failure"

      assert {:error, _} = Speak.interpret_poll(%{"status" => "cancelled"})
    end

    test "error when the payload carries an error" do
      assert {:error, msg} =
               Speak.interpret_poll(%{"error" => "bad_request", "message" => "invalid token"})

      assert msg =~ "bad_request"
      assert msg =~ "invalid token"
    end

    test "surfaces a nested output error on failure" do
      assert {:error, msg} =
               Speak.interpret_poll(%{
                 "status" => "failure",
                 "output" => %{"error" => "An unexpected error has occurred"}
               })

      assert msg =~ "unexpected error"
    end
  end

  describe "extract_audio_urls/1" do
    test "reads the Qwen3-TTS output.result[].audio_urls[].url shape" do
      output = %{
        "result" => [
          %{
            "audio_urls" => [%{"content_type" => "audio/mp3", "url" => "https://x/a.mp3"}],
            "count" => 1
          }
        ]
      }

      assert Speak.extract_audio_urls(output) == ["https://x/a.mp3"]
    end

    test "reads the generic output.file_url shape" do
      assert Speak.extract_audio_urls(%{"file_url" => "https://x/b.mp3"}) == ["https://x/b.mp3"]
    end

    test "returns all urls across segments, de-duplicated" do
      output = %{
        "result" => [
          %{"audio_urls" => [%{"url" => "https://x/a.mp3"}, %{"url" => "https://x/b.mp3"}]},
          %{"audio_urls" => [%{"url" => "https://x/a.mp3"}]}
        ]
      }

      assert Speak.extract_audio_urls(output) == ["https://x/a.mp3", "https://x/b.mp3"]
    end

    test "returns an empty list when there is no audio" do
      assert Speak.extract_audio_urls(%{}) == []
      assert Speak.extract_audio_urls(nil) == []
    end
  end

  describe "chunk_text/1" do
    test "keeps text at or below the limit as a single segment" do
      assert Speak.chunk_text("你好，世界") == ["你好，世界"]
      assert Speak.chunk_text(String.duplicate("a", 150)) == [String.duplicate("a", 150)]
    end

    test "splits longer text into segments of at most 150 chars, losslessly" do
      poem = """
      我说你是人间的四月天；
      笑响点亮了四面风；
      轻灵在春的光艳中交舞着变。

      你是四月早天里的云烟，
      黄昏吹着风的软，
      星子在无意中闪，细雨点洒在花前。

      那轻，那娉婷，你是，
      鲜妍百花的冠冕你戴着，
      你是天真，庄严，你是夜夜的月圆。

      雪化后那片鹅黄，你像；
      新鲜初放芽的绿，你是；
      柔嫩喜悦，水光浮动着你梦期待中白莲。

      你是一树一树的花开，
      是燕在梁间呢喃，
      ——你是爱，是暖，是希望，
      你是人间的四月天！
      """

      chunks = Speak.chunk_text(poem)

      assert length(chunks) > 1
      assert Enum.all?(chunks, &(String.length(&1) <= 150))
      assert Enum.join(chunks) == poem
    end

    test "prefers a newline boundary" do
      text = String.duplicate("a", 100) <> "\n" <> String.duplicate("b", 100)
      assert [first, second] = Speak.chunk_text(text)
      assert first == String.duplicate("a", 100) <> "\n"
      assert second == String.duplicate("b", 100)
    end

    test "falls back to a hard split when there is no break point" do
      text = String.duplicate("a", 400)
      chunks = Speak.chunk_text(text)
      assert Enum.map(chunks, &String.length/1) == [150, 150, 100]
      assert Enum.join(chunks) == text
    end

    test "never splits a multibyte grapheme" do
      text = String.duplicate("汉字", 100)
      chunks = Speak.chunk_text(text)
      assert Enum.all?(chunks, &(String.length(&1) <= 150))
      assert Enum.join(chunks) == text
    end

    test "drops whitespace-only input" do
      assert Speak.chunk_text("   \n  ") == []
    end
  end

  describe "concat_wav/1" do
    test "joins the data chunks and keeps a valid RIFF/WAVE header" do
      {:ok, out} = Speak.concat_wav([wav(<<0, 1, 2, 3>>), wav(<<4, 5>>)])

      assert <<"RIFF", _::little-32, "WAVE", "fmt ", 16::little-32, _fmt::binary-size(16), "data",
               size::little-32, data::binary>> = out

      assert size == 6
      assert data == <<0, 1, 2, 3, 4, 5>>
    end

    test "handles odd-sized data with RIFF padding" do
      {:ok, out} = Speak.concat_wav([wav(<<1, 2, 3>>), wav(<<4>>)])

      assert <<"RIFF", _::little-32, "WAVE", "fmt ", 16::little-32, _::binary-size(16), "data",
               size::little-32, data::binary-size(4)>> = out

      assert size == 4
      assert data == <<1, 2, 3, 4>>
    end

    test "a single input round-trips" do
      original = wav(<<9, 8, 7, 6>>)
      assert {:ok, ^original} = Speak.concat_wav([original])
    end

    test "rejects non-WAV input" do
      assert {:error, msg} = Speak.concat_wav([<<"not a wav">>])
      assert msg =~ "invalid WAV"
      assert {:error, _} = Speak.concat_wav([])
    end
  end

  # Minimal canonical 16-bit mono WAV for concatenation tests.
  defp wav(data) do
    pad = if rem(byte_size(data), 2) == 1, do: <<0>>, else: <<>>

    fmt =
      <<1::little-16, 1::little-16, 24_000::little-32, 48_000::little-32, 2::little-16,
        16::little-16>>

    body =
      "WAVE" <>
        "fmt " <>
        <<16::little-32>> <> fmt <> "data" <> <<byte_size(data)::little-32>> <> data <> pad

    "RIFF" <> <<byte_size(body)::little-32>> <> body
  end
end
