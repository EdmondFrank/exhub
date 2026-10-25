defmodule Exhub.TTS.RealtimeTest do
  use ExUnit.Case, async: true

  alias Exhub.TTS.Realtime

  describe "defaults" do
    test "model, voice, tuning and predicates" do
      assert Realtime.default_model() == "TeleTTS-Bidirectional-Mandarin"
      assert Realtime.default_voice() == "guangzhounv"
      assert Realtime.default_sample_rate() == 24_000
      assert Realtime.default_volume() == 50
      assert Realtime.default_speech_rate() == 1

      assert Realtime.model?("TeleTTS-MultiDialect")
      assert Realtime.model?("TeleTTS-Bidirectional-Mandarin")
      assert Realtime.model?("TeleTTS-Mandarin")
      refute Realtime.model?("Step-Audio-TTS-3B")

      assert Realtime.voiceless_model?("TeleTTS-Mandarin")
      refute Realtime.voiceless_model?("TeleTTS-Bidirectional-Mandarin")

      assert Realtime.voice?("guangzhounv")
      assert Realtime.voice?("lucas")
      refute Realtime.voice?("alloy")
      assert "guangzhounv" in Realtime.voices()
      assert length(Realtime.voices()) == 10
    end
  end

  describe "websocket_url/2" do
    test "builds the model-scoped wss URL" do
      assert Realtime.websocket_url("TeleTTS-Mandarin") ==
               "wss://api.moark.com/v1/audio/speech/ws?model=TeleTTS-Mandarin"
    end

    test "honours host/path overrides" do
      url = Realtime.websocket_url("m", host: "example.com", path: "/ws")
      assert url == "wss://example.com/ws?model=m"
    end
  end

  describe "subprotocols/1" do
    test "carries the token in the last subprotocol" do
      assert Realtime.subprotocols("tok") == [
               "realtime",
               "openai-beta.realtime-v1",
               "openai-insecure-api-key.tok"
             ]
    end
  end

  describe "build_start_message/2" do
    test "applies the model/voice/sample-rate defaults" do
      assert Realtime.build_start_message("你好") == %{
               "type" => "start",
               "model" => "TeleTTS-Bidirectional-Mandarin",
               "payload" => %{
                 "input" => %{"type" => "text", "text" => "你好"},
                 "output" => %{"type" => "audio", "format" => "PCM", "sample_rate" => 24_000},
                 "options" => %{"voice" => "guangzhounv"}
               }
             }
    end

    test "overrides model, voice, sample rate and tuning" do
      message =
        Realtime.build_start_message("hi",
          model: "TeleTTS-MultiDialect",
          voice: "lucas",
          volume: 30,
          speech_rate: 1.5,
          sample_rate: 16_000
        )

      assert message["model"] == "TeleTTS-MultiDialect"
      assert message["payload"]["output"]["sample_rate"] == 16_000

      assert message["payload"]["options"] == %{
               "voice" => "lucas",
               "volume" => 30,
               "speech_rate" => 1.5
             }
    end

    test "omits the voice for a voiceless model" do
      message =
        Realtime.build_start_message("hi", model: "TeleTTS-Mandarin", voice: "guangzhounv")

      refute Map.has_key?(message["payload"]["options"], "voice")
    end

    test "a blank voice falls back to the default" do
      message = Realtime.build_start_message("hi", voice: "   ")
      assert message["payload"]["options"]["voice"] == "guangzhounv"
    end
  end

  describe "wav/1" do
    test "wraps PCM in a 24 kHz mono s16le RIFF/WAVE header" do
      assert Realtime.wav(<<1, 2, 3, 4>>) ==
               <<"RIFF", 40::little-32, "WAVE", "fmt ", 16::little-32, 1::little-16, 1::little-16,
                 24_000::little-32, 48_000::little-32, 2::little-16, 16::little-16, "data",
                 4::little-32, 1, 2, 3, 4>>
    end

    test "honours a custom sample rate and channel count" do
      wav = Realtime.wav(<<0, 0>>, 16_000, 2)

      assert <<"RIFF", _size::little-32, "WAVE", "fmt ", 16::little-32, 1::little-16,
               2::little-16, 16_000::little-32, 64_000::little-32, 4::little-16, 16::little-16,
               "data", 2::little-32, 0, 0>> = wav
    end

    test "an empty clip still yields a valid header" do
      assert <<"RIFF", 36::little-32, "WAVE", _rest::binary>> = Realtime.wav(<<>>)
      assert byte_size(Realtime.wav(<<>>)) == 44
    end
  end
end
