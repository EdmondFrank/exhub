defmodule Exhub.RealtimeASR.ClientTest do
  use ExUnit.Case, async: true

  alias Exhub.RealtimeASR.Client

  describe "default_model/0" do
    test "is the MoArk multi-dialect real-time ASR model" do
      assert Client.default_model() == "TeleASR-MultiDialect"
    end
  end

  describe "websocket_url/2" do
    test "builds the documented MoArk URL with the model in the query" do
      assert Client.websocket_url("TeleASR-MultiDialect") ==
               "wss://api.moark.com/v1/audio/transcriptions/ws?model=TeleASR-MultiDialect"
    end

    test "encodes the model name" do
      assert Client.websocket_url("a b") ==
               "wss://api.moark.com/v1/audio/transcriptions/ws?model=a+b"
    end

    test "honours host and path overrides" do
      url = Client.websocket_url("m", host: "localhost", path: "/ws")
      assert url == "wss://localhost/ws?model=m"
    end
  end

  describe "subprotocols/1" do
    test "uses OpenAI realtime subprotocol names and carries the token last" do
      assert Client.subprotocols("abc123") == [
               "realtime",
               "openai-beta.realtime-v1",
               "openai-insecure-api-key.abc123"
             ]
    end
  end

  describe "start_message/2" do
    test "has the documented shape and PCM input descriptor" do
      message = Client.start_message("TeleASR-MultiDialect")

      assert %{
               "type" => "start",
               "model" => "TeleASR-MultiDialect",
               "payload" => %{
                 "input" => %{
                   "type" => "audio",
                   "format" => "pcm_s16le",
                   "sample_rate" => 16_000,
                   "channels" => 1
                 },
                 "options" => opts
               }
             } = message

      assert opts["enable_punctuation"] == true
      assert opts["enable_inverse_text_normalization"] == true
      assert opts["enable_emendation"] == false
      assert opts["enable_words"] == false
      assert opts["max_end_silence"] == 10
    end

    test "merges option overrides over the defaults" do
      message = Client.start_message("m", options: %{"enable_words" => true, "extra" => 1})
      assert message["payload"]["options"]["enable_words"] == true
      assert message["payload"]["options"]["extra"] == 1
      assert message["payload"]["options"]["enable_punctuation"] == true
    end
  end
end
