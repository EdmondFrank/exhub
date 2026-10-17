defmodule Exhub.RealtimeASR.HandlerTest do
  use ExUnit.Case, async: true
  import Plug.Test

  alias Exhub.RealtimeASR.Handler

  describe "supported_models/0" do
    test "lists the WebSocket-only and HTTP models" do
      models = Handler.supported_models()
      assert "TeleASR-MultiDialect" in models
      assert "whisper-large-v3-turbo" in models
      assert "whisper-large-v3" in models
    end
  end

  describe "call/1" do
    test "returns an OpenAI-shaped 400 when no file is uploaded" do
      conn = Handler.call(conn(:post, "/openai/v1/audio/transcriptions", ""))

      assert conn.status == 400
      assert %{"error" => %{"message" => message}} = Jason.decode!(conn.resp_body)
      assert message =~ "`file` is required"
    end
  end
end
