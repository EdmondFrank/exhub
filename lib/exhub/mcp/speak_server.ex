defmodule Exhub.MCP.SpeakServer do
  @moduledoc """
  MCP Server for text-to-speech synthesis via Gitee AI / MoArk.

  Exposes the `speak` tool which synthesizes speech from text using the
  Qwen3-TTS model on MoArk's async Serverless API. The tool submits the text,
  polls for the result, and returns the generated audio URL (optionally saving
  the audio file locally).

  The server uses HTTP transport and can be accessed at the /speak/mcp endpoint.
  """

  use Anubis.Server,
    name: "exhub-speak-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.Speak)

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end

  @impl true
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end
