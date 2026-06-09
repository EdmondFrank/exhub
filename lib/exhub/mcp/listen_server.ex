defmodule Exhub.MCP.ListenServer do
  @moduledoc """
  MCP Server for audio transcription via Gitee AI / moark.com speech-to-text API.

  Exposes the `listen` tool which transcribes audio files to text using
  models like Whisper.

  The server uses HTTP transport and can be accessed at the /listen/mcp endpoint.
  """

  use Anubis.Server,
    name: "exhub-listen-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.Listen)

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
