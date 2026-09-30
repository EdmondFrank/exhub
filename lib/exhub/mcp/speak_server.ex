defmodule Exhub.MCP.SpeakServer do
  @moduledoc """
  MCP Server for text-to-speech synthesis via Gitee AI / MoArk.

  Exposes the `speak` tool. By default it synthesizes speech synchronously with
  `CosyVoice2` on Gitee AI's OpenAI-compatible `/v1/audio/speech` endpoint
  (audio bytes returned directly). With `provider: "async"` it falls back to the
  legacy MoArk `Qwen3-TTS` flow (submit + poll), returning the generated audio
  URL.

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
