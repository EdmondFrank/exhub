defmodule Exhub.MCP.VideoGenServer do
  @moduledoc """
  MCP Server for AI video generation via Gitee AI / MoArk.

  Exposes the `video_gen` tool which generates videos from text descriptions
  (text-to-video) or first/last frame images using the MiniMax-H3 model on
  MoArk's async Serverless API. The tool submits the task and polls for the
  result, returning the video URL.

  The server uses HTTP transport and can be accessed at the /video-gen/mcp endpoint.
  """

  use Anubis.Server,
    name: "exhub-video-gen-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.VideoGen)

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
