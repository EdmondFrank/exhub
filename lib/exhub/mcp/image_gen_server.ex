defmodule Exhub.MCP.ImageGenServer do
  @moduledoc """
  MCP Server for AI image generation via Gitee AI.

  Exposes the `image_gen` tool which generates images from text descriptions
  using the Gitee AI image generation API (OpenAI-compatible).

  Supported models: Qwen-Image, Kolors, GLM-Image, FLUX.2-dev,
  HunyuanDiT-v1.2-Diffusers-Distilled.

  The server uses HTTP transport and can be accessed at the /image-gen/mcp endpoint.
  """

  use Anubis.Server,
    name: "exhub-image-gen-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.ImageGen)

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end
end
