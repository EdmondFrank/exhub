defmodule Exhub.MCP.LookServer do
  @moduledoc """
  MCP Server for image understanding via Gitee AI vision models.

  Exposes the `look` tool which analyzes images using vision-capable
  models (Qwen-VL, GLM-4V, deepseek-vl, etc.).

  Supports both local file paths and remote URLs as input.

  The server uses HTTP transport and can be accessed at the /look/mcp endpoint.
  """

  use Anubis.Server,
    name: "exhub-look-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.Look)

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end
end
