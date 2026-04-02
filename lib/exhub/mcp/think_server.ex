defmodule Exhub.MCP.ThinkServer do
  @moduledoc """
  MCP Server for think and plan functionality.

  This server exposes two tools via the Model Context Protocol:
  1. think - Record thoughts for complex reasoning
  2. plan - Plan steps for complex reasoning

  The server uses HTTP transport and can be accessed at the /think/mcp endpoint.
  """

  use Anubis.Server,
    name: "exhub-think-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Register the tool components
  component(Exhub.MCP.Tools.Think)
  component(Exhub.MCP.Tools.Plan)

  @impl true
  def init(client_info, frame) do
    _ = client_info

    {:ok, frame}
  end
end
