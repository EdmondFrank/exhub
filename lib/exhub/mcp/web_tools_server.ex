defmodule Exhub.MCP.WebToolsServer do
  @moduledoc """
  MCP Server for web search and web fetch functionality.

  This server exposes two tools via the Model Context Protocol:
  1. web_search - Search the web for information
  2. web_fetch - Fetch content from a specific URL

  The server uses HTTP transport and can be accessed at the /mcp/web endpoint.
  """

  use Anubis.Server,
    name: "exhub-web-tools-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Register the tool components
  component(Exhub.MCP.Tools.WebSearch)
  component(Exhub.MCP.Tools.WebFetch)

  @impl true
  def init(client_info, frame) do
    # You can also access client info to customize initialization
    # client_info contains: %{"name" => "client-name", "version" => "x.y.z"}
    _ = client_info

    {:ok, frame}
  end
end
