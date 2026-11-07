defmodule Exhub.MCP.WebTermServer do
  @moduledoc """
  MCP Server for driving a browser-hosted web terminal.

  Exposes the `webterm` tool, which talks to the `chrome` upstream MCP server
  (Browser MCP extension) to paste quote-safe commands into an xterm.js
  terminal, run them with a sentinel, and read the screen back through the
  `look` vision model.

  The server uses HTTP transport and can be accessed at the /webterm/mcp endpoint.
  """

  use Anubis.Server,
    name: "exhub-webterm-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.WebTerm)

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
