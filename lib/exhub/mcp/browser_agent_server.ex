defmodule Exhub.MCP.BrowserAgentServer do
  @moduledoc """
  MCP Server for the Jev-style browser agent.

  Exposes the `browser_agent` tool, which combines ExHub's `kuri-agent` browser
  automation with the Smart Decide (System One) decision model: it observes the
  attached tab, chooses an operation and a target in one request, and executes
  only the chosen operation. See `Exhub.MCP.Tools.BrowserAgent`.

  The server uses HTTP transport and can be accessed at the /browser-agent/mcp
  endpoint.
  """

  use Anubis.Server,
    name: "exhub-browser-agent-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.BrowserAgent)

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
