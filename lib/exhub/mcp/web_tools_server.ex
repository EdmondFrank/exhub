defmodule Exhub.MCP.WebToolsServer do
  @moduledoc """
  MCP Server for web search and web fetch functionality.

  This server exposes two tools via the Model Context Protocol:
  1. web_search - Search the web for information
  2. web_fetch - Fetch content from a specific URL

  The server uses HTTP transport and can be accessed at the /mcp/web endpoint.

  ## Relevance filtering

  `web_search` results are filtered for relevance by the Smart Decide
  (System One) model: each candidate page is judged with a single yes/no
  (`noul`) question and only the relevant ones are kept (on by default;
  `web_search.filter: false` disables it). See
  `Exhub.MCP.WebTools.Relevance`.

  ## Proxy decision

  `web_fetch` does not attach the configured `:exhub, :proxy` to every request.
  It starts direct, and a failure shaped like a blocked route is escalated to
  one Smart Decide call that may approve a single retry through a proxy it
  judged reachable and worth the leak risk — the same gate the Desktop shell
  tools use, so the verdict cache is shared. See `Exhub.MCP.WebTools.Proxy`.

  ## See Also
  - `Exhub.MCP.WebTools.Relevance` — Smart Decide relevance filter over
    web search results
  - `Exhub.MCP.WebTools.Proxy` — Smart Decide proxy decision for outbound
    `web_fetch` requests (`Exhub.MCP.Desktop.ProxyEnv` does the judging)
  - `docs/modules/web-tools.md` — full user-facing documentation
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

  @impl true
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end
