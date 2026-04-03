defmodule Exhub.MCP.BrowserUseServer do
  @moduledoc """
  MCP Server for browser automation via kuri-agent.

  Exposes a set of focused tools that drive Chrome via CDP using the
  `kuri-agent` CLI binary. Supports tab management, navigation, page
  inspection, element interaction, security testing, and auth headers.

  The server uses HTTP transport and can be accessed at the /browser-use/mcp endpoint.

  ## Tools

  - `browser_tabs`         — Discover tabs, attach session (tabs / use / status)
  - `browser_navigate`     — Navigate the browser (go / back / forward / reload)
  - `browser_inspect`      — Inspect page content (snap / text / eval / shot)
  - `browser_interact`     — Interact with elements (click / type / fill / select / hover / focus / scroll)
  - `browser_security`     — Security testing (cookies / headers / audit / storage / jwt / fetch / probe)
  - `browser_auth_headers` — Manage persistent auth headers (set_header / show_headers / clear_headers)
  """

  use Anubis.Server,
    name: "exhub-browser-use-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.BrowserUse.Tabs)
  component(Exhub.MCP.Tools.BrowserUse.Navigate)
  component(Exhub.MCP.Tools.BrowserUse.Inspect)
  component(Exhub.MCP.Tools.BrowserUse.Interact)
  component(Exhub.MCP.Tools.BrowserUse.Security)
  component(Exhub.MCP.Tools.BrowserUse.AuthHeaders)

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end
end
