defmodule Exhub.MCP.LuaServer do
  @moduledoc """
  MCP server exposing the Lua code-mode sandbox.

  Mounted at `/lua/mcp` (streamable HTTP) and registered as the built-in server
  `lua`, so its tool is reachable both over HTTP and, locally, through
  `Exhub.MCP.Hub.BuiltInRegistry` (no HTTP hop). The evaluation itself is the
  transport-agnostic `Exhub.Lua.Engine`; see `docs/modules/lua.md`.

  ## Tools

    * `code_mode` — run a Lua 5.3 snippet with every visible MCP tool bridged in
      as a function (see `Exhub.MCP.Tools.Lua.CodeMode`).

  The `lua` server and the `mcp-hub` meta server are excluded from the bridged
  tool set (config `exclude_servers`), so a script can neither recurse into the
  hub nor call back into `code_mode`.
  """

  use Anubis.Server,
    name: "exhub-lua-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.Lua.CodeMode)
  component(Exhub.MCP.Tools.Lua.Help)

  @impl Anubis.Server
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end

  @impl Anubis.Server
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end
