defmodule Exhub.MCP.LlmServer do
  @moduledoc """
  MCP server exposing LangChain chat calls to agents.

  Mounted at `/llm/mcp` (streamable HTTP) and registered as the built-in server
  `llm`, so its tools are reachable both over HTTP and, locally, through
  `Exhub.MCP.Hub.BuiltInRegistry` (no HTTP hop). Because the `lua` server bridges
  every visible built-in tool, Lua code-mode snippets get the same surface for
  free:

      local out = llm.chat({model = "kimi-k2.6", prompt = "Say hi"})
      return out.content

  ## Tools

    * `models`    — configured LLM names, provider, and the current default
    * `chat`      — one LangChain chat turn
    * `chat_json` — one chat turn constrained by a JSON Schema

  All three delegate to `Exhub.Llm.Client`, which owns model resolution
  (full/bare/variant names), the TLS compat `req_config`, the token-pool key and
  the allow-list guard.
  """

  use Anubis.Server,
    name: "exhub-llm-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.Llm.Models)
  component(Exhub.MCP.Tools.Llm.Chat)
  component(Exhub.MCP.Tools.Llm.ChatJson)

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
