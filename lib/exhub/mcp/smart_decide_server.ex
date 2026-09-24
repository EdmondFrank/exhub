defmodule Exhub.MCP.SmartDecideServer do
  @moduledoc """
  MCP Server for System One structured decision making via Gitee AI / moark.com.

  Exposes the `smart_decide` tool, which evaluates a piece of state against a
  set of typed questions and returns one structured answer per question. Backed
  by the Jev-compatible `POST /v1/systemone` endpoint, served by the
  `APUS-OpenJev-v1-9B` model (8k-token context) by default.

  The server uses HTTP transport and can be accessed at the /smart-decide/mcp
  endpoint.
  """

  use Anubis.Server,
    name: "exhub-smart-decide-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.SmartDecide)

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
