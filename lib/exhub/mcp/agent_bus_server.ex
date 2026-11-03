defmodule Exhub.MCP.AgentBusServer do
  @moduledoc """
  MCP server exposing the agent bus: participant identity, direct messages,
  rooms and fan-in waits.

  Mounted at `/agent-bus/mcp` — deliberately separate from `/agent/mcp`
  (lifecycle + ACP control) so a subagent can be handed this endpoint without
  also being able to spawn agents, grant permissions or shut anything down.

  ## Tools

  - `agent_register` — create/update my participant record
  - `agent_identity` — who am I (auto-registers on first contact)
  - `agent_list` — participants, status, unread
  - `agent_send` — direct message (`ask` / `tell` / `reply` / `handoff`)
  - `agent_inbox` — drain my unread, optionally long-poll
  - `agent_await` — wait until peers settle or reply
  - `agent_room_open` / `agent_post` / `agent_room_history` — shared log
  - `agent_bus_status` — one coordination snapshot
  """

  use Anubis.Server,
    name: "exhub-agent-bus",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.AgentBus.Register)
  component(Exhub.MCP.Tools.AgentBus.Identity)
  component(Exhub.MCP.Tools.AgentBus.List)
  component(Exhub.MCP.Tools.AgentBus.Send)
  component(Exhub.MCP.Tools.AgentBus.Inbox)
  component(Exhub.MCP.Tools.AgentBus.Await)
  component(Exhub.MCP.Tools.AgentBus.RoomOpen)
  component(Exhub.MCP.Tools.AgentBus.Post)
  component(Exhub.MCP.Tools.AgentBus.RoomHistory)
  component(Exhub.MCP.Tools.AgentBus.Status)

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
