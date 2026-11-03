defmodule Exhub.MCP.Tools.AgentBus.RoomHistory do
  @moduledoc "MCP Tool: agent_room_history — replay a room's log by sequence."
  alias Exhub.Agent.Bus
  alias Exhub.MCP.Tools.AgentBus.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_room_history"

  @impl true
  def description do
    """
    Read a room's messages after `since_seq` (default 0 — from the start of the
    retained log).

    Use this when you join a room late, or to re-read a decision without asking
    a peer to repeat it. Each message has a per-stream `seq`; pass the last one
    you read as `since_seq` next time.
    """
  end

  schema do
    field(:room, {:required, :string}, description: "Room name")

    field(:since_seq, :integer,
      description: "Return messages with seq greater than this",
      default: 0
    )

    field(:limit, :integer, description: "Max messages (default 50, max 200)", default: 50)
  end

  @impl true
  def execute(params, frame) do
    case Bus.history("room:" <> Map.get(params, :room),
           since_seq: Map.get(params, :since_seq, 0),
           limit: Map.get(params, :limit, 50)
         ) do
      {:ok, payload} -> Helpers.ok(frame, payload)
      {:error, reason} -> Helpers.error(frame, Helpers.error_message(reason))
    end
  end
end
