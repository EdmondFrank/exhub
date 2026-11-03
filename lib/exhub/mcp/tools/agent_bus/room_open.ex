defmodule Exhub.MCP.Tools.AgentBus.RoomOpen do
  @moduledoc "MCP Tool: agent_room_open — create or join a shared log."
  alias Exhub.Agent.Bus
  alias Exhub.MCP.Tools.AgentBus.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_room_open"

  @impl true
  def description do
    """
    Create a room (if needed) and join it.

    A room is a flat shared log for peers working on the same task: everyone in
    it reads every post through agent_inbox, and can replay it with
    agent_room_history. Rooms have no access control — do not put secrets in
    one.
    """
  end

  schema do
    field(:room, {:required, :string}, description: "Room name (lowercase, '.', '_', '-')")
    field(:as, :string, description: "Caller id; optional when the bus URL carries `?as=<id>`")
  end

  @impl true
  def execute(params, frame) do
    with {:ok, caller} <- Helpers.require_caller(params, frame) do
      case Bus.open_room(Map.get(params, :room), by: caller) do
        {:ok, room} -> Helpers.ok(frame, %{room: room, as: caller})
        {:error, reason} -> Helpers.error(frame, Helpers.error_message(reason))
      end
    else
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end
end
