defmodule Exhub.MCP.Tools.AgentBus.Status do
  @moduledoc """
  MCP Tool: agent_bus_status

  One snapshot of the coordination surface: who is running, what they are
  doing, what is unread, and which rooms are active. The synthesis view for a
  coordinator agent.
  """

  alias Exhub.Agent.{Bus, Registry}
  alias Exhub.MCP.Tools.AgentBus.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_bus_status"

  @impl true
  def description do
    """
    Snapshot of the whole bus: participants (status, detail, running, unread),
    rooms with members, and storage stats.

    Use this instead of calling agent_list plus room queries when you need to
    decide what to do next as a coordinator.
    """
  end

  schema do
  end

  @impl true
  def execute(_params, frame) do
    agents =
      Registry.list()
      |> Enum.map(fn record ->
        %{
          id: record.id,
          status: record.status,
          detail: record.detail,
          running: Helpers.running?(record.id),
          unread: safe_unread(record.id)
        }
      end)

    rooms =
      Bus.rooms()
      |> Enum.map(fn room ->
        %{
          name: room.name,
          members: room.members,
          created_by: room.created_by,
          created_at: room.created_at
        }
      end)

    Helpers.ok(frame, %{
      protocol: "agent-bus/1",
      bus: safe_stats(),
      agents: agents,
      rooms: rooms
    })
  end

  defp safe_unread(id) do
    Bus.unread(id)
  rescue
    _ -> %{}
  catch
    :exit, _ -> %{}
  end

  defp safe_stats do
    Bus.stats()
  rescue
    _ -> %{}
  catch
    :exit, _ -> %{}
  end
end
