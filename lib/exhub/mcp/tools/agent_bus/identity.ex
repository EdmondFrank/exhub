defmodule Exhub.MCP.Tools.AgentBus.Identity do
  @moduledoc """
  MCP Tool: agent_identity

  Returns who the caller is on the bus, auto-registering a minimal record on
  first contact so an agent that was handed a scoped bus URL does not have to
  discover the registration step before it can be addressed.
  """

  alias Exhub.Agent.{Bus, Registry}
  alias Exhub.MCP.Tools.AgentBus.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_identity"

  @impl true
  def description do
    """
    Return the caller's participant record, unread counts and joined rooms.

    Auto-registers a minimal record on first call. Use this at the start of a
    task to learn your own id, home directory and MEMORY.md path.
    """
  end

  schema do
    field(:as, :string,
      description: "Caller id; optional when connected to /agent-bus/mcp?as=<id>"
    )
  end

  @impl true
  def execute(params, frame) do
    with {:ok, id} <- Helpers.require_caller(params, frame) do
      {record, auto_registered} =
        case Registry.get(id) do
          {:ok, record} ->
            {record, false}

          {:error, :not_found} ->
            {:ok, record} = Registry.register(%{"id" => id, "name" => id, "kind" => "acp"})
            {record, true}
        end

      unread = safe_unread(id)
      rooms = joined_rooms(id)

      Helpers.ok(frame, %{
        agent: record,
        auto_registered: auto_registered,
        memory_path: Registry.memory_path(id),
        unread: unread,
        rooms: rooms
      })
    else
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end

  defp safe_unread(id) do
    Bus.unread(id)
  rescue
    _ -> %{}
  catch
    :exit, _ -> %{}
  end

  defp joined_rooms(id) do
    Bus.rooms()
    |> Enum.filter(&(id in &1.members))
    |> Enum.map(& &1.name)
  rescue
    _ -> []
  catch
    :exit, _ -> []
  end
end
