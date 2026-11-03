defmodule Exhub.MCP.Tools.AgentBus.List do
  @moduledoc """
  MCP Tool: agent_list

  Participants known to the bus, with status, live process flag and unread
  counts. This is the routing surface: pick a collaborator by `capabilities`
  and `status`, not by guessing ids.
  """

  alias Exhub.Agent.{Bus, Registry}
  alias Exhub.MCP.Tools.AgentBus.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_list"

  @impl true
  def description do
    """
    List bus participants with status, capabilities, whether an ACP process is
    currently running, and how many unread messages each has.

    Filter with `status` when you only want available peers (e.g. "idle").
    """
  end

  schema do
    field(:status, {:enum, ~w(starting idle working blocked waiting_input done error offline)},
      description: "Only return participants in this status"
    )

    field(:include_unread, :boolean,
      description: "Include unread message counts (default true)",
      default: true
    )
  end

  @impl true
  def execute(params, frame) do
    status = Map.get(params, :status)
    include_unread = Map.get(params, :include_unread, true)

    agents =
      Registry.list()
      |> Enum.filter(fn record -> is_nil(status) or record.status == status end)
      |> Enum.map(fn record ->
        base = %{
          id: record.id,
          name: record.name,
          description: record.description,
          capabilities: record.capabilities,
          kind: record.kind,
          status: record.status,
          detail: record.detail,
          running: Helpers.running?(record.id),
          last_seen_at: record.last_seen_at
        }

        if include_unread do
          Map.put(base, :unread, safe_unread(record.id))
        else
          base
        end
      end)

    Helpers.ok(frame, %{count: length(agents), agents: agents})
  end

  defp safe_unread(id) do
    Bus.unread(id)
  rescue
    _ -> %{}
  catch
    :exit, _ -> %{}
  end
end
