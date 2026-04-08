defmodule Exhub.MCP.Tools.Agent.PromptEvents do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_prompt_events"

  @impl true
  def description do
    """
    Long-poll for events from an agent session.

    Collects events for up to `collect_ms` milliseconds before returning,
    reducing the number of poll calls needed. Returns early if no new event
    arrives within `idle_ms` milliseconds, or immediately when a terminal
    event (complete/error) is received.

    Parameters:
    - agent_id: ID of the agent
    - session_id: Session ID to poll
    - collect_ms: Max collection window in ms (default 30000)
    - idle_ms: Return early if idle for this many ms (default 15000)
    """
  end

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID to poll")
    field(:collect_ms, :integer, description: "Max collection window in ms (default 30000)", default: 30_000)
    field(:idle_ms, :integer, description: "Idle timeout in ms — return early if no events for this long (default 15000)", default: 15_000)
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.get(params, :agent_id)
    session_id = Map.get(params, :session_id)
    collect_ms = Map.get(params, :collect_ms, 30_000)
    idle_ms = Map.get(params, :idle_ms, 15_000)

    case Exhub.MCP.Agent.Store.collect_events(agent_id, session_id, collect_ms, idle_ms) do
      {:ok, events} ->
        resp = Response.tool() |> Response.text(Jason.encode!(%{events: events}))
        {:reply, resp, frame}
      {:error, reason} ->
        {:reply, Response.tool() |> Response.error(inspect(reason)), frame}
    end
  end
end
