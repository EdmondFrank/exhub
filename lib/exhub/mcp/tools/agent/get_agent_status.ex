defmodule Exhub.MCP.Tools.Agent.GetAgentStatus do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_get_status"

  @impl true
  def description, do: "Get detailed status of a running ACP agent."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
  end

  @impl true
  def execute(%{agent_id: agent_id}, frame) do
    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        connection_status = ExMCP.ACP.Client.status(entry.client)
        result = %{
          agent_id: entry.agent_id,
          command: entry.command,
          started_at: DateTime.to_iso8601(entry.started_at),
          status: entry.status_text,
          connection_status: connection_status,
          sessions: MapSet.to_list(entry.sessions)
        }
        resp = Response.tool() |> Response.text(Jason.encode!(result))
        {:reply, resp, frame}
      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end
end
