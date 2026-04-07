defmodule Exhub.MCP.Tools.Agent.ListSessions do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_list_sessions"

  @impl true
  def description, do: "List sessions for an agent (active sessions tracked locally)."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
  end

  @impl true
  def execute(%{agent_id: agent_id}, frame) do
    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        sessions = MapSet.to_list(entry.sessions)
        resp = Response.tool() |> Response.text(Jason.encode!(%{agent_id: agent_id, sessions: sessions}))
        {:reply, resp, frame}
      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end
end
