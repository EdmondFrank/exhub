defmodule Exhub.MCP.Tools.Agent.ListRunningAgents do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_list_running"

  @impl true
  def description, do: "List all running ACP agents with their status and active sessions."

  schema do
  end

  @impl true
  def execute(_params, frame) do
    agents =
      Exhub.MCP.Agent.Store.list()
      |> Enum.map(fn entry ->
        %{
          agent_id: entry.agent_id,
          command: entry.command,
          started_at: DateTime.to_iso8601(entry.started_at),
          status: entry.status_text,
          sessions: MapSet.to_list(entry.sessions)
        }
      end)
    resp = Response.tool() |> Response.text(Jason.encode!(%{agents: agents}))
    {:reply, resp, frame}
  end
end
