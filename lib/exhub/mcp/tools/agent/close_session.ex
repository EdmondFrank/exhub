defmodule Exhub.MCP.Tools.Agent.CloseSession do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_close_session"

  @impl true
  def description, do: "Close an active session (preserved for later resume)."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID to close")
  end

  @impl true
  def execute(%{agent_id: agent_id, session_id: session_id}, frame) do
    Exhub.MCP.Agent.Store.remove_session(agent_id, session_id)
    resp = Response.tool() |> Response.text(Jason.encode!(%{status: "closed", session_id: session_id}))
    {:reply, resp, frame}
  end
end
