defmodule Exhub.MCP.Tools.Agent.SetAgentStatus do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_set_status"

  @impl true
  def description, do: "Set the status text for a running agent."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:status_text, {:required, :string}, description: "Status text to set")
  end

  @impl true
  def execute(%{agent_id: agent_id, status_text: status_text}, frame) do
    Exhub.MCP.Agent.Store.set_status(agent_id, status_text)
    resp = Response.tool() |> Response.text(Jason.encode!(%{status: "ok", agent_id: agent_id, status_text: status_text}))
    {:reply, resp, frame}
  end
end
