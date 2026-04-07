defmodule Exhub.MCP.Tools.Agent.Cancel do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_cancel"

  @impl true
  def description, do: "Cancel an in-progress prompt in an agent session."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID to cancel")
  end

  @impl true
  def execute(%{agent_id: agent_id, session_id: session_id}, frame) do
    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        ExMCP.ACP.Client.cancel(entry.client, session_id)
        resp = Response.tool() |> Response.text(Jason.encode!(%{status: "cancelled", session_id: session_id}))
        {:reply, resp, frame}
      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end
end
