defmodule Exhub.MCP.Tools.Agent.Shutdown do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_shutdown"

  @impl true
  def description, do: "Gracefully shut down a running ACP agent, closing all sessions."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent to shut down")
  end

  @impl true
  def execute(%{agent_id: agent_id}, frame) do
    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        try do
          ExMCP.ACP.Client.disconnect(entry.client)
        catch
          :exit, _ -> :ok
          _, _ -> :ok
        end
        Exhub.MCP.Agent.Store.unregister(agent_id)
        resp = Response.tool() |> Response.text(Jason.encode!(%{status: "shutdown", agent_id: agent_id}))
        {:reply, resp, frame}
      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end
end
