defmodule Exhub.MCP.Tools.Agent.SetMode do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_set_mode"

  @impl true
  def description, do: "Set the operating mode for an agent session."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID")
    field(:mode_id, {:required, :string}, description: "Mode ID to set")
  end

  @impl true
  def execute(%{agent_id: agent_id, session_id: session_id, mode_id: mode_id}, frame) do
    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        case ExMCP.ACP.Client.set_mode(entry.client, session_id, mode_id) do
          {:ok, result} ->
            resp = Response.tool() |> Response.text(Jason.encode!(result))
            {:reply, resp, frame}
          {:error, reason} ->
            {:reply, Response.tool() |> Response.error("Failed to set mode: #{inspect(reason)}"), frame}
        end
      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end
end
