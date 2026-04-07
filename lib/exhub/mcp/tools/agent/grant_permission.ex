defmodule Exhub.MCP.Tools.Agent.GrantPermission do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_grant_permission"

  @impl true
  def description, do: "Resolve a pending permission request (operator mode). Call after receiving a permission_request event."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID with pending permission")
    field(:option_id, {:required, :string}, description: "The optionId to select from the permission options")
  end

  @impl true
  def execute(%{agent_id: agent_id, session_id: session_id, option_id: option_id}, frame) do
    case Exhub.MCP.Agent.Store.resolve_permission(agent_id, session_id, option_id) do
      {:ok, _} ->
        resp = Response.tool() |> Response.text(Jason.encode!(%{status: "granted", option_id: option_id}))
        {:reply, resp, frame}
      {:error, reason} ->
        {:reply, Response.tool() |> Response.error("Failed to grant permission: #{inspect(reason)}"), frame}
    end
  end
end
