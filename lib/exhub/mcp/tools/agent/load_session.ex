defmodule Exhub.MCP.Tools.Agent.LoadSession do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_load_session"

  @impl true
  def description, do: "Resume a previously created ACP session."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the initialized agent")
    field(:session_id, {:required, :string}, description: "Session ID to resume")
    field(:cwd, :string, description: "Working directory")
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.get(params, :agent_id)
    session_id = Map.get(params, :session_id)
    cwd = Map.get(params, :cwd, File.cwd!())
    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        case ExMCP.ACP.Client.load_session(entry.client, session_id, cwd) do
          {:ok, result} ->
            Exhub.MCP.Agent.Store.add_session(agent_id, session_id)
            resp = Response.tool() |> Response.text(Jason.encode!(result))
            {:reply, resp, frame}
          {:error, reason} ->
            {:reply, Response.tool() |> Response.error("Failed to load session: #{inspect(reason)}"), frame}
        end
      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end
end
