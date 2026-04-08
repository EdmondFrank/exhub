defmodule Exhub.MCP.Tools.Agent.NewSession do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_new_session"

  @impl true
  def description, do: "Create a new ACP session on an initialized agent. Returns the session ID."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the initialized agent")
    field(:cwd, :string, description: "Working directory for the session")
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.get(params, :agent_id)
    cwd = Map.get(params, :cwd, File.cwd!())
    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        case ExMCP.ACP.Client.new_session(entry.client, cwd) do
          {:ok, result} ->
            session_id = Map.get(result, "sessionId")
            if session_id, do: Exhub.MCP.Agent.Store.add_session(agent_id, session_id)
            resp = Response.tool() |> Response.text(Jason.encode!(result))
            {:reply, resp, frame}
          {:error, reason} ->
            {:reply, Response.tool() |> Response.error("Failed to create session: #{inspect(reason)}"), frame}
        end
      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end
end
