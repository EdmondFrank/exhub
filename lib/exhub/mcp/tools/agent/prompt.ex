defmodule Exhub.MCP.Tools.Agent.Prompt do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_prompt"

  @impl true
  def description, do: "Send a prompt to an ACP agent session and block until complete or timeout. Returns all accumulated events."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID")
    field(:content, {:required, :string}, description: "Prompt text to send to the agent")
    field(:timeout_ms, :integer, description: "Max wait time in ms (default 120000)", default: 120_000)
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.get(params, :agent_id)
    session_id = Map.get(params, :session_id)
    content = Map.get(params, :content)
    timeout_ms = Map.get(params, :timeout_ms, 120_000)

    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        client = entry.client
        Task.start(fn ->
          case ExMCP.ACP.Client.prompt(client, session_id, content) do
            {:ok, result} ->
              event = %{type: "complete", session_id: session_id, agent_id: agent_id,
                        stop_reason: Map.get(result, "stopReason")}
              Exhub.MCP.Agent.Store.push_event(agent_id, session_id, event)
            {:error, reason} ->
              event = %{type: "error", session_id: session_id, agent_id: agent_id,
                        message: inspect(reason)}
              Exhub.MCP.Agent.Store.push_event(agent_id, session_id, event)
          end
        end)
        case Exhub.MCP.Agent.Store.wait_for_events(agent_id, session_id, timeout_ms) do
          {:ok, events} ->
            resp = Response.tool() |> Response.text(Jason.encode!(%{events: events}))
            {:reply, resp, frame}
          {:error, reason} ->
            {:reply, Response.tool() |> Response.error("Wait failed: #{inspect(reason)}"), frame}
        end
      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end
end
