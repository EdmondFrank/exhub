defmodule Exhub.MCP.Tools.Agent.PromptStart do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_prompt_start"

  @impl true
  def description, do: "Send a prompt to an ACP agent session (non-blocking). Use agent_prompt_events to poll for results."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID")
    field(:content, {:required, :string}, description: "Prompt text to send to the agent")
  end

  @impl true
  def execute(%{agent_id: agent_id, session_id: session_id, content: content}, frame) do
    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        client = entry.client
        Task.start(fn ->
          try do
            case ExMCP.ACP.Client.prompt(client, session_id, content, timeout: :infinity) do
              {:ok, result} ->
                event = %{type: "complete", session_id: session_id, agent_id: agent_id,
                          stop_reason: Map.get(result, "stopReason")}
                Exhub.MCP.Agent.Store.push_event(agent_id, session_id, event)
              {:error, reason} ->
                event = %{type: "error", session_id: session_id, agent_id: agent_id,
                          message: inspect(reason)}
                Exhub.MCP.Agent.Store.push_event(agent_id, session_id, event)
            end
          catch
            :exit, reason ->
              event = %{type: "error", session_id: session_id, agent_id: agent_id,
                        message: "Agent exited: #{inspect(reason)}"}
              Exhub.MCP.Agent.Store.push_event(agent_id, session_id, event)
            kind, reason ->
              event = %{type: "error", session_id: session_id, agent_id: agent_id,
                        message: "Unexpected error (#{kind}): #{inspect(reason)}"}
              Exhub.MCP.Agent.Store.push_event(agent_id, session_id, event)
          end
        end)
        resp = Response.tool() |> Response.text(Jason.encode!(%{status: "prompted", session_id: session_id}))
        {:reply, resp, frame}
      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end
end
