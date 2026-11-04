defmodule Exhub.MCP.Tools.Agent.PromptStart do
  alias Anubis.Server.Response
  alias Exhub.MCP.Agent.{Status, Store}
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_prompt_start"

  @impl true
  def description,
    do: """
    Send a prompt to an ACP agent session (non-blocking). Use agent_prompt_events to poll for results.

    Prefer `agent_prompt`, which waits for the turn to finish and returns a
    digest in one call — polling costs a model turn per poll. Use this tool
    only for fire-and-forget work. While the prompt runs the agent is marked
    `working` in the participant registry, so `agent_await` can be used as a
    fan-in wait.
    """

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID")
    field(:content, {:required, :string}, description: "Prompt text to send to the agent")
  end

  @impl true
  def execute(%{agent_id: agent_id, session_id: session_id, content: content}, frame) do
    case Store.get(agent_id) do
      {:ok, entry} ->
        client = entry.client
        Store.mark_running(agent_id, session_id)
        Store.set_status(agent_id, "working")
        Status.mirror(agent_id, "working")

        Task.start(fn ->
          try do
            case ExMCP.ACP.Client.prompt(client, session_id, content, timeout: :infinity) do
              {:ok, result} ->
                event = %{
                  type: "complete",
                  session_id: session_id,
                  agent_id: agent_id,
                  stop_reason: Map.get(result, "stopReason")
                }

                Store.push_event(agent_id, session_id, event)
                finish(agent_id, "idle")

              {:error, reason} ->
                event = %{
                  type: "error",
                  session_id: session_id,
                  agent_id: agent_id,
                  message: inspect(reason)
                }

                Store.push_event(agent_id, session_id, event)
                finish(agent_id, "error")
            end
          catch
            :exit, reason ->
              event = %{
                type: "error",
                session_id: session_id,
                agent_id: agent_id,
                message: "Agent exited: #{inspect(reason)}"
              }

              Store.push_event(agent_id, session_id, event)
              finish(agent_id, "error")

            kind, reason ->
              event = %{
                type: "error",
                session_id: session_id,
                agent_id: agent_id,
                message: "Unexpected error (#{kind}): #{inspect(reason)}"
              }

              Store.push_event(agent_id, session_id, event)
              finish(agent_id, "error")
          end
        end)

        resp =
          Response.tool()
          |> Response.text(Jason.encode!(%{status: "prompted", session_id: session_id}))

        {:reply, resp, frame}

      {:error, :not_found} ->
        {:reply, Response.tool() |> Response.error("Agent '#{agent_id}' is not running."), frame}
    end
  end

  defp finish(agent_id, status) do
    Store.set_status(agent_id, status)
    Status.mirror(agent_id, status)
  end
end
