defmodule Exhub.MCP.Tools.Agent.PromptEvents do
  alias Anubis.Server.Response
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_prompt_events"

  @impl true
  def description, do: "Non-blocking poll for events from an agent session."

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:session_id, {:required, :string}, description: "Session ID to poll")
  end

  @impl true
  def execute(%{agent_id: agent_id, session_id: session_id}, frame) do
    case Exhub.MCP.Agent.Store.pop_events(agent_id, session_id) do
      {:ok, events} ->
        resp = Response.tool() |> Response.text(Jason.encode!(%{events: events}))
        {:reply, resp, frame}
      {:error, reason} ->
        {:reply, Response.tool() |> Response.error(inspect(reason)), frame}
    end
  end
end
