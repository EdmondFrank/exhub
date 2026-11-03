defmodule Exhub.MCP.Tools.Agent.SetAgentStatus do
  @moduledoc """
  MCP Tool: agent_set_status

  Sets an agent's status in the ACP store **and** mirrors it into the agent
  registry, so `agent_list` / `agent_await` see the same truth the ACP tools do.
  """

  alias Anubis.Server.Response
  alias Exhub.Agent.Registry
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_set_status"

  @impl true
  def description do
    """
    Set the status text for an agent.

    When the text is one of the bus statuses (starting, idle, working, blocked,
    waiting_input, done, error, offline) it becomes the agent's bus status and
    the record is kept in sync; otherwise it is stored as the status detail.
    """
  end

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent")
    field(:status_text, {:required, :string}, description: "Status text to set")
    field(:detail, :string, description: "Optional explanation for the status")
  end

  @impl true
  def execute(%{agent_id: agent_id, status_text: status_text} = params, frame) do
    Exhub.MCP.Agent.Store.set_status(agent_id, status_text)
    mirror_to_registry(agent_id, status_text, Map.get(params, :detail))

    resp =
      Response.tool()
      |> Response.text(
        Jason.encode!(%{status: "ok", agent_id: agent_id, status_text: status_text})
      )

    {:reply, resp, frame}
  end

  defp mirror_to_registry(agent_id, status_text, detail) do
    attrs =
      if Registry.valid_status?(status_text) do
        # A new status without a detail must not inherit the previous detail.
        %{"id" => agent_id, "status" => status_text, "detail" => detail || ""}
      else
        %{"id" => agent_id, "detail" => status_text}
      end

    Registry.register(attrs)
  rescue
    _ -> :ok
  catch
    :exit, _ -> :ok
  end
end
