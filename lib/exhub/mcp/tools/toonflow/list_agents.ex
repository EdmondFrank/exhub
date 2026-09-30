defmodule Exhub.MCP.Tools.Toonflow.ListAgents do
  @moduledoc """
  MCP Tool: `toonflow_list_agents` — list available sagents agents.
  """

  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_list_agents"

  @impl true
  def description do
    """
    List the registered agent profiles and whether each is currently running.
    The `"toonflow"` director agent (with the Toonflow tool set) is started
    lazily when first chatted with via `toonflow_chat`.
    """
  end

  schema do
  end

  @impl true
  def execute(_params, frame) do
    agents = Hub.list_agents()

    resp =
      Response.tool()
      |> Response.structured(%{"agents" => agents, "count" => length(agents), "success" => true})

    {:reply, resp, frame}
  end
end
