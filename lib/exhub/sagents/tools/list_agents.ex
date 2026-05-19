defmodule Exhub.Sagents.Tools.ListAgents do
  @moduledoc "MCP Tool: agent_hub_list — List all registered agents."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_list"

  @impl true
  def description do
    "List all registered agents in the Agent Hub with their running status."
  end

  schema do
  end

  @impl true
  def execute(_params, frame) do
    agents = Hub.list_agents()
    resp = Response.tool() |> Response.text(Jason.encode!(agents))
    {:reply, resp, frame}
  end
end
