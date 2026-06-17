defmodule Exhub.Sagents.AgentHubServer do
  @moduledoc """
  MCP Server for the Agent Hub platform.

  Provides tools to list, start, chat with, and manage sagents agents.
  Accessible at `/agent-hub/mcp`.
  """

  use Anubis.Server,
    name: "exhub-agent-hub-server",
    version: "1.0.0",
    capabilities: [:tools]

  # component Exhub.Sagents.Tools.ListAgents
  # component Exhub.Sagents.Tools.StartAgent
  # component Exhub.Sagents.Tools.Chat
  # component Exhub.Sagents.Tools.Status
  # component Exhub.Sagents.Tools.Reset
  # component Exhub.Sagents.Tools.Stop

  # @impl true
  # def init(client_info, frame) do
  #   _ = client_info
  #   {:ok, frame}
  # end

  # @impl true
  # def handle_request(request, frame) do
  #   Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  # end
end
