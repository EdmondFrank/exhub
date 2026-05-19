defmodule Exhub.Sagents.Tools.Status do
  @moduledoc "MCP Tool: agent_hub_status — Get agent status."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_status"

  @impl true
  def description do
    "Get the current status of a specific agent."
  end

  schema do
    field(:name, {:required, :string}, description: "Agent name")
  end

  @impl true
  def execute(%{name: name}, frame) do
    status = Hub.get_status(name)
    resp = Response.tool() |> Response.text(Jason.encode!(status))
    {:reply, resp, frame}
  end
end
