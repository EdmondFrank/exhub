defmodule Exhub.Sagents.Tools.StartAgent do
  @moduledoc "MCP Tool: agent_hub_start — Start an agent."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_start"

  @impl true
  def description do
    available = Hub.list_agents() |> Enum.map(& &1.name) |> Enum.join(", ")
    "Start an agent by name.\nAvailable agents: #{available}"
  end

  schema do
    field(:name, {:required, :string}, description: "Agent name to start")
  end

  @impl true
  def execute(%{name: name}, frame) do
    case Hub.start_agent(name) do
      {:ok, _pid} ->
        resp = Response.tool() |> Response.text(Jason.encode!(%{status: "started", name: name}))
        {:reply, resp, frame}

      {:error, :not_found} ->
        resp = Response.tool() |> Response.error("Agent '#{name}' not found.")
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to start agent: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
