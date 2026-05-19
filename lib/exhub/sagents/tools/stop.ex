defmodule Exhub.Sagents.Tools.Stop do
  @moduledoc "MCP Tool: agent_hub_stop — Stop a running agent."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_stop"

  @impl true
  def description do
    "Stop a running agent and free its resources."
  end

  schema do
    field(:name, {:required, :string}, description: "Agent name")
  end

  @impl true
  def execute(%{name: name}, frame) do
    Hub.stop_agent(name)
    resp = Response.tool() |> Response.text(Jason.encode!(%{status: "stopped", name: name}))
    {:reply, resp, frame}
  end
end
