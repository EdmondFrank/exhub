defmodule Exhub.Sagents.Tools.Reset do
  @moduledoc "MCP Tool: agent_hub_reset — Reset agent conversation state."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_reset"

  @impl true
  def description do
    "Reset an agent's conversation state (messages, todos, metadata)."
  end

  schema do
    field(:name, {:required, :string}, description: "Agent name")
  end

  @impl true
  def execute(%{name: name}, frame) do
    Hub.reset(name)
    resp = Response.tool() |> Response.text(Jason.encode!(%{status: "reset", name: name}))
    {:reply, resp, frame}
  end
end
