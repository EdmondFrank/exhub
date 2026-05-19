defmodule Exhub.Sagents.Tools.Chat do
  @moduledoc "MCP Tool: agent_hub_chat — Send a message to an agent and get a response."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_chat"

  @impl true
  def description do
    "Send a chat message to a specific agent. The agent is auto-started if not running. Returns the agent's response."
  end

  schema do
    field(:agent, {:required, :string}, description: "Agent name")
    field(:message, {:required, :string}, description: "Message to send")
  end

  @impl true
  def execute(%{agent: agent, message: message}, frame) do
    case Hub.chat(agent, message) do
      {:ok, response} ->
        resp = Response.tool() |> Response.text(response)
        {:reply, resp, frame}

      {:error, :not_found} ->
        resp = Response.tool() |> Response.error("Agent '#{agent}' not found.")
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Chat failed: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
