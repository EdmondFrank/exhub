defmodule Exhub.MCP.Tools.Toonflow.Chat do
  @moduledoc """
  MCP Tool: `toonflow_chat` — talk to the Toonflow director agent.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Sagents.Hub

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_chat"

  @impl true
  def description do
    """
    Send a message to a Toonflow agent (default `"toonflow"`) and get its reply.
    The agent is started lazily on first use; it has the full Toonflow MCP tool
    set, so it can drive projects through the pipeline itself. Use
    `toonflow_list_agents` to discover agents.
    """
  end

  schema do
    field(:message, {:required, :string}, description: "The message to send.")
    field(:agent, :string, description: "Agent name (default \"toonflow\").")
  end

  @impl true
  def execute(params, frame) do
    message = Helpers.opt(params, :message) || Map.get(params, :message)
    agent = Helpers.opt(params, :agent) || "toonflow"

    case Hub.chat(agent, message) do
      {:ok, reply} ->
        resp =
          Response.tool()
          |> Response.structured(%{"agent" => agent, "reply" => reply, "success" => true})

        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, {:chat_failed, reason})
    end
  end
end
