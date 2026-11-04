defmodule Exhub.MCP.Tools.Agent.Initialize do
  @moduledoc """
  MCP Tool: agent_initialize

  Spawn an ACP agent process and perform the initialize handshake.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Agent.{Config, Runner}

  use Anubis.Server.Component, type: :tool

  def name, do: "agent_initialize"

  @impl true
  def description do
    available_agents =
      case Config.load() do
        {:ok, config} when map_size(config) > 0 ->
          agent_ids = config |> Map.keys() |> Enum.sort() |> Enum.join(", ")
          "\nAvailable agent_id values: #{agent_ids}"

        _ ->
          ""
      end

    """
    Spawn an ACP agent process and perform the initialize handshake.
    #{available_agents}

    `agent_task` spawns the agent itself, so this is only needed when you want
    to manage sessions manually (initialize → new_session → prompt).
    """
  end

  schema do
    field(:agent_id, {:required, :string},
      description: "Unique identifier for this agent instance"
    )
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.get(params, :agent_id)

    # Check if already running (with stale-entry guard handled by Runner).
    case Exhub.MCP.Agent.Store.get(agent_id) do
      {:ok, entry} ->
        if Process.alive?(entry.client) do
          resp =
            Response.tool()
            |> Response.error(
              "Agent '#{agent_id}' is already running. Call agent_shutdown first."
            )

          {:reply, resp, frame}
        else
          start(agent_id, frame)
        end

      {:error, :not_found} ->
        start(agent_id, frame)
    end
  end

  defp start(agent_id, frame) do
    case Runner.ensure_agent(agent_id) do
      {:ok, entry} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            agent_id: agent_id,
            status: "initialized",
            command: entry.command
          })

        {:reply, resp, frame}

      {:error, reason} ->
        {:reply, Response.tool() |> Response.error(reason), frame}
    end
  end
end
