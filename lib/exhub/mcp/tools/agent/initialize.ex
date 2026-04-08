defmodule Exhub.MCP.Tools.Agent.Initialize do
  @moduledoc """
  MCP Tool: agent_initialize

  Spawn an ACP agent process and perform the initialize handshake.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Agent.Store
  alias Exhub.MCP.Agent.Config

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

    Parameters:
    - agent_id: Unique identifier for this agent instance
    """
  end

  schema do
    field(:agent_id, {:required, :string}, description: "Unique identifier for this agent instance")
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.get(params, :agent_id)

    # Check if already running (with stale-entry guard)
    case Store.get(agent_id) do
      {:ok, entry} ->
        if Process.alive?(entry.client) do
          resp = Response.tool() |> Response.error("Agent '#{agent_id}' is already running. Call agent_shutdown first.")
          {:reply, resp, frame}
        else
          # Stale entry — clean up and fall through to fresh start
          Store.unregister(agent_id)
          do_start_agent(agent_id, frame)
        end

      {:error, :not_found} ->
        do_start_agent(agent_id, frame)
    end
  end

  defp do_start_agent(agent_id, frame) do
    case Config.get_agent_command(agent_id) do
      {:ok, cmd_list} ->
        result =
          try do
            ExMCP.ACP.Client.start_link(
              command: cmd_list,
              handler: Exhub.MCP.Agent.Handler,
              handler_opts: [agent_id: agent_id]
            )
          catch
            :exit, reason -> {:error, {:exit, reason}}
          end

        case result do
          {:ok, client_pid} ->
            if Process.alive?(client_pid) do
              Store.register(agent_id, client_pid, cmd_list)

              resp =
                Response.tool()
                |> Helpers.toon_response(%{
                  agent_id: agent_id,
                  status: "initialized",
                  command: cmd_list
                })

              {:reply, resp, frame}
            else
              Store.unregister(agent_id)
              resp = Response.tool() |> Response.error("Agent '#{agent_id}' started but died immediately")
              {:reply, resp, frame}
            end

          {:error, {:exit, {:timeout, _}}} ->
            Store.unregister(agent_id)
            resp = Response.tool() |> Response.error("Agent '#{agent_id}' timed out during initialization. The agent process may be slow to start.")
            {:reply, resp, frame}

          {:error, {:exit, reason}} ->
            Store.unregister(agent_id)
            resp = Response.tool() |> Response.error("Agent '#{agent_id}' crashed during initialization: #{inspect(reason)}")
            {:reply, resp, frame}

          {:error, reason} ->
            Store.unregister(agent_id)
            resp = Response.tool() |> Response.error("Failed to start agent: #{inspect(reason)}")
            {:reply, resp, frame}
        end

      {:error, :not_found} ->
        resp =
          Response.tool()
          |> Response.error("Agent '#{agent_id}' not found in config.")

        {:reply, resp, frame}
    end
  end
end
