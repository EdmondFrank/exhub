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
    """
    Spawn an ACP agent process and perform the initialize handshake.

    The agent_id is a key from your agents config (~/.config/exhub/agents.json),
    or provide command directly to spawn a specific agent.

    Parameters:
    - agent_id: Unique identifier for this agent instance
    - command: Command to run (overrides config). E.g. 'gemini'
    - args: Command arguments
    """
  end

  schema do
    field(:agent_id, {:required, :string}, description: "Unique identifier for this agent instance")
    field(:command, :string, description: "Command to run (overrides config). E.g. 'gemini'")
    field(:args, {:list, :string}, description: "Command arguments", default: [])
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.get(params, :agent_id)
    command = Map.get(params, :command)
    args = Map.get(params, :args, [])

    # Check if already running
    case Store.get(agent_id) do
      {:ok, _entry} ->
        resp = Response.tool() |> Response.error("Agent '#{agent_id}' is already running. Call agent_shutdown first.")
        {:reply, resp, frame}

      {:error, :not_found} ->
        # Build command
        cmd_result =
          if command do
            {:ok, [command | args]}
          else
            Config.get_agent_command(agent_id)
          end

        case cmd_result do
          {:ok, cmd_list} ->
            case ExMCP.ACP.Client.start_link(
                   command: cmd_list,
                   handler: Exhub.MCP.Agent.Handler,
                   handler_opts: [agent_id: agent_id]
                 ) do
              {:ok, client_pid} ->
                Store.register(agent_id, client_pid, cmd_list)

                resp =
                  Response.tool()
                  |> Helpers.toon_response(%{
                    agent_id: agent_id,
                    status: "initialized",
                    command: cmd_list
                  })

                {:reply, resp, frame}

              {:error, reason} ->
                resp = Response.tool() |> Response.error("Failed to start agent: #{inspect(reason)}")
                {:reply, resp, frame}
            end

          {:error, :not_found} ->
            resp =
              Response.tool()
              |> Response.error("Agent '#{agent_id}' not found in config and no command provided.")

            {:reply, resp, frame}
        end
    end
  end
end
