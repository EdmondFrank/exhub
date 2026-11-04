defmodule Exhub.MCP.Tools.Agent.NewSession do
  @moduledoc """
  MCP Tool: agent_new_session

  Creates an ACP session and, optionally, hands the agent MCP servers it can
  call itself. `expose_bus: true` injects the agent-bus endpoint pre-addressed
  to this agent, so a spawned subagent can message its peers without being
  driven through the orchestrator.

  Transport shapes follow the ACP spec (see `Exhub.MCP.Agent.Runner`):
  `%{"type" => "http" | "sse", "url" => ..., "headers" => [...]}` or
  `%{"command" => ..., "args" => [...], "env" => [...]}` for stdio.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Agent.Runner
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_new_session"

  @impl true
  def description do
    """
    Create a new ACP session on an initialized agent. Returns the session ID.

    MCP servers:
    - `expose_bus: true` — give the agent the ExHub agent bus (identity,
      messaging, rooms), scoped to `/agent-bus/mcp?as=<agent_id>`. This is what
      lets subagents talk to each other instead of relaying through you.
    - `mcp_servers` — extra servers, `[{name, url}]` (http/sse) or
      `[{name, command, args, env}]` (stdio), passed through to the ACP runtime.

    For the common "run this task and tell me the result" case use `agent_task`
    (spawn + session + prompt + digest in one call) or `agent_prompt` on an
    existing session.
    """
  end

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the initialized agent")
    field(:cwd, :string, description: "Working directory for the session")

    field(:expose_bus, :boolean,
      description: "Inject the ExHub agent bus, addressed to this agent_id",
      default: false
    )

    field(:mcp_servers, {:list, :map},
      description:
        "Additional MCP servers: [{name, url}] (http/sse) or [{name, command, args, env}] (stdio)"
    )
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.get(params, :agent_id)
    cwd = Map.get(params, :cwd, File.cwd!())

    opts = [
      cwd: cwd,
      expose_bus: Map.get(params, :expose_bus, false),
      mcp_servers: Map.get(params, :mcp_servers, [])
    ]

    case Runner.new_session(agent_id, opts) do
      {:ok, session_id, mcp_servers} ->
        slim = %{
          "sessionId" => session_id,
          "mcp_servers" => mcp_servers,
          "mcp_server_count" => length(mcp_servers)
        }

        resp = Response.tool() |> Response.text(Jason.encode!(slim))
        {:reply, resp, frame}

      {:error, reason} ->
        {:reply, Response.tool() |> Response.error(reason), frame}
    end
  end
end
