defmodule Exhub.MCP.Tools.Agent.Task do
  @moduledoc """
  MCP Tool: agent_task

  One-shot delegation: spawn the agent if needed, open a session, run the prompt
  to completion and return the digest. Collapses the usual
  `agent_initialize` → `agent_new_session` → `agent_prompt_start` → poll loop
  into a single tool call.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Agent.{Config, Runner}
  use Anubis.Server.Component, type: :tool

  def name, do: "agent_task"

  @impl true
  def description do
    available =
      case Config.load() do
        {:ok, config} when map_size(config) > 0 ->
          "\nAvailable agent_id values: #{config |> Map.keys() |> Enum.sort() |> Enum.join(", ")}"

        _ ->
          ""
      end

    """
    Delegate a task to an ACP agent in a single call and return a compact digest.
    #{available}

    Spawns the agent if it is not running, opens a session (optionally with the
    ExHub agent bus), sends the prompt, waits for completion, and returns the
    answer plus usage, tool trace, changed files and errors. Use
    `keep_session: true` to keep the session id for follow-up `agent_prompt`
    calls; by default the session is closed after the turn.

    Parameters:
    - agent_id, content: required
    - cwd: working directory for the session (default: the server's cwd)
    - expose_bus: inject the ExHub agent bus, addressed to this agent
    - mcp_servers: extra MCP servers, `[{name, url}]` or `[{name, command, args}]`
    - timeout_ms: max wait (default 300000, capped at 570000)
    - keep_session: keep the session open for follow-ups (default false)
    - max_answer_bytes: cap on the returned answer (default 4000)
    """
  end

  schema do
    field(:agent_id, {:required, :string}, description: "ID of the agent to run the task")
    field(:content, {:required, :string}, description: "Prompt text to send to the agent")
    field(:cwd, :string, description: "Working directory for the session")

    field(:expose_bus, :boolean,
      description: "Inject the ExHub agent bus, addressed to this agent_id",
      default: false
    )

    field(:mcp_servers, {:list, :map},
      description:
        "Additional MCP servers: [{name, url}] (http/sse) or [{name, command, args, env}] (stdio)"
    )

    field(:timeout_ms, :integer,
      description: "Max wait in ms (default 300000, capped at 570000)",
      default: 300_000
    )

    field(:keep_session, :boolean,
      description: "Keep the session open for follow-up agent_prompt calls",
      default: false
    )

    field(:max_answer_bytes, :integer,
      description: "Cap on the returned answer text (default 4000 bytes)",
      default: 4_000
    )
  end

  @impl true
  def execute(params, frame) do
    agent_id = Map.fetch!(params, :agent_id)
    content = Map.fetch!(params, :content)

    opts = [
      cwd: Map.get(params, :cwd),
      expose_bus: Map.get(params, :expose_bus, false),
      mcp_servers: Map.get(params, :mcp_servers, []),
      timeout_ms: Map.get(params, :timeout_ms, 300_000),
      keep_session: Map.get(params, :keep_session, false),
      max_answer_bytes: Map.get(params, :max_answer_bytes, 4_000)
    ]

    case Runner.run_task(agent_id, content, opts) do
      {:ok, digest} ->
        {:reply, Response.tool() |> Response.text(Jason.encode!(digest)), frame}

      {:error, reason} ->
        {:reply, Response.tool() |> Response.error(reason), frame}
    end
  end
end
