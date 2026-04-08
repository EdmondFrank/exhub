defmodule Exhub.MCP.AgentServer do
  @moduledoc """
  MCP Server for ACP (Agent Communication Protocol) operations.

  Provides tools to spawn, manage, and interact with ACP agents like Claude Code,
  Gemini CLI, and other AI coding assistants.

  ## Tool Categories

  ### Lifecycle
  - `agent_initialize` — Spawn and initialize an ACP agent process
  - `agent_shutdown` — Gracefully shut down a running agent
  - `agent_list_running` — List all running agents
  - `agent_get_status` — Get detailed status of an agent
  - `agent_set_status` — Set status text for an agent

  ### Sessions
  - `agent_new_session` — Create a new session on an agent
  - `agent_load_session` — Resume a previous session
  - `agent_list_sessions` — List active sessions for an agent
  - `agent_close_session` — Close a session

  ### Prompts
  - `agent_prompt_start` — Send a prompt (non-blocking)
  - `agent_prompt_events` — Poll for events from a session
  - `agent_cancel` — Cancel an in-progress prompt

  ### Permissions (Operator Mode)
  - `agent_grant_permission` — Approve a pending permission request

  ### Modes
  - `agent_set_mode` — Set the operating mode for a session
  """

  use Anubis.Server,
    name: "exhub-agent-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Lifecycle tools
  component Exhub.MCP.Tools.Agent.Initialize
  component Exhub.MCP.Tools.Agent.Shutdown
  component Exhub.MCP.Tools.Agent.ListRunningAgents
  component Exhub.MCP.Tools.Agent.GetAgentStatus
  component Exhub.MCP.Tools.Agent.SetAgentStatus

  # Session tools
  component Exhub.MCP.Tools.Agent.NewSession
  component Exhub.MCP.Tools.Agent.LoadSession
  component Exhub.MCP.Tools.Agent.ListSessions
  component Exhub.MCP.Tools.Agent.CloseSession

  # Prompt tools
  component Exhub.MCP.Tools.Agent.PromptStart
  component Exhub.MCP.Tools.Agent.PromptEvents
  component Exhub.MCP.Tools.Agent.Cancel

  # Permission tools
  component Exhub.MCP.Tools.Agent.GrantPermission

  # Mode tools
  component Exhub.MCP.Tools.Agent.SetMode

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end
end
