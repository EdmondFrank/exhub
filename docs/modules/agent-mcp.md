# ACP Agent MCP Server

## Overview

The ACP Agent MCP Server provides a bridge between MCP clients and ACP (Agent Communication Protocol) agents like Claude Code, Gemini CLI, OpenCode, and Codex. It enables spawning, managing, and interacting with AI coding agents via MCP tools.

**HTTP Endpoint:** `POST /agent/mcp`

This server acts as a gateway that translates MCP tool calls into ACP protocol messages, allowing any MCP-compatible client to orchestrate AI coding agents for complex development workflows.

---

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           MCP Client (Claude Code, Zed, etc.)               │
└─────────────────────────────────┬───────────────────────────────────────────┘
                                  │ HTTP POST /agent/mcp
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        Exhub.MCP.AgentServer (Anubis MCP)                   │
│  ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────────────────────┐│
│  │ Lifecycle Tools │ │  Session Tools  │ │      Prompt/Permission Tools    ││
│  │ ─────────────── │ │ ─────────────── │ │ ─────────────────────────────── ││
│  │ agent_initialize│ │ agent_new_session│ │ agent_prompt                   ││
│  │ agent_shutdown  │ │ agent_load_session││ agent_prompt_start             ││
│  │ agent_list_...  │ │ agent_list_sessions││ agent_prompt_events           ││
│  │ agent_get_status│ │ agent_close_session││ agent_cancel                  ││
│  │ agent_set_status│ │                 │ │ agent_grant_permission          ││
│  └─────────────────┘ └─────────────────┘ │ agent_set_mode                  ││
│                                          └─────────────────────────────────┘│
└─────────────────────────────────┬───────────────────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Exhub.MCP.Agent.Store (GenServer)                   │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │  State: %{agents: %{agent_id => %{                                      ││
│  │    client: pid,          # ExMCP.ACP.Client process                     ││
│  │    command: [...],       # Spawn command                                ││
│  │    started_at: DateTime, # Initialization time                          ││
│  │    status_text: "...",   # Human-readable status                        ││
│  │    sessions: MapSet,     # Active session IDs                           ││
│  │    event_queues: %{},    # Per-session event buffers                    ││
│  │    waiters: %{},         # Pending event waiters                        ││
│  │    pending_permissions: {} # Permission request state                   ││
│  │  }}}                                                                    ││
│  └─────────────────────────────────────────────────────────────────────────┘│
└─────────────────────────────────┬───────────────────────────────────────────┘
                                  │
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         ExMCP.ACP.Client (ExMCP Library)                    │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │  ACP Protocol Handler: Exhub.MCP.Agent.Handler                          ││
│  │  - handle_session_update/3    → Push events to Store                    ││
│  │  - handle_permission_request/4 → Block and wait for grant_permission    ││
│  │  - handle_file_read/4         → Local file access                       ││
│  │  - handle_file_write/4        → Local file write                        ││
│  └─────────────────────────────────────────────────────────────────────────┘│
└─────────────────────────────────┬───────────────────────────────────────────┘
                                  │ Stdio/ACP Protocol
                                  ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                      ACP Agent Process (Gemini, Claude, etc.)               │
│                         ┌─────────────┐    ┌─────────────┐                  │
│                         │   Session   │◄──►│   Session   │                  │
│                         │     A       │    │     B       │                  │
│                         └─────────────┘    └─────────────┘                  │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Prerequisites

- Exhub server running (`mix run --no-halt` or release)
- At least one ACP-compatible coding agent installed:
  ```bash
  npm install -g @google/gemini-cli   # Gemini CLI
  npm install -g opencode             # OpenCode
  pip install claude-code             # Claude Code (ACP mode)
  npm install -g @anthropic-ai/codex  # Codex CLI
  ```

---

## Configuration

Create `~/.config/exhub/agents.json` to define available agents:

```json
{
  "gemini": {
    "command": "gemini",
    "args": ["--acp"]
  },
  "opencode": {
    "command": "opencode",
    "args": []
  },
  "claude": {
    "command": "claude",
    "args": []
  },
  "codex": {
    "command": "codex",
    "args": ["--acp"]
  }
}
```

**Configuration Options:**

| Field     | Type            | Required | Description                                 |
|-----------|-----------------|----------|---------------------------------------------|
| `command` | string          | yes      | Executable name or path to spawn the agent  |
| `args`    | list of strings | no       | Arguments to pass to the command            |
| `env`     | map             | no       | Environment variables (not yet implemented) |

**Alternative:** You can skip the config file and pass `command` directly to `agent_initialize`.

**Environment Variable:** Set `EXHUB_AGENTS_CONFIG` to use a custom config path.

---

## MCP Tools Reference

### Lifecycle Tools (5)

#### `agent_initialize`

Spawn an ACP agent process and perform the initialize handshake.

**Parameters:**

| Parameter  | Type            | Required | Description                                        |
|------------|-----------------|----------|----------------------------------------------------|
| `agent_id` | string          | yes      | Unique identifier for this agent instance          |
| `command`  | string          | no       | Command to run (overrides config). E.g. `'gemini'` |
| `args`     | list of strings | no       | Command arguments (default: `[]`)                  |

**Response (success):**

```json
{
  "agent_id": "gemini",
  "status": "initialized",
  "command": ["gemini", "--acp"]
}
```

**Error Cases:**
- `"Agent 'X' is already running. Call agent_shutdown first."` — Agent with this ID already exists
- `"Agent 'X' not found in config and no command provided."` — No config entry and no command override
- `"Failed to start agent: ..."` — Process spawn failure

---

#### `agent_shutdown`

Gracefully shut down a running agent and clean up all resources.

**Parameters:**

| Parameter  | Type   | Required | Description                  |
|------------|--------|----------|------------------------------|
| `agent_id` | string | yes      | ID of the agent to shut down |

**Response (success):**

```json
{
  "agent_id": "gemini",
  "status": "shutdown"
}
```

---

#### `agent_list_running`

List all currently running agents with their status and session counts.

**Parameters:** None

**Response:**

```json
{
  "agents": [
    {
      "agent_id": "gemini",
      "status": "initialized",
      "sessions": ["sess_abc123", "sess_def456"],
      "started_at": "2026-04-08T10:30:00Z"
    }
  ]
}
```

---

#### `agent_get_status`

Get detailed status of a specific agent including connection state and sessions.

**Parameters:**

| Parameter  | Type   | Required | Description     |
|------------|--------|----------|-----------------|
| `agent_id` | string | yes      | ID of the agent |

**Response:**

```json
{
  "agent_id": "gemini",
  "connection_status": "connected",
  "status_text": "working on auth refactor",
  "sessions": ["sess_abc123"],
  "command": ["gemini", "--acp"],
  "started_at": "2026-04-08T10:30:00Z"
}
```

---

#### `agent_set_status`

Set a human-readable status label for an agent (useful for tracking what each agent is doing).

**Parameters:**

| Parameter     | Type   | Required | Description                       |
|---------------|--------|----------|-----------------------------------|
| `agent_id`    | string | yes      | ID of the agent                   |
| `status_text` | string | yes      | Human-readable status description |

**Response:**

```json
{
  "agent_id": "gemini",
  "status_text": "working on auth refactor"
}
```

---

### Session Tools (4)

#### `agent_new_session`

Create a new ACP session on an initialized agent.

**Parameters:**

| Parameter  | Type   | Required | Description                                                    |
|------------|--------|----------|----------------------------------------------------------------|
| `agent_id` | string | yes      | ID of the initialized agent                                    |
| `cwd`      | string | no       | Working directory for the session (default: current directory) |

**Response:**

```json
{
  "sessionId": "sess_abc123",
  "modes": {
    "available": ["auto", "manual"],
    "current": "manual"
  }
}
```

---

#### `agent_load_session`

Resume a previous session (if the agent supports session persistence).

**Parameters:**

| Parameter    | Type   | Required | Description                       |
|--------------|--------|----------|-----------------------------------|
| `agent_id`   | string | yes      | ID of the agent                   |
| `session_id` | string | yes      | Session ID to resume              |
| `cwd`        | string | no       | Working directory for the session |

**Response:** Same as `agent_new_session`

---

#### `agent_list_sessions`

List all active sessions for an agent.

**Parameters:**

| Parameter  | Type   | Required | Description     |
|------------|--------|----------|-----------------|
| `agent_id` | string | yes      | ID of the agent |

**Response:**

```json
{
  "agent_id": "gemini",
  "sessions": ["sess_abc123", "sess_def456"]
}
```

---

#### `agent_close_session`

Close a session (keeps it resumable if the agent supports persistence).

**Parameters:**

| Parameter    | Type   | Required | Description         |
|--------------|--------|----------|---------------------|
| `agent_id`   | string | yes      | ID of the agent     |
| `session_id` | string | yes      | Session ID to close |

**Response:**

```json
{
  "session_id": "sess_abc123",
  "status": "closed"
}
```

---

### Prompt Tools (4)

#### `agent_prompt`

Send a prompt to an agent session and **block** until complete or timeout. Returns all accumulated events.

**Parameters:**

| Parameter    | Type    | Required | Description                                       |
|--------------|---------|----------|---------------------------------------------------|
| `agent_id`   | string  | yes      | ID of the agent                                   |
| `session_id` | string  | yes      | Session ID                                        |
| `content`    | string  | yes      | Prompt text to send to the agent                  |
| `timeout_ms` | integer | no       | Max wait time in milliseconds (default: `120000`) |

**Response:**

```json
{
  "events": [
    { "type": "update", "update": { "sessionUpdate": "agent_message_chunk", ... } },
    { "type": "update", "update": { "sessionUpdate": "tool_call", ... } },
    { "type": "complete", "stop_reason": "end_turn" }
  ]
}
```

---

#### `agent_prompt_start`

Send a prompt to an agent session (**non-blocking**). Use with `agent_prompt_events` for streaming.

**Parameters:**

| Parameter    | Type   | Required | Description         |
|--------------|--------|----------|---------------------|
| `agent_id`   | string | yes      | ID of the agent     |
| `session_id` | string | yes      | Session ID          |
| `content`    | string | yes      | Prompt text to send |

**Response:**

```json
{
  "status": "prompted",
  "session_id": "sess_abc123"
}
```

---

#### `agent_prompt_events`

Poll for events from a session. Call repeatedly after `agent_prompt_start` until you see `type: "complete"` or `type: "error"`.

**Parameters:**

| Parameter    | Type    | Required | Description                                     |
|--------------|---------|----------|-------------------------------------------------|
| `agent_id`   | string  | yes      | ID of the agent                                 |
| `session_id` | string  | yes      | Session ID                                      |
| `timeout_ms` | integer | no       | Max wait time for new events (default: `30000`) |

**Response:**

```json
{
  "events": [
    { "type": "update", "update": { ... } },
    { "type": "permission_request", "tool_call": {...}, "options": [...] }
  ]
}
```

---

#### `agent_cancel`

Cancel an in-progress prompt.

**Parameters:**

| Parameter    | Type   | Required | Description     |
|--------------|--------|----------|-----------------|
| `agent_id`   | string | yes      | ID of the agent |
| `session_id` | string | yes      | Session ID      |

**Response:**

```json
{
  "session_id": "sess_abc123",
  "status": "cancelled"
}
```

---

### Permission Tools (1)

#### `agent_grant_permission`

Resolve a pending permission request (operator mode). Call after receiving a `permission_request` event.

**Parameters:**

| Parameter    | Type   | Required | Description                                          |
|--------------|--------|----------|------------------------------------------------------|
| `agent_id`   | string | yes      | ID of the agent                                      |
| `session_id` | string | yes      | Session ID with pending permission                   |
| `option_id`  | string | yes      | The `optionId` to select from the permission options |

**Response:**

```json
{
  "status": "granted",
  "option_id": "allow_once"
}
```

---

### Mode Tools (1)

#### `agent_set_mode`

Set the operating mode for a session (if the agent supports modes like `auto`/`manual`).

**Parameters:**

| Parameter    | Type   | Required | Description                                  |
|--------------|--------|----------|----------------------------------------------|
| `agent_id`   | string | yes      | ID of the agent                              |
| `session_id` | string | yes      | Session ID                                   |
| `mode_id`    | string | yes      | Mode identifier (e.g., `"auto"`, `"manual"`) |

**Response:**

```json
{
  "session_id": "sess_abc123",
  "mode": "auto"
}
```

---

## Event Types

Events returned by `agent_prompt` and `agent_prompt_events`:

| Type                 | Description                                                           |
|----------------------|-----------------------------------------------------------------------|
| `update`             | Streaming update from agent (message chunk, tool call, plan, etc.)    |
| `permission_request` | Agent is asking for permission to use a tool                          |
| `complete`           | Prompt finished — check `stop_reason` (`end_turn`, `cancelled`, etc.) |
| `error`              | Prompt failed — check `message`                                       |

### Update Event Structure

The `update` events mirror the ACP `sessionUpdate` types:

```json
{
  "type": "update",
  "update": {
    "sessionUpdate": "agent_message_chunk",
    "chunk": "..."
  },
  "session_id": "sess_abc123",
  "agent_id": "gemini"
}
```

**Common `sessionUpdate` types:**
- `agent_message_chunk` — Streaming response text
- `tool_call` — Agent invoked a tool
- `tool_call_update` — Tool execution progress
- `plan` — Agent's execution plan
- `current_mode_update` — Mode change notification

### Permission Request Event

```json
{
  "type": "permission_request",
  "session_id": "sess_abc123",
  "agent_id": "gemini",
  "tool_call": {
    "toolCallId": "tc_xyz",
    "title": "Run: npm test"
  },
  "options": [
    { "optionId": "allow_once", "name": "Allow once", "kind": "allow_once" },
    { "optionId": "allow_always", "name": "Always allow", "kind": "allow_always" },
    { "optionId": "reject_once", "name": "Reject", "kind": "reject_once" }
  ]
}
```

---

## Multi-Agent Pipelines

You can run multiple agents simultaneously. Each has its own `agent_id`:

```json
// Initialize two agents
{ "tool": "agent_initialize", "params": { "agent_id": "gemini-writer" } }
{ "tool": "agent_initialize", "params": { "agent_id": "gemini-reviewer" } }

// Writer creates code
{ "tool": "agent_new_session", "params": { "agent_id": "gemini-writer", "cwd": "/project" } }
// → Returns sess_A

{ "tool": "agent_prompt", "params": {
  "agent_id": "gemini-writer",
  "session_id": "sess_A",
  "content": "Implement the OAuth2 login flow"
} }

// Reviewer checks it
{ "tool": "agent_new_session", "params": { "agent_id": "gemini-reviewer", "cwd": "/project" } }
// → Returns sess_B

{ "tool": "agent_prompt", "params": {
  "agent_id": "gemini-reviewer",
  "session_id": "sess_B",
  "content": "Review the changes in git diff and suggest improvements"
} }
```

### Pipeline Patterns

**Sequential Pipeline:**
```
Agent A (Research) → Agent B (Implement) → Agent C (Test)
```

**Parallel Review:**
```
                    ┌→ Agent B (Security Review)
Agent A (Code) ────┼→ Agent C (Performance Review)
                    └→ Agent D (Style Review)
```

**Hierarchical:**
```
Orchestrator Agent → Sub-agent 1 (Task A)
                   → Sub-agent 2 (Task B)
                   → Sub-agent 3 (Task C)
```

---

## Troubleshooting

### Agent not found in config

```
Error: Agent 'gemini' not found in config and no command provided.
```

**Solution:** Create `~/.config/exhub/agents.json` or pass `command` directly to `agent_initialize`.

---

### Agent already running

```
Error: Agent 'gemini' is already running. Call agent_shutdown first.
```

**Solution:** Call `agent_shutdown` before re-initializing, or use a different `agent_id`.

---

### Session not active

```
Error: Agent 'gemini' is not running.
```

**Solution:** The agent process may have crashed. Call `agent_list_running` to check, then re-initialize with `agent_initialize`.

---

### Prompt timeout

**Solution:** Increase `timeout_ms` in `agent_prompt`, or switch to the non-blocking `agent_prompt_start` + `agent_prompt_events` pattern.

---

### Permission request blocking

**Solution:** Poll `agent_prompt_events` and call `agent_grant_permission` with the appropriate `option_id` from the event's `options` list.

---

### Agent process crashes immediately

**Check:**
1. Is the agent binary installed and on PATH? (`which gemini`)
2. Does the agent support ACP mode? (check `--acp` flag)
3. Check Exhub logs for stderr output from the agent process

---

## Source Files

| File                                               | Description                                                                 |
|----------------------------------------------------|-----------------------------------------------------------------------------|
| `lib/exhub/mcp/agent_server.ex`                    | MCP server definition (Anubis) with 15 tool components                      |
| `lib/exhub/mcp/agent/store.ex`                     | GenServer for agent state management, event queues, and permission handling |
| `lib/exhub/mcp/agent/config.ex`                    | Configuration loader for `~/.config/exhub/agents.json`                      |
| `lib/exhub/mcp/agent/handler.ex`                   | ACP protocol handler implementing `ExMCP.ACP.Client.Handler` behaviour      |
| `lib/exhub/mcp/tools/agent/initialize.ex`          | `agent_initialize` tool — spawn and handshake                               |
| `lib/exhub/mcp/tools/agent/shutdown.ex`            | `agent_shutdown` tool — graceful termination                                |
| `lib/exhub/mcp/tools/agent/list_running_agents.ex` | `agent_list_running` tool — list all agents                                 |
| `lib/exhub/mcp/tools/agent/get_agent_status.ex`    | `agent_get_status` tool — detailed status                                   |
| `lib/exhub/mcp/tools/agent/set_agent_status.ex`    | `agent_set_status` tool — status label                                      |
| `lib/exhub/mcp/tools/agent/new_session.ex`         | `agent_new_session` tool — create session                                   |
| `lib/exhub/mcp/tools/agent/load_session.ex`        | `agent_load_session` tool — resume session                                  |
| `lib/exhub/mcp/tools/agent/list_sessions.ex`       | `agent_list_sessions` tool — list sessions                                  |
| `lib/exhub/mcp/tools/agent/close_session.ex`       | `agent_close_session` tool — close session                                  |
| `lib/exhub/mcp/tools/agent/prompt.ex`              | `agent_prompt` tool — blocking prompt                                       |
| `lib/exhub/mcp/tools/agent/prompt_start.ex`        | `agent_prompt_start` tool — non-blocking prompt                             |
| `lib/exhub/mcp/tools/agent/prompt_events.ex`       | `agent_prompt_events` tool — poll for events                                |
| `lib/exhub/mcp/tools/agent/cancel.ex`              | `agent_cancel` tool — cancel prompt                                         |
| `lib/exhub/mcp/tools/agent/grant_permission.ex`    | `agent_grant_permission` tool — resolve permissions                         |
| `lib/exhub/mcp/tools/agent/set_mode.ex`            | `agent_set_mode` tool — set session mode                                    |

---

## See Also

- [Quick Start Guide](../agent-mcp-quickstart.md) — Step-by-step setup and usage
- [Desktop MCP Server](./desktop.md) — Filesystem and process management tools
- [Todo MCP Server](./todo.md) — Multi-tenant task tracking
