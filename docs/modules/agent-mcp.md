# ACP Agent MCP Server

## Overview

The ACP Agent MCP Server provides a bridge between MCP clients and ACP (Agent Communication Protocol) agents like Claude Code, Gemini CLI, OpenCode, and Codex. It enables spawning, managing, and interacting with AI coding agents via MCP tools.

**HTTP Endpoint:** `POST /agent/mcp`

This server acts as a gateway that translates MCP tool calls into ACP protocol messages, allowing any MCP-compatible client to orchestrate AI coding agents for complex development workflows.

> **Control plane vs peer plane.** This server manages agent *processes*. For
> agents that need to talk to each other — direct messages, shared rooms, fan-in
> waits without polling — use the [Agent Bus](agent-bus.md) at `/agent-bus/mcp`.
> `agent_new_session(..., expose_bus: true)` hands a spawned subagent that
> endpoint, pre-addressed to its own id.

### Recommended orchestration loop

```text
agent_task {agent_id, content, cwd}                  # spawn + session + prompt + digest
```

That is the whole loop for a one-off delegation. Reach for the lower-level tools
only when the session must survive the turn:

```text
agent_initialize → agent_new_session(keep) → agent_prompt(keep)   # one digest per turn
agent_prompt_start → agent_await(agents)                          # fire-and-forget + fan-in
agent_prompt_start → agent_prompt_events (only if you need the live stream)
```

The cost model matters: an agent turn is cheap for the subagent but each MCP
round trip re-reads the orchestrator's whole context. A one-call digest costs
one turn; the old poll loop cost one turn per poll, including the empty ones.

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
│  │ agent_initialize│ │ agent_new_session│ │ agent_prompt_start             ││
│  │ agent_shutdown  │ │ agent_load_session││ agent_prompt_events            ││
│  │ agent_list_...  │ │ agent_list_sessions││ agent_cancel                  ││
│  │ agent_get_status│ │ agent_close_session││ agent_grant_permission        ││
│  │ agent_set_status│ │                 │ │ agent_set_mode                  ││
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
│  │    waiters: %{},         # Pending single-event waiters                 ││
│  │    collect_waiters: %{}, # Long-poll collect waiters                    ││
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
    "args": ["acp"]
  },
  "codex": {
    "command": "codex-acp",
    "args": []
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

| Parameter  | Type   | Required | Description                               |
|------------|--------|----------|-------------------------------------------|
| `agent_id` | string | yes      | Unique identifier for this agent instance |

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
- `"Agent 'X' not found in config."` — No config entry for this agent_id
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

| Parameter     | Type    | Required | Description                                                       |
|---------------|---------|----------|-------------------------------------------------------------------|
| `agent_id`    | string  | yes      | ID of the initialized agent                                       |
| `cwd`         | string  | no       | Working directory for the session (default: current directory)     |
| `expose_bus`  | boolean | no       | Inject the ExHub agent bus, pre-addressed to `agent_id`            |
| `mcp_servers` | array   | no       | Extra servers, see below                                          |

`mcp_servers` follows the ACP spec, and the shape matters — a wrong one is
rejected by the runtime with `Unsupported MCP server transport`:

```json
[
  {"name": "brain", "type": "http", "url": "http://127.0.0.1:9069/brain/mcp", "headers": []},
  {"name": "local", "command": "node", "args": ["bridge.js"], "env": [{"name": "K", "value": "V"}]}
]
```

`uri` is accepted as an alias for `url`, and a `headers`/`env` map is converted
to the required `[{name, value}]` array. `headers: []` is always sent for HTTP
servers because pi dereferences it unconditionally.

**Response:**

```json
{
  "sessionId": "sess_abc123",
  "mcp_servers": [ ... ],
  "mcp_server_count": 1
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

### Prompt Tools (5)

> **Use `agent_task` (or `agent_prompt`) unless you specifically need the live
> stream.** Every `agent_prompt_events` poll is a separate model turn, and an
> agent that is thinking or running a slow command produces empty or partial
> batches. Measured on the `pi` agent: "read two files and answer" cost one
> 27.8 KB poll response and several turns, while `agent_prompt` returns the same
> outcome as a ~2 KB digest in a single call.

#### `agent_task` — one-call delegation (preferred)

Spawns the agent if needed, opens a session, sends the prompt, waits for the
turn to finish and returns a digest. This is the whole cold-start sequence in
one tool call.

**Parameters:**

| Parameter          | Type    | Required | Description                                              |
|--------------------|---------|----------|----------------------------------------------------------|
| `agent_id`         | string  | yes      | Agent from `agents.json`                                 |
| `content`          | string  | yes      | Prompt text                                              |
| `cwd`              | string  | no       | Session working directory                                |
| `expose_bus`       | boolean | no       | Inject the ExHub agent bus for peer messaging            |
| `mcp_servers`      | array   | no       | Extra MCP servers (ACP shape, see `agent_new_session`)   |
| `timeout_ms`       | integer | no       | Max wait, default `300000`, capped at `570000`           |
| `keep_session`     | boolean | no       | Keep the session for follow-up `agent_prompt` calls      |
| `max_answer_bytes` | integer | no       | Cap on the returned answer (default `4000`)              |

#### `agent_prompt` — run a prompt to completion

Same digest, but for an existing session.

| Parameter          | Type    | Required | Description                                    |
|--------------------|---------|----------|------------------------------------------------|
| `agent_id`         | string  | yes      | ID of the agent                                |
| `session_id`       | string  | yes      | Session ID                                     |
| `content`          | string  | yes      | Prompt text                                    |
| `timeout_ms`       | integer | no       | Max wait, default `300000`, capped at `570000` |
| `format`           | enum    | no       | `digest` (default) or `full`                   |
| `max_answer_bytes` | integer | no       | Cap on the answer (default `4000`)             |
| `include_thoughts` | boolean | no       | Keep reasoning events with `format: "full"`    |

**Digest response** (one turn, typically 0.4–3 KB):

```json
{
  "agent_id": "pi",
  "session_id": "01a1a00a-980a-7000-aae9-2edd8c071d9f",
  "status": "complete",
  "stop_reason": "end_turn",
  "elapsed_ms": 14531,
  "answer": "Test passes. Change made: in `mathops.py`, `add()` returned `a-b`; changed it to `return a+b`.",
  "answer_truncated": false,
  "usage": {"used": 17003, "size": 256000, "cost": {"amount": 0.0011, "currency": "USD"}},
  "tool_count": 6,
  "tools": [
    {"id": "call_…", "kind": "execute", "status": "completed",
     "title": "$ python3 test_mathops.py && echo PASS", "output": "PASS", "output_bytes": 87}
  ],
  "tools_truncated": false,
  "changed_files": ["/tmp/pi-acp-test3/mathops.py"],
  "errors": [],
  "permission_requests": [],
  "event_count": 19
}
```

`status` is `complete`, `timeout` or `error`. A timeout cancels the prompt. A
permission prompt from the agent blocks the call until it is resolved with
`agent_grant_permission` or the timeout expires.

When a prompt starts, the agent's registry status becomes `working` and returns
to `idle`/`error` at the end, so `agent_await` can be used as a real fan-in wait
instead of a poll loop.

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

Long-poll for events from a session. Prefer `agent_prompt` — every poll costs a
model turn.

Collection: up to `collect_ms` (default `30000`), returning early on a terminal
event (`complete`/`error`), or after `idle_ms` (default `15000`) of silence
**once at least one event has arrived**. A poll with nothing to report waits the
full window instead of returning an empty batch early (that early return used to
cost one wasted turn per quiet interval).

Output is bounded and redacted by default: boilerplate session updates
(`available_commands_update`, `session_info_update`, `config_option_update`,
`current_mode_update`) are dropped at the handler, reasoning chunks are dropped,
each tool output is capped, and the batch is capped at `max_bytes`.

**Parameters:**

| Parameter           | Type    | Required | Description                                                    |
|---------------------|---------|----------|----------------------------------------------------------------|
| `agent_id`          | string  | yes      | ID of the agent                                                |
| `session_id`        | string  | yes      | Session ID                                                     |
| `collect_ms`        | integer | no       | Max collection window in ms (default: `30000`)                 |
| `idle_ms`           | integer | no       | Early return after this much silence, once events exist        |
| `include_thoughts`  | boolean | no       | Keep `agent_thought_chunk` events (default `false`)            |
| `include_raw`       | boolean | no       | Keep `rawInput`/`rawOutput`/tool content uncapped (default `false`) |
| `max_output_bytes`  | integer | no       | Per-tool-output cap (default `800`)                            |
| `max_bytes`         | integer | no       | Whole-response cap (default `20000`)                           |
| `slim`              | boolean | no       | Set `false` for the untouched event list (default `true`)      |

**Response:**

```json
{
  "session_id": "sess_abc123",
  "agent_id": "pi",
  "state": "running",
  "usage": {"used": 17003, "size": 256000, "cost": {"amount": 0.0011, "currency": "USD"}},
  "events": [
    { "type": "update", "update": {"sessionUpdate": "tool_call_update", "toolCallId": "call_…",
                                   "status": "completed", "output": "PASS", "output_bytes": 87} },
    { "type": "permission_request", "tool_call": {...}, "options": [...] }
  ],
  "event_count": 12,
  "dropped": 4,
  "truncated": false
}
```

`state` is `running`, `complete`, `error`, `timeout` or `unknown`. `dropped` and
`truncated` describe what the caps removed from *this* response. Per-session
queues hold at most 1000 events; overflow drops the oldest and appends a
`{"type": "queue_overflow", "dropped": n}` marker.

Only one poll may wait on a session at a time — a second concurrent call returns
an error instead of hanging the first (`already_waiting`).

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

Events returned by `agent_prompt_events`:

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

{ "tool": "agent_prompt_start", "params": {
  "agent_id": "gemini-writer",
  "session_id": "sess_A",
  "content": "Implement the OAuth2 login flow"
} }
// Poll until complete:
{ "tool": "agent_prompt_events", "params": { "agent_id": "gemini-writer", "session_id": "sess_A" } }

// Reviewer checks it
{ "tool": "agent_new_session", "params": { "agent_id": "gemini-reviewer", "cwd": "/project" } }
// → Returns sess_B

{ "tool": "agent_prompt_start", "params": {
  "agent_id": "gemini-reviewer",
  "session_id": "sess_B",
  "content": "Review the changes in git diff and suggest improvements"
} }
// Poll until complete:
{ "tool": "agent_prompt_events", "params": { "agent_id": "gemini-reviewer", "session_id": "sess_B" } }
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

**Solution:** Use `agent_prompt_start` + `agent_prompt_events`. Each poll call long-polls for up to `collect_ms` (default 30s) and returns early after `idle_ms` (default 15s) of silence — no busy-polling needed.

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
| `lib/exhub/mcp/agent_server.ex`                    | MCP server definition (Anubis) with 14 tool components                      |
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
