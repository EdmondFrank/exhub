# Exhub Agent MCP Server — Quick Start Guide

Bridge any MCP client (Claude Code, Zed, VS Code, etc.) to ACP coding agents
(Gemini CLI, Claude Code, Codex, OpenCode, etc.) via Exhub's `/agent/mcp` endpoint.

---

## 1. Prerequisites

- Exhub running (`mix run --no-halt` or release)
- At least one ACP-compatible coding agent installed, e.g.:
  ```bash
  npm install -g @google/gemini-cli   # Gemini CLI
  npm install -g opencode             # OpenCode
  pip install claude-code             # Claude Code (ACP mode)
  ```

---

## 2. Configure Your Agents

Create `~/.config/exhub/agents.json`. Each key is an **agent_id** you choose;
the value describes how to spawn the agent process.

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

> **Tip:** The `agent_id` is just a label — you can have multiple entries for
> the same binary (e.g. `"gemini-work"` and `"gemini-personal"`).

You can also skip the config file entirely and pass `command` directly when
calling `agent_initialize`.

---

## 3. Add Exhub to Your MCP Client

### Claude Code (`~/.claude/settings.json`)
```json
{
  "mcpServers": {
    "exhub-agent": {
      "type": "http",
      "url": "http://localhost:9069/agent/mcp"
    }
  }
}
```

### Zed (`settings.json`)
```json
{
  "context_servers": {
    "exhub-agent": {
      "command": { "path": "curl", "args": ["-s", "http://localhost:9069/agent/mcp"] }
    }
  }
}
```

### Any MCP client (generic HTTP/SSE)
```
POST http://localhost:9069/agent/mcp
```

---

## 4. Basic Workflow

### Step 1 — Initialize an agent

```
Call: agent_initialize
  agent_id: "gemini"
```

Or without a config file, pass the command directly:

```
Call: agent_initialize
  agent_id: "my-gemini"
  command: "gemini"
  args: ["--acp"]
```

Response:
```json
{ "agent_id": "gemini", "status": "initialized", "command": ["gemini", "--acp"] }
```

---

### Step 2 — Create a session

```
Call: agent_new_session
  agent_id: "gemini"
  cwd: "/path/to/your/project"
```

Response:
```json
{ "sessionId": "sess_abc123", "modes": { ... } }
```

Save the `sessionId` — you'll use it for all subsequent calls.

---

### Step 3 — Send a prompt (blocking)

```
Call: agent_prompt
  agent_id: "gemini"
  session_id: "sess_abc123"
  content: "Refactor the auth module to use JWT"
  timeout_ms: 120000
```

Response:
```json
{
  "events": [
    { "type": "update", "update": { "sessionUpdate": "agent_message_chunk", ... } },
    { "type": "update", "update": { "sessionUpdate": "tool_call", ... } },
    { "type": "complete", "stop_reason": "end_turn" }
  ]
}
```

The call blocks until the agent finishes or `timeout_ms` is reached.

---

### Step 4 — Send a prompt (non-blocking / streaming)

For long-running tasks, use the fire-and-poll pattern:

```
# 1. Fire the prompt
Call: agent_prompt_start
  agent_id: "gemini"
  session_id: "sess_abc123"
  content: "Write comprehensive tests for the payment module"

# Response immediately:
{ "status": "prompted", "session_id": "sess_abc123" }

# 2. Poll for events (repeat until you see type: "complete" or "error")
Call: agent_prompt_events
  agent_id: "gemini"
  session_id: "sess_abc123"

# Response (may be empty if agent is still thinking):
{ "events": [ { "type": "update", ... }, ... ] }
```

---

### Step 5 — Handle permission requests (operator mode)

When an agent wants to run a command or edit a file, it emits a
`permission_request` event. You'll see it in `agent_prompt_events`:

```json
{
  "type": "permission_request",
  "tool_call": { "toolCallId": "tc_xyz", "title": "Run: npm test" },
  "options": [
    { "optionId": "allow_once", "name": "Allow once", "kind": "allow_once" },
    { "optionId": "reject_once", "name": "Reject", "kind": "reject_once" }
  ]
}
```

Grant or deny it:

```
Call: agent_grant_permission
  agent_id: "gemini"
  session_id: "sess_abc123"
  option_id: "allow_once"
```

---

### Step 6 — Manage sessions

```
# List active sessions for an agent
Call: agent_list_sessions
  agent_id: "gemini"

# Resume a previous session (agent must support it)
Call: agent_load_session
  agent_id: "gemini"
  session_id: "sess_abc123"
  cwd: "/path/to/project"

# Close a session (keeps it resumable)
Call: agent_close_session
  agent_id: "gemini"
  session_id: "sess_abc123"
```

---

### Step 7 — Shut down

```
Call: agent_shutdown
  agent_id: "gemini"
```

---

## 5. Multi-Agent Pipelines

You can run multiple agents simultaneously. Each has its own `agent_id`:

```
# Initialize two agents
agent_initialize  agent_id: "gemini-writer"
agent_initialize  agent_id: "gemini-reviewer"

# Writer creates code
agent_new_session  agent_id: "gemini-writer",  cwd: "/project"  → sess_A
agent_prompt       agent_id: "gemini-writer",  session_id: sess_A
                   content: "Implement the OAuth2 login flow"

# Reviewer checks it
agent_new_session  agent_id: "gemini-reviewer", cwd: "/project" → sess_B
agent_prompt       agent_id: "gemini-reviewer", session_id: sess_B
                   content: "Review the changes in git diff and suggest improvements"
```

---

## 6. Status & Monitoring

```
# List all running agents
Call: agent_list_running
→ { "agents": [ { "agent_id": "gemini", "status": "initialized", "sessions": [...] } ] }

# Detailed status of one agent
Call: agent_get_status
  agent_id: "gemini"
→ { "agent_id": "gemini", "connection_status": :connected, "sessions": [...], ... }

# Set a human-readable status label
Call: agent_set_status
  agent_id: "gemini"
  status_text: "working on auth refactor"

# Cancel an in-progress prompt
Call: agent_cancel
  agent_id: "gemini"
  session_id: "sess_abc123"

# Switch agent mode (if supported)
Call: agent_set_mode
  agent_id: "gemini"
  session_id: "sess_abc123"
  mode_id: "auto"
```

---

## 7. Tool Reference

| Tool | Required params | Description |
|------|----------------|-------------|
| `agent_initialize` | `agent_id` | Spawn & handshake an ACP agent |
| `agent_shutdown` | `agent_id` | Gracefully shut down |
| `agent_new_session` | `agent_id` | Create a new session |
| `agent_load_session` | `agent_id`, `session_id` | Resume a session |
| `agent_list_sessions` | `agent_id` | List active sessions |
| `agent_close_session` | `agent_id`, `session_id` | Close a session |
| `agent_prompt` | `agent_id`, `session_id`, `content` | Prompt + block until done |
| `agent_prompt_start` | `agent_id`, `session_id`, `content` | Prompt (non-blocking) |
| `agent_prompt_events` | `agent_id`, `session_id` | Poll for events |
| `agent_grant_permission` | `agent_id`, `session_id`, `option_id` | Resolve permission request |
| `agent_cancel` | `agent_id`, `session_id` | Cancel active prompt |
| `agent_set_mode` | `agent_id`, `session_id`, `mode_id` | Set agent mode |
| `agent_list_running` | — | List all running agents |
| `agent_get_status` | `agent_id` | Detailed agent status |
| `agent_set_status` | `agent_id`, `status_text` | Set status label |

---

## 8. Event Types

Events returned by `agent_prompt` and `agent_prompt_events`:

| `type` | Description |
|--------|-------------|
| `update` | Streaming update from agent (message chunk, tool call, plan, etc.) |
| `permission_request` | Agent is asking for permission to use a tool |
| `complete` | Prompt finished — check `stop_reason` (`end_turn`, `cancelled`, etc.) |
| `error` | Prompt failed — check `message` |

The `update` events mirror the ACP `sessionUpdate` types:
`agent_message_chunk`, `tool_call`, `tool_call_update`, `plan`, `current_mode_update`, etc.

---

## 9. Troubleshooting

**Agent not found in config**
```
Error: Agent 'gemini' not found in config and no command provided.
```
→ Create `~/.config/exhub/agents.json` or pass `command` directly to `agent_initialize`.

**Agent already running**
```
Error: Agent 'gemini' is already running. Call agent_shutdown first.
```
→ Call `agent_shutdown` before re-initializing, or use a different `agent_id`.

**Session not active**
```
Error: Agent 'gemini' is not running.
```
→ The agent process may have crashed. Call `agent_list_running` to check, then re-initialize.

**Prompt timeout**
→ Increase `timeout_ms` in `agent_prompt`, or switch to the non-blocking
`agent_prompt_start` + `agent_prompt_events` pattern.

**Permission request blocking**
→ Poll `agent_prompt_events` and call `agent_grant_permission` with the
appropriate `option_id` from the event's `options` list.
