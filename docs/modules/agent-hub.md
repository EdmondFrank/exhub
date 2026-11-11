# Agent Hub (exhub-agent-hub)

Multi-conversation agent platform powered by `sagents`. Each conversation is
one agent thread with its own transcript, tool calls, approvals, and restored
middleware state.

## Features

- **Conversation store** — SQLite (`~/.config/exhub/agent_hub/hub.db`, WAL) via
  `Exhub.Sagents.Store`: conversations, display messages, serialized agent
  state, ordered by `last_message_at`
- **Agent Factory** — agents defined in code with system prompts, middleware,
  and MCP tool groups (`Exhub.Sagents.Factory`)
- **Lazy agent lifecycle** — one agent process per conversation, started on the
  first chat (or page load), stopped by the sagents inactivity timeout, state
  restored from SQLite on the next start
- **Live web UI** — server-rendered transcript plus a long-lived SSE feed for
  turns, tool status, approvals, and multi-tab updates
- **MCP server** — endpoint at `/agent-hub/mcp` (streamable HTTP). The hub MCP
  tools are currently commented out in `Exhub.Sagents.AgentHubServer`; the HTTP
  API and web UI are fully operational.

## Web UI

### Conversation list

`http://localhost:9069/agent-hub`

Lists registered agents and conversations (most recently active first), with
create/rename/delete and start/stop controls. Each agent profile is rendered as
a card showing what it can do: its MCP tool groups (with tool counts), its
native in-process tools, and its middleware (HITL entries also list the tools
they pause on). Clicking a card selects it in the "New conversation" picker,
which repeats the selected profile's tool groups as a hint.

### Conversation

`http://localhost:9069/agent-hub/c/{conversation_id}`

- The transcript is rendered on the server from SQLite, so a reload or a second
  browser shows the whole conversation.
- Message text is rendered as Markdown (`Exhub.Sagents.Markdown`, MDEx). Rows
  carry a pre-rendered `html` field, so the server-rendered first paint and the
  rows that arrive live over SSE look identical. Raw HTML in a message is never
  passed through (`unsafe` rendering stays off). Streaming deltas stay plain
  text and are replaced by the rendered row when it is persisted.
- An `EventSource` on `GET /agent-hub/c/{id}/events` keeps the page live: new
  rows, tool status changes, streamed deltas, status changes, and pending
  approvals all arrive without polling or page reloads.
- `POST /agent-hub/c/{id}/messages` queues a message and returns immediately;
  the response body is not read.
- Approve/Reject posts to `/decision`; the resumed turn streams on the same
  feed.

The legacy `/agent-hub/agents/{name}/chat` URL redirects to the agent's newest
conversation.

## API endpoints

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/agent-hub` | Conversation list page |
| `GET` | `/agent-hub/c/{id}` | Conversation page |
| `GET` | `/agent-hub/c/{id}/events` | Long-lived SSE feed (EventSource) |
| `POST` | `/agent-hub/c/{id}/messages` | Queue a message; `202` immediately |
| `POST` | `/agent-hub/c/{id}/chat` | Send and stream this turn (legacy SSE) |
| `POST` | `/agent-hub/c/{id}/decision` | Approve/reject a HITL interrupt |
| `GET` | `/agent-hub/c/{id}/status` | Conversation status |
| `GET` | `/agent-hub/c/{id}/interrupt` | Pending approval payload, if any |
| `POST` | `/agent-hub/c/{id}/reset` | Clear transcript and agent state |
| `POST` | `/agent-hub/c/{id}/stop` | Stop the conversation's agent |
| `POST` | `/agent-hub/c/{id}/start` | Start the agent without sending a message |
| `GET` | `/agent-hub/conversations` | List conversations (optional `?agent=`) |
| `POST` | `/agent-hub/conversations` | Create a conversation |
| `PATCH` | `/agent-hub/conversations/{id}` | Rename |
| `DELETE` | `/agent-hub/conversations/{id}` | Delete (stops the agent first) |
| `GET` | `/agent-hub/agents` | Registered agent profiles with `tool_groups`, `native_tools`, `middleware` |
| `POST` | `/agent-hub/agents/{name}/chat` | Legacy agent-name SSE chat |
| `GET` | `/agent-hub/agents/{name}/status` | Aggregate status for an agent |
| `POST` | `/agent-hub/agents/{name}/start`, `/stop`, `/reset` | Legacy controls |

## SSE contract

`GET /agent-hub/c/{id}/events` responds with `text/event-stream` and:

- `retry: 3000` once at open, followed by named events.
- A `: ping` comment every 15 seconds. If the agent process has gone away, the
  stream sends `status: not_running` and closes so the client reconnects.
- `event: snapshot` first — `{messages, status, alive, running, interrupt}`.
  Rows are de-duplicated by `id`, so a reconnect is safe. `alive` means the
  agent process exists; `running` means a turn is executing right now.
- `event: display_message` — a saved transcript row (`id`, `message_type`,
  `content_type`, `content`, `status`, `call_id`, …). `text` rows also carry
  `html`, the Markdown-rendered body; the client uses it directly.
- `event: tool_status` — `{call_id, name, status, display_text}` for
  `executing`, `completed`, `failed`, `interrupted`, or `cancelled`.
- `event: delta` — `{text}` LLM output as it streams.
- `event: status` — `{status, running, alive, payload}`; `payload` carries the
  approval summary for `interrupted`. `alive` is `false` only for
  `not_running` (the agent process is gone).
- `event: queued` — a message was queued (sagents 0.10+ only).
- The connection closes after 30 minutes; `EventSource` reconnects and receives
  a fresh snapshot. A closed HTTP connection ends the producer subscription.

The POST `/chat` endpoints keep the older single-turn format: repeated
`data: {json}` lines with `type` of `delta`, `tool_call`, `tool_result`,
`complete`, `interrupted`, `cancelled`, `paused`, or `error`, terminated by
`data: [DONE]`.

## MCP tools

The endpoint `/agent-hub/mcp` is wired up, but the components in
`lib/exhub/sagents/tools/` are commented out in `AgentHubServer` during the
conversation refactor. The modules (`agent_hub_list`, `agent_hub_start`,
`agent_hub_chat`, `agent_hub_status`, `agent_hub_reset`, `agent_hub_stop`) call
the same `Exhub.Sagents.Hub` API and can be re-enabled by uncommenting them.

## Default agents

- **coder** — desktop + web-tools, HITL for `execute_command` and `write_file`
- **researcher** — web-tools + brain + look
- **assistant** — desktop + web-tools + todo
- **genclaw** — image generation via `kimi-k2.6`; perception + completion-guard
  middleware and native `t2i`, `i2i`, `search`, `reason`, `format_prompt`,
  `vlm_review`, `code_scene_draft`, `code_text_draft` tools
- **toonflow** — short-drama pipeline over the native `toonflow_*` tools

## Architecture

- **`Exhub.Sagents.Hub`** — conversation lifecycle GenServer, turn streams, and
  the follow-mode event state machine
- **`Exhub.Sagents.Store`** — SQLite persistence (conversations, display
  messages, agent state)
- **`Exhub.Sagents.Persistence`** — `Sagents.AgentPersistence` over the store
- **`Exhub.Sagents.DisplayMessages`** — `Sagents.DisplayMessagePersistence`
  over the store
- **`Exhub.Sagents.Factory`** — builds `LangChain` models and agents from
  Exhub's LLM configuration
- **`Exhub.Sagents.McpAdapter`** — bridges Exhub MCP tools to
  `LangChain.Function`, passing JSON Schema through as `parameters_schema`
- **`Exhub.Sagents.AgentHubServer`** — MCP server for the hub

## Adding a custom agent

Add an entry to `agents/0` in `lib/exhub/sagents/factory.ex`:

```elixir
"my-agent" => %{
  system_prompt: "You are a specialized assistant...",
  mcp_tools: [:desktop, :brain],
  middleware: [
    {Sagents.Middleware.TodoList, []},
    {Sagents.Middleware.FileSystem, []}
  ]
}
```

Then hot-reload the code (the running release picks up new `:exhub` modules).

## Dependencies

- `sagents ~> 0.9.0` — agent orchestration
- `langchain >= 0.8.11` — LLM integration
- `ex_mcp ~> 0.9` + Anubis — MCP servers over streamable HTTP