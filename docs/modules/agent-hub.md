# Agent Hub (exhub-agent-hub)

MCP-based agent orchestration platform powered by sagents. Manage multiple
LLM-backed agents with middleware, tools, and conversation state.

## Features

- **Agent Factory** — Define agents in code with custom system prompts, middleware, and MCP tool groups
- **Chat API** — HTTP REST with SSE streaming at `POST /agent-hub/agents/{name}/chat`
- **MCP Server** — Single hub endpoint at `/agent-hub/mcp` with tools for agent management
- **Selective MCP Tool Injection** — Each agent can use specific exhub MCP tool groups (desktop, web-tools, brain, etc.)
- **Optional Persistence** — File-based state persistence per agent
- **Web UI** — Browser-based interface for managing and chatting with agents

## Web UI

The Agent Hub provides web pages for managing and interacting with agents:

### Agent Overview

URL: `http://localhost:9069/agent-hub`

Displays all registered agents with their running status. Provides buttons to:
- **Start** — Start a stopped agent
- **Stop** — Stop a running agent
- **Chat** — Open chat interface for a running agent
- **Reset** — Reset agent conversation state

### Agent Chat

URL: `http://localhost:9069/agent-hub/agents/{name}/chat`

Real-time chat interface for a specific agent. Features:
- Server-Sent Events (SSE) for streaming responses
- Typing indicators
- Tool call notifications
- Message formatting (code blocks, inline code)
- Auto-resizing input
- Keyboard shortcuts (Enter to send, Shift+Enter for newline)

## API Endpoints

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/agent-hub/agents` | List all agents |
| `POST` | `/agent-hub/agents/{name}/start` | Start an agent |
| `POST` | `/agent-hub/agents/{name}/chat` | Chat with agent (SSE stream) |
| `GET` | `/agent-hub/agents/{name}/status` | Agent status |
| `POST` | `/agent-hub/agents/{name}/reset` | Reset agent state |
| `POST` | `/agent-hub/agents/{name}/stop` | Stop agent |

## MCP Tools

> **Note:** MCP tools are temporarily disabled in `AgentHubServer` during refactoring.
> The HTTP REST API and Web UI remain fully operational.

| Tool | Description |
|------|-------------|
| `agent_hub_list` | List all registered agents |
| `agent_hub_start` | Start an agent |
| `agent_hub_chat` | Send message to agent |
| `agent_hub_status` | Get agent status |
| `agent_hub_reset` | Reset agent state |
| `agent_hub_stop` | Stop a running agent |

## Default Agents

- **coder** — Coding assistant with desktop + web-tools, HITL for dangerous operations
- **researcher** — Research assistant with web-tools + brain + look
- **assistant** — General-purpose with desktop + web-tools + todo
- **genclaw** — Image generation agent powered by kimi-k2.6. Uses perception middleware (sub-LLM extracts painter notes) and completion guard middleware. Native tools: `t2i`, `i2i`, `search`, `reason`, `format_prompt`, `vlm_review`, `code_scene_draft` (SVG → headless Chrome PNG for exact object counts/positions), `code_text_draft` (HTML/CSS rendering for verbatim text). All image generation via Gitee AI (Qwen-Image-2512). See [priv/genclaw/diagrams.txt](../../priv/genclaw/diagrams.txt) for architecture diagrams.

## Chat Example

```bash
curl -N http://localhost:9069/agent-hub/agents/assistant/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "What files are in the current directory?"}'
```

## MCP Example

```bash
# List agents via MCP
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"agent_hub_list","arguments":{}}}' | \
  curl -s -X POST http://localhost:9069/agent-hub/mcp \
  -H "Content-Type: application/json" \
  -H "Accept: application/json" \
  -d @-
```

## Architecture

The Agent Hub is built on the `sagents` agent orchestration framework:

- **`Exhub.Sagents.Factory`** — Creates agents using exhub's LLM config
- **`Exhub.Sagents.Hub`** — GenServer managing agent lifecycle (lazy start, chat routing)
- **`Exhub.Sagents.McpAdapter`** — Bridges exhub MCP tools → LangChain.Function
- **`Exhub.Sagents.AgentHubServer`** — MCP server exposing hub management tools
- **`Exhub.Sagents.Persistence`** — Optional file-based state persistence

## Adding Custom Agents

Edit `lib/exhub/sagents/factory.ex` and add to the `agents/0` function:

```elixir
"my-agent" => %{
  system_prompt: "You are a specialized assistant...",
  mcp_tools: [:desktop, :brain],
  middleware: [
    {Sagents.Middleware.TodoList, []},
    {Sagents.Middleware.FileSystem, []}
  ],
  persistence: :file  # optional
}
```

Then hot-reload: `curl -X POST http://localhost:9069/system/reload`

## MCP Tool Groups

Agents can selectively use exhub's MCP tools:

| Group | Tools |
|-------|-------|
| `:desktop` | File read/write/edit, process management, search |
| `:"web-tools"` | Web search, URL fetch |
| `:brain` | Obsidian vault (list/search notes) |
| `:look` | Image understanding via vision models |
| `:todo` | Todo list management |
| `:browser-use` | Chrome browser automation |
| `:image-gen` | AI image generation |
| `:doc-extract` | Document text extraction |

## Dependencies

- `sagents ~> 0.7.0` — Agent orchestration framework
- `langchain ~> 0.8.0` — LLM integration (upgraded from 0.4.1)
- `anubis_mcp ~> 1.0.0` — MCP server framework
