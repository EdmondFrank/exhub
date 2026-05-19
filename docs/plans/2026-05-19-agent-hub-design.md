# Agent Hub Platform Design

**Date**: 2026-05-19
**Status**: Approved
**Package**: `sagents` (~/Code/sagents)

## Overview

Integrate the `sagents` agent orchestration framework into `exhub` to create an Agent Hub platform. The hub manages multiple LLM-backed agents with middleware, tools, and conversation state, exposed via HTTP REST API and MCP server endpoints.

## Goals

1. **Agent Hub** — Manage sagents-defined agents (create, list, configure) via a code-based factory
2. **Agent API** — Chat with specific agents via HTTP REST with SSE streaming
3. **Agent MCP** — Expose agents as MCP servers (single hub endpoint + per-agent virtual routes)

## Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| LLM backend | Reuse exhub's `LlmConfigServer` | Unified API key management via SecretVault |
| Agent definition | Code-based factory (`Sagents.Factory` behaviour) | Type-safe, programmatic control |
| MCP exposure | Both hub + per-agent endpoints | Hub for discovery, per-agent for direct access |
| Chat API | HTTP REST + SSE streaming | Simple integration, supports streaming |
| State persistence | In-memory default, opt-in file | Lightweight default, persistence when needed |
| MCP tool injection | Selective per agent | Controlled tool sets, smaller context windows |
| Agent creation | Factory-only (compile-time) | Predictable, hot-reloadable |

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                    Exhub Application                 │
│                                                      │
│  ┌──────────────┐  ┌──────────────┐  ┌────────────┐ │
│  │ Exhub.Router  │  │ Exhub.Router  │  │ MCP Hub    │ │
│  │ /agent-hub/*  │  │ /agent-hub/   │  │ /mcp-hub   │ │
│  │ agents/chat   │  │ {name}/mcp    │  │ (existing)  │ │
│  └──────┬───────┘  └──────┬───────┘  └────────────┘ │
│         │                  │                          │
│  ┌──────▼──────────────────▼───────────────────────┐ │
│  │           Exhub.Sagents.Hub (GenServer)         │ │
│  │  - Agent lifecycle management                    │ │
│  │  - Message routing                               │ │
│  │  - State persistence coordinator                 │ │
│  └──────────────────┬──────────────────────────────┘ │
│                     │                                 │
│  ┌──────────────────▼──────────────────────────────┐ │
│  │        Exhub.Sagents.Factory (behaviour)        │ │
│  │  - Agent definitions (model, prompt, tools)     │ │
│  │  - MCP tool injection adapter                    │ │
│  │  - Uses Exhub.Llm.LlmConfigServer for models   │ │
│  └──────────────────┬──────────────────────────────┘ │
│                     │                                 │
│  ┌──────────────────▼──────────────────────────────┐ │
│  │              Sagents.Supervisor                  │ │
│  │  ┌─────────────────────────────────────────┐    │ │
│  │  │  Sagents.AgentSupervisor ("coder")      │    │ │
│  │  │  ├── Sagents.AgentServer                 │    │ │
│  │  │  └── Sagents.SubAgentsDynamicSupervisor  │    │ │
│  │  └─────────────────────────────────────────┘    │ │
│  │  ┌─────────────────────────────────────────┐    │ │
│  │  │  Sagents.AgentSupervisor ("researcher") │    │ │
│  │  │  ├── Sagents.AgentServer                 │    │ │
│  │  │  └── Sagents.SubAgentsDynamicSupervisor  │    │ │
│  │  └─────────────────────────────────────────┘    │ │
│  └─────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────┘
```

## Modules

### Exhub.Sagents.Factory

Implements `Sagents.Factory` behaviour. Builds `Sagents.Agent` structs from agent_id + config.

- `create_agent/2` called by Hub when starting an agent
- Uses `Exhub.Llm.LlmConfigServer` to get default model → `LangChain.ChatModels` struct
- Each agent profile declares: `name`, `base_system_prompt`, `middleware`, `mcp_tools`
- MCP tool adapter converts selected exhub MCP tool groups into `LangChain.Function` structs

### Exhub.Sagents.Hub

GenServer managing agent lifecycle.

- State: `%{agents: %{name => %{agent: Agent.t(), pid: pid(), config: map()}}}`
- `start_agent(name)` — starts `Sagents.AgentServer` on-demand (lazy)
- `chat(name, message)` — sends user message, executes agent, returns response
- `chat_stream(name, message)` — returns a stream of events for SSE
- `get_state(name)` — returns current agent state
- `reset(name)` — clears agent state
- `list_agents()` — returns all registered agent profiles + running status

### Exhub.Sagents.McpAdapter

Bridges exhub MCP tools → LangChain.Function.

- Takes a list of MCP tool group atoms (e.g., `[:desktop, :web-tools]`)
- For each group, queries `Exhub.MCP.Hub.BuiltInRegistry.list_tools(group)`
- Wraps each MCP tool as a `LangChain.Function` that calls `BuiltInRegistry.call_tool/3`

### Exhub.Sagents.AgentHubServer

MCP server (Anubis.Server) at `/agent-hub/mcp`.

Tools:
- `agent_hub_list` — list all registered agents
- `agent_hub_start` — start an agent
- `agent_hub_chat` — send message to agent, get response
- `agent_hub_status` — get agent status
- `agent_hub_reset` — reset agent state
- `agent_hub_stop` — stop a running agent

### Per-agent MCP endpoints

Dynamic virtual routes at `/agent-hub/{name}/mcp`. Each agent's tools exposed directly using the same pattern as MCP Hub virtual route proxy.

## REST API

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/agent-hub/agents` | List all agents |
| `POST` | `/agent-hub/agents/{name}/chat` | Send message, SSE stream response |
| `GET` | `/agent-hub/agents/{name}/status` | Agent status |
| `POST` | `/agent-hub/agents/{name}/reset` | Reset agent state |
| `POST` | `/agent-hub/agents/{name}/stop` | Stop agent |

### Chat API Example

```
POST /agent-hub/agents/coder/chat
Content-Type: application/json

{"message": "Read the file lib/exhub.ex and summarize it"}

→ SSE stream:
data: {"type": "delta", "text": "Let me read..."}
data: {"type": "delta", "text": " The file defines..."}
data: {"type": "tool_call", "tool": "desktop__read_file", "args": {"path": "lib/exhub.ex"}}
data: {"type": "tool_result", "tool": "desktop__read_file", "result": "..."}
data: {"type": "complete", "text": "The file defines the main Exhub module..."}
data: [DONE]
```

## Files to Create/Modify

| File | Action |
|------|--------|
| `mix.exs` | Add `{:sagents, "~> 0.7.0"}` dependency |
| `lib/exhub/application.ex` | Add `Sagents.Supervisor` to supervision tree |
| `lib/exhub/sagents/factory.ex` | **New** — Factory implementing `Sagents.Factory` |
| `lib/exhub/sagents/hub.ex` | **New** — Hub GenServer |
| `lib/exhub/sagents/mcp_adapter.ex` | **New** — MCP tool → LangChain.Function adapter |
| `lib/exhub/sagents/persistence.ex` | **New** — Optional file persistence |
| `lib/exhub/sagents/agent_hub_server.ex` | **New** — MCP server for hub tools |
| `lib/exhub/sagents/tools/*.ex` | **New** — MCP tool components |
| `lib/exhub/router.ex` | Add REST routes + MCP forward |
| `docs/modules/agent-hub.md` | **New** — Documentation |
