# MCP Hub

## Overview

The MCP Hub is a unified gateway that aggregates multiple upstream MCP (Model Context Protocol) servers into a single endpoint. It allows MCP clients to discover and invoke tools from all connected upstream servers through one connection, with automatic tool name namespacing to avoid conflicts.

The Hub also provides **virtual route proxying** — each upstream server can optionally be exposed at its own direct endpoint (e.g., `/probe/mcp`), bypassing the namespace prefix for clients that need a 1:1 mapping.

## Architecture

```text
┌─────────────────────────────────────────────────────────────────────────────┐
│                      MCP Client (Claude Code, Zed, etc.)                    │
└──────────────┬──────────────────────────────────────────────┬───────────────┘
               │ HTTP POST /mcp-hub/mcp                       │ HTTP POST /probe/mcp
               │ (unified — all tools)                        │ (virtual — direct proxy)
               ▼                                              ▼
┌──────────────────────────┐              ┌────────────────────────────────────┐
│  Exhub.MCP.Hub.Server    │              │  Exhub.MCP.Hub.ProxyPlug           │
│  (Anubis.Server)         │              │  (Plug — streamable HTTP proxy)    │
│  component: RetrieveTools│              │  Tool: tool (no prefix)            │
│  component: CallTools    │              └──────────────┬─────────────────────┘
└──────────────┬───────────┘                             │
               │                                         │
               ▼                                         ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    Exhub.MCP.Hub.ClientManager (GenServer)                  │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────────────┐   │
│  │ DynamicSupervisor│  │  Config Store    │  │  Tool Aggregation        │   │
│  │  (Anubis.Client) │  │  (JSON file)     │  │  (namespaced listing)    │   │
│  └──────────────────┘  └──────────────────┘  └──────────────────────────┘   │
└───────────┬──────────────────────────────────────────────┬──────────────────┘
            │                                              │
            ▼                                              ▼
┌──────────────────────────┐              ┌────────────────────────────────────┐
│  BuiltInRegistry         │              │  Exhub.MCP.Hub.Store               │
│  (in-process tools)      │              │  (ETS table owner)                 │
│  14 built-in servers     │              │  :mcp_hub_search_index             │
│  direct function calls   │              │  :mcp_hub_proxy_sessions           │
└──────────────────────────┘              └────────────────────────────────────┘
            │
            ▼
     ┌──────────┐  ┌──────────┐  ┌──────────────┐
     │  probe   │  │  skills  │  │  deepwiki    │  ... (any external MCP server)
     │  (stdio) │  │  (stdio) │  │  (HTTP)      │
     └──────────┘  └──────────┘  └──────────────┘
```

### Components

| Module                               | Role                                                                               |
|--------------------------------------|------------------------------------------------------------------------------------|
| `Exhub.MCP.Hub.ClientManager`        | GenServer that manages upstream connections, tool aggregation, and CRUD operations |
| `Exhub.MCP.Hub.ClientState`          | Struct tracking per-server connection state (pid, status, tools, errors)           |
| `Exhub.MCP.Hub.ServerConfig`         | Configuration struct with transport options, validation, and serialization         |
| `Exhub.MCP.Hub.Server`               | Anubis.Server exposing the unified `{server}__{tool}` namespace                    |
| `Exhub.MCP.Hub.ProxyPlug`            | Plug for virtual route proxying to individual upstream servers                     |
| `Exhub.MCP.Hub.BuiltInRegistry`      | Maps 14 built-in MCP servers to their modules for direct in-process tool calls     |
| `Exhub.MCP.Hub.Store`                | GenServer owning ETS tables for search index and proxy sessions                    |
| `Exhub.MCP.Tools.Hub.RetrieveTools`  | Anubis.Server.Component: TF-IDF tool search across all servers                     |
| `Exhub.MCP.Tools.Hub.CallTools`      | Anubis.Server.Component: execute tools on upstream servers via the hub             |
| `Exhub.Controllers.MCPHubController` | REST API controller for management operations                                      |

### Application Supervision

```text
Exhub.Supervisor
  └── Exhub.MCP.Hub.Store              (GenServer — ETS table owner, starts first)
  └── Exhub.MCP.Hub.TaskSupervisor     (Task.Supervisor — async client startup)
  └── Exhub.MCP.Hub.ClientManager      (GenServer — connection lifecycle)
  └── Exhub.MCP.Hub.Server             (Anubis.Server — unified MCP endpoint)
```

---

## Transport Types

The Hub supports all MCP transport types via [Anubis.Client](https://hexdocs.pm/anubis):

| Transport           | Config Key                      | Description                                         |
|---------------------|---------------------------------|-----------------------------------------------------|
| **stdio**           | `"stdio"`                       | Spawns a local process (e.g., `npx`, `uvx`, binary) |
| **SSE**             | `"sse"`                         | Connects to a Server-Sent Events endpoint           |
| **Streamable HTTP** | `"streamable_http"` or `"http"` | Connects via HTTP Streamable transport              |
| **WebSocket**       | `"websocket"`                   | Connects via WebSocket transport                    |

---

## Configuration

### Config File

Servers are configured in `~/.config/exhub/mcp_servers.json` (or the path given by the `EXHUB_MCP_SERVERS_CONFIG` environment variable):

```json
{
  "servers": [
    {
      "name": "probe",
      "transport": "stdio",
      "command": "probe",
      "args": ["mcp"],
      "enabled": true,
      "expose_route": "probe"
    },
    {
      "name": "deepwiki",
      "transport": "streamable_http",
      "url": "https://mcp.deepwiki.com/mcp",
      "enabled": true,
      "expose_route": "deepwiki"
    },
    {
      "name": "remote-mcp",
      "transport": "sse",
      "url": "http://localhost:3001/sse",
      "headers": {"Authorization": "Bearer token"},
      "enabled": false
    }
  ]
}
```

### Server Config Fields

| Field          | Type    | Required | Default   | Description                                                     |
|----------------|---------|----------|-----------|-----------------------------------------------------------------|
| `name`         | string  | yes      | —         | Unique server identifier (used in tool namespace)               |
| `transport`    | string  | yes      | `"stdio"` | One of: `stdio`, `sse`, `streamable_http`/`http`, `websocket`   |
| `enabled`      | boolean | no       | `true`    | Whether the server is active                                    |
| `builtin`      | boolean | no       | `false`   | Whether this is a built-in server (auto-registered, protected)  |
| `command`      | string  | yes*     | —         | Executable command (required for `stdio`)                       |
| `args`         | array   | no       | `[]`      | Arguments passed to the command                                 |
| `env`          | object  | no       | `{}`      | Environment variables for the process                           |
| `url`          | string  | yes*     | —         | Remote URL (required for `sse`, `streamable_http`, `websocket`) |
| `headers`      | object  | no       | `{}`      | HTTP headers for remote transports                              |
| `expose_route` | string  | no       | `null`    | Virtual route name for direct proxy access                      |

### Validation Rules

- `name` is required and must be unique
- `command` is required when `transport` is `stdio`
- `url` is required when `transport` is `sse`, `streamable_http`, or `websocket`

---

## Built-in Server Integration

The Hub automatically registers all **built-in MCP servers** that run in the same BEAM VM. These servers are managed by `Exhub.MCP.Hub.BuiltInRegistry` and are accessed directly via in-process function calls, bypassing the Anubis.Client → HTTP → Anubis.Server indirection.

### Built-in Servers

| Server Name   | Module                       | Route              | Description                       |
|---------------|------------------------------|--------------------|-----------------------------------|
| `habit`       | `Exhub.MCP.HabitServer`      | `/mcp`             | Habit/environment config storage  |
| `time`        | `Exhub.MCP.TimeServer`       | `/time/mcp`        | Timezone-aware time utilities     |
| `think`       | `Exhub.MCP.ThinkServer`      | `/think/mcp`       | Reasoning scratchpad              |
| `web-tools`   | `Exhub.MCP.WebToolsServer`   | `/web-tools/mcp`   | Web search and URL fetch          |
| `archery`     | `Exhub.MCP.ArcheryServer`    | `/archery/mcp`     | SQL audit platform                |
| `browser-use` | `Exhub.MCP.BrowserUseServer` | `/browser-use/mcp` | Chrome browser automation         |
| `image-gen`   | `Exhub.MCP.ImageGenServer`   | `/image-gen/mcp`   | AI image generation               |
| `doc-extract` | `Exhub.MCP.DocExtractServer` | `/doc-extract/mcp` | Document text extraction          |
| `look`        | `Exhub.MCP.LookServer`       | `/look/mcp`        | Image analysis via vision models  |
| `todo`        | `Exhub.MCP.TodoServer`       | `/todo/mcp`        | Multi-tenant todo list management |
| `desktop`     | `Exhub.MCP.DesktopServer`    | `/desktop/mcp`     | Filesystem and process management |
| `agent`       | `Exhub.MCP.AgentServer`      | `/agent/mcp`       | ACP agent bridge                  |
| `brain`       | `Exhub.MCP.BrainServer`      | `/brain/mcp`       | Obsidian vault interface          |
| `exhub`       | `Exhub.MCP.ExhubServer`      | `/exhub/mcp`       | Exhub management tools            |

### How Built-in Servers Work

1. **Auto-registration**: On startup, `ClientManager` merges built-in configs (from `builtin_server_configs/0`) with external configs from `~/.config/exhub/mcp_servers.json` (or `$EXHUB_MCP_SERVERS_CONFIG`). External configs take precedence if names collide.

2. **Zero-latency execution**: Built-in servers start with status `:connected` immediately — no HTTP handshake or tool discovery is needed. Tool calls are routed directly to the server module via `BuiltInRegistry.call_tool/3`.

3. **Tool aggregation**: Built-in server tools are included in both `list_all_tools` and `search_tools` responses, merged with upstream tools.

4. **Protection**: Built-in servers cannot be removed (`DELETE`) or toggled (`POST /toggle`) via the REST API. These operations return `{:error, :cannot_remove_builtin}` or `{:error, :cannot_toggle_builtin}`.

5. **Config persistence**: Built-in servers are excluded from the config file persistence — they are always regenerated from the registry at startup.

---

## Endpoints

### Unified MCP Endpoint

**`POST /mcp-hub/mcp`** — Streamable HTTP MCP endpoint exposing all tools from all connected servers.

Tool names follow the convention `{server_name}__{original_tool_name}`:

| Original Tool | Server              | Hub Exposed Name               | Description                              |
|---------------|---------------------|--------------------------------|------------------------------------------|
| `read_file`   | `desktop-commander` | `desktop-commander__read_file` | `[desktop-commander] Read file contents` |
| `web_search`  | `fetch-server`      | `fetch-server__web_search`     | `[fetch-server] Web search`              |

### Virtual Route Proxy

**`POST /:group/mcp`** — Direct proxy to a specific upstream server (no namespace prefix).

Only servers with `expose_route` configured are accessible:

| Server Config                | Virtual Route   | Tool Names                         |
|------------------------------|-----------------|------------------------------------|
| `"expose_route": "probe"`    | `/probe/mcp`    | Original names (e.g., `read_file`) |
| `"expose_route": "deepwiki"` | `/deepwiki/mcp` | Original names                     |

### REST Management API

| Method   | Endpoint                        | Description                  |
|----------|---------------------------------|------------------------------|
| `GET`    | `/mcp-hub/servers`              | List all servers with status |
| `POST`   | `/mcp-hub/servers`              | Add a new server             |
| `GET`    | `/mcp-hub/servers/:name`        | Get server status            |
| `PUT`    | `/mcp-hub/servers/:name`        | Update server config         |
| `DELETE` | `/mcp-hub/servers/:name`        | Remove a server              |
| `POST`   | `/mcp-hub/servers/:name/toggle` | Enable/disable a server      |
| `GET`    | `/mcp-hub/tools`                | List all aggregated tools    |

---

## REST API Reference

### List Servers

```
GET /mcp-hub/servers
```

**Response (200)**

```json
{
  "servers": [
    {
      "name": "probe",
      "transport": "stdio",
      "enabled": true,
      "status": "connected",
      "tool_count": 12,
      "last_error": null,
      "expose_route": "probe"
    }
  ]
}
```

### Add Server

```
POST /mcp-hub/servers
Content-Type: application/json

{
  "name": "my-server",
  "transport": "stdio",
  "command": "npx",
  "args": ["-y", "some-mcp-package"],
  "enabled": true,
  "expose_route": "my-server"
}
```

**Response (201)**

```json
{
  "success": true,
  "message": "Server added successfully",
  "server": {
    "name": "my-server",
    "transport": "stdio",
    "enabled": true,
    "expose_route": "my-server",
    "created_at": "2025-01-15T10:30:00Z",
    "updated_at": "2025-01-15T10:30:00Z"
  }
}
```

**Error Responses**

| Status | Error                        | Condition                       |
|--------|------------------------------|---------------------------------|
| 400    | `name_required`              | Missing server name             |
| 400    | `command_required_for_stdio` | stdio transport without command |
| 400    | `url_required`               | Remote transport without URL    |
| 409    | `server_already_exists`      | Duplicate server name           |

### Get Server Status

```
GET /mcp-hub/servers/:name
```

**Response (200)**

```json
{
  "server": {
    "name": "probe",
    "transport": "stdio",
    "enabled": true,
    "status": "connected",
    "tool_count": 12,
    "last_error": null,
    "expose_route": "probe"
  }
}
```

**Error (404)** — Server not found.

### Update Server

```
PUT /mcp-hub/servers/:name
Content-Type: application/json

{
  "args": ["-y", "updated-package@2.0"],
  "enabled": true
}
```

Merges the provided fields with the existing config. The server is restarted with the new configuration.

**Response (200)** — Same format as Add Server.

### Remove Server

```
DELETE /mcp-hub/servers/:name
```

Stops the client process and removes the configuration.

**Response (200)**

```json
{
  "success": true,
  "message": "Server removed successfully"
}
```

**Error (400)** — `cannot_remove_builtin`: Built-in servers cannot be removed.

### Toggle Server

```
POST /mcp-hub/servers/:name/toggle
Content-Type: application/json

{"enabled": false}
```

**Response (200)** — Same format as Add Server.

**Error (400)** — `cannot_toggle_builtin`: Built-in servers cannot be toggled.

### List All Tools

```
GET /mcp-hub/tools
```

**Response (200)**

```json
{
  "tools": [
    {
      "name": "probe__read_file",
      "description": "[probe] Read file contents",
      "inputSchema": { ... },
      "server": "probe"
    }
  ]
}
```

---

## Hub Meta-Tools

The Hub Server exposes two meta-tools as `Anubis.Server.Component` modules:

### `retrieve_tools` — Tool Search & Discovery

Search for relevant tools across all connected servers (both upstream and built-in) using TF-IDF scoring. Instead of returning all tools (which can overwhelm clients), the search returns only the most relevant tools for a given natural language query.

**Module**: `Exhub.MCP.Tools.Hub.RetrieveTools`

**Parameters**:

| Parameter | Type    | Required | Default | Description                          |
|-----------|---------|----------|---------|--------------------------------------|
| `query`   | string  | yes      | —       | Natural language search query        |
| `limit`   | integer | no       | 5       | Maximum number of results to return  |

**Example**:

```json
{
  "method": "tools/call",
  "params": {
    "name": "retrieve_tools",
    "arguments": {
      "query": "read file contents",
      "limit": 3
    }
  }
}
```

**Response:**

```json
{
  "tools": [
    {
      "name": "desktop-commander__read_file",
      "description": "[desktop-commander] Read file contents",
      "server": "desktop-commander",
      "inputSchema": { ... }
    }
  ],
  "count": 1
}
```

### `call_tools` — Execute Tool on Upstream Server

Execute a tool on a specific upstream MCP server through the hub. Use this to call any tool from any connected server (upstream or built-in) by specifying the server name and tool name explicitly.

**Module**: `Exhub.MCP.Tools.Hub.CallTools`

**Parameters**:

| Parameter     | Type    | Required | Default | Description                                    |
|---------------|---------|----------|---------|------------------------------------------------|
| `server_name` | string  | yes      | —       | Name of the upstream server to call the tool on|
| `tool_name`   | string  | yes      | —       | Name of the tool to execute (without prefix)   |
| `arguments`   | map     | no       | `{}`    | Arguments to pass to the tool                  |

**Example**:

```json
{
  "method": "tools/call",
  "params": {
    "name": "call_tools",
    "arguments": {
      "server_name": "desktop",
      "tool_name": "read_file",
      "arguments": {"path": "/tmp/test.txt"}
    }
  }
}
```

**Response:**

```json
{
  "result": { ... }
}
```

**Note**: If the initial tool call fails and the `tool_name` contains a `__` prefix (e.g., `desktop__read_file`), the tool will automatically retry with the prefix stripped.

---

## Search Endpoint (HTTP)

The MCP Hub also exposes tool search via an HTTP endpoint:

```
GET /mcp-hub/tools/search?query=<query>&limit=<limit>
```

| Parameter | Type    | Required | Default | Description                          |
|-----------|---------|----------|---------|--------------------------------------|
| `query`   | string  | yes      | —       | Natural language search query        |
| `limit`   | integer | no       | 5       | Maximum number of results to return  |

**Response (200)**

```json
{
  "tools": [
    {
      "name": "desktop-commander__read_file",
      "description": "[desktop-commander] Read file contents",
      "server": "desktop-commander",
      "inputSchema": { ... }
    }
  ],
  "query": "read file"
}
```

### How Search Works

The search uses an in-memory **TF-IDF (Term Frequency-Inverse Document Frequency)** index stored in `Exhub.MCP.Hub.Store`:

1. **Tokenization**: Query and tool descriptions are tokenized (lowercased, punctuation removed, stop words filtered)
2. **Scoring**: Each tool is scored based on term frequency matches between the query and tool name/description
3. **Ranking**: Results are sorted by score and the top-k are returned

The index is rebuilt automatically when:
- A new upstream server connects (via `ClientManager.rebuild_search_index/1`)
- The Hub Server initializes (in `init/2`)
- The `retrieve_tools` component finds no index and falls back to rebuilding

---

## Health Monitoring & Auto-Reconnect

The MCP Hub includes built-in health monitoring and automatic reconnection for upstream servers.

### Health Check Behavior

- **Periodic checks**: Every 30 seconds, all connected clients are checked
- **Health status**: Each client tracks `:healthy`, `:degraded`, or `:unhealthy` status
- **Auto-reconnect**: Clients in `:error` state are automatically reconnected with exponential backoff

### Reconnect Strategy

| Attempt | Delay  |
|---------|--------|
| 1st     | 5s     |
| 2nd     | 10s    |
| 3rd     | 20s    |
| 4th+    | 40s    |

- Maximum 3 reconnect attempts before requiring manual intervention
- Reconnects are staggered to avoid thundering herd
- Health status is exposed in the server list API

### Health Fields in Server Response

```json
{
  "servers": [
    {
      "name": "probe",
      "status": "connected",
      "health_status": "healthy",
      "tool_count": 12,
      "last_error": null,
      "connected_at": "2025-01-15T10:30:00Z",
      "last_health_check": "2025-01-15T10:35:00Z",
      "reconnect_attempts": 0
    }
  ]
}
```

---

## Startup Behavior

The ClientManager uses a **non-blocking startup** pattern to avoid delaying the Exhub application supervisor tree:

1. **`init/1`** — Loads config from `~/.config/exhub/mcp_servers.json` (or `$EXHUB_MCP_SERVERS_CONFIG`), merges with built-in server configs (from `BuiltInRegistry`), creates the `DynamicSupervisor`, and marks all enabled servers as `:connecting` (external) or `:connected` (built-in). Returns immediately.
2. **`handle_continue(:start_clients)`** — Spawns a `Task` for each enabled **external** server under `Exhub.MCP.Hub.TaskSupervisor`. Built-in servers are skipped since they run in the same BEAM VM. Each task:
   - Starts the `Anubis.Client` under the `DynamicSupervisor`
   - Performs the MCP handshake (initialize)
   - Discovers available tools (with retry, up to 5 attempts with 1s backoff)
   - Reports the result back to the `ClientManager`
   - On success, triggers `rebuild_search_index/1` to update the TF-IDF index
3. Tasks complete independently — one server failing does not block others.

### Reconnection

- If the `DynamicSupervisor` crashes, it is recreated and a reconnect is scheduled after 30 seconds.
- Failed/error clients are periodically reconnected via `:reconnect_failed_clients` messages.
- Upstream server failures **do not crash** the main Exhub application.

---

## Client Lifecycle States

| State           | Description                                                     |
|-----------------|-----------------------------------------------------------------|
| `:connecting`   | Client task is running (handshake + tool discovery in progress) |
| `:connected`    | Client is alive, handshake complete, tools discovered           |
| `:disconnected` | Client is not running (disabled or not yet started)             |
| `:error`        | Client failed to start or crashed (see `last_error`)            |

---

## Registry Keys

Upstream clients are registered in `Exhub.Registry`:

| Key Pattern                         | Purpose                            |
|-------------------------------------|------------------------------------|
| `{:mcp_hub_client, server_name}`    | Anubis.Client process registration |
| `{:mcp_hub_transport, server_name}` | Transport process registration     |

---

## Connecting an MCP Client

### Unified Endpoint (all tools)

Configure your MCP client to connect to:

```
http://localhost:9069/mcp-hub/mcp
```

Tools will appear with the `{server}__{tool}` naming convention.

### Direct Virtual Route (single server)

If a server has `expose_route` configured, connect directly:

```
http://localhost:9069/probe/mcp
```

Tools appear with their original names (no prefix).

---

## Configuration Persistence

All configuration changes (add, update, remove, toggle) are automatically persisted to `~/.config/exhub/mcp_servers.json` (or the path given by `EXHUB_MCP_SERVERS_CONFIG`). The file is rewritten on every modification, so manual edits to the file are picked up on the next application restart.

Built-in servers are **excluded** from persistence — they are always regenerated from `BuiltInRegistry` at startup. External configs that override a built-in server name take precedence.

---

## Tool Filtering

All MCP servers (including the Hub and individual upstream servers) support filtering the `tools/list` response via HTTP headers. This allows clients to include only specific tools or exclude unwanted ones.

### Headers

| Header            | Description                                                                       | Example                       |
|-------------------|-----------------------------------------------------------------------------------|-------------------------------|
| `x-include-tools` | Comma-separated list of tool names to include. Only these tools will be returned. | `read_file,write_file`        |
| `x-exclude-tools` | Comma-separated list of tool names to exclude. Applied after the include filter.  | `delete_file,execute_command` |

### Behavior

1. If `x-include-tools` is provided, only tools whose names appear in the list are returned.
2. If `x-exclude-tools` is provided, any tools whose names appear in the list are removed from the result.
3. Both headers can be used together — exclusion is applied after inclusion.
4. Tool names are trimmed but are **case-sensitive**.
5. Unknown tool names in either header are silently ignored.

### Examples

**Include only specific tools:**
```
POST /desktop/mcp
x-include-tools: read_file,write_file,list_directory

{"method": "tools/list"}
```

**Exclude sensitive tools:**
```
POST /desktop/mcp
x-exclude-tools: execute_command,start_process,kill_process

{"method": "tools/list"}
```

**Combined filtering (Hub):**
```
POST /mcp-hub/mcp
x-include-tools: desktop-commander__read_file,desktop-commander__write_file
x-exclude-tools: desktop-commander__delete_file

{"method": "tools/list"}
```

### Implementation

Tool filtering is implemented in `Exhub.MCP.ServerHelpers` and applied automatically to all MCP servers via `handle_request_with_filtered_tools/3`. The filtering wraps the default Anubis `tools/list` handler, so no server-specific changes are required beyond including the helper.

---

## Security Considerations

- Upstream server failures are isolated — a crashed upstream client does not affect the Hub or other servers.
- Built-in servers run in-process and cannot be removed or toggled via the REST API.
- The `DynamicSupervisor` uses `max_restarts: 100` in 60 seconds to handle flaky connections.
- Virtual route proxy sessions are tracked in `Exhub.MCP.Hub.Store` (`:mcp_hub_proxy_sessions` ETS table) with `mcp-session-id` headers.
- ETS tables are owned by `Exhub.MCP.Hub.Store` GenServer, ensuring proper cleanup on shutdown.
- Environment variables in server configs may contain secrets — the config file should be protected accordingly.
- **Tool filtering headers** (`x-include-tools`, `x-exclude-tools`) are evaluated server-side and do not bypass any underlying tool permissions. They only affect the `tools/list` discovery response.
