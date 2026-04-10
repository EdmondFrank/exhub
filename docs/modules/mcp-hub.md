# MCP Hub

## Overview

The MCP Hub is a unified gateway that aggregates multiple upstream MCP (Model Context Protocol) servers into a single endpoint. It allows MCP clients to discover and invoke tools from all connected upstream servers through one connection, with automatic tool name namespacing to avoid conflicts.

The Hub also provides **virtual route proxying** — each upstream server can optionally be exposed at its own direct endpoint (e.g., `/probe/mcp`), bypassing the namespace prefix for clients that need a 1:1 mapping.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                      MCP Client (Claude Code, Zed, etc.)                    │
└──────────────┬──────────────────────────────────────────────┬───────────────┘
               │ HTTP POST /mcp-hub/mcp                       │ HTTP POST /probe/mcp
               │ (unified — all tools)                        │ (virtual — direct proxy)
               ▼                                              ▼
┌──────────────────────────┐              ┌────────────────────────────────────┐
│  Exhub.MCP.Hub.Server    │              │  Exhub.MCP.Hub.ProxyPlug           │
│  (Anubis.Server)         │              │  (Plug — streamable HTTP proxy)    │
│  Tool: server__tool      │              │  Tool: tool (no prefix)            │
└──────────────┬───────────┘              └──────────────┬─────────────────────┘
               │                                         │
               ▼                                         ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    Exhub.MCP.Hub.ClientManager (GenServer)                  │
│  ┌──────────────────┐  ┌──────────────────┐  ┌──────────────────────────┐   │
│  │ DynamicSupervisor│  │  Config Store    │  │  Tool Aggregation        │   │
│  │  (Anubis.Client) │  │  (JSON file)     │  │  (namespaced listing)    │   │
│  └──────────────────┘  └──────────────────┘  └──────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────────────┘
               │              │              │
               ▼              ▼              ▼
        ┌──────────┐  ┌──────────┐  ┌──────────────┐
        │  probe   │  │  skills  │  │  deepwiki    │  ... (any MCP server)
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
| `Exhub.Controllers.MCPHubController` | REST API controller for management operations                                      |

### Application Supervision

```
Exhub.Supervisor
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

Servers are configured in `priv/mcp_servers.json`:

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

### Toggle Server

```
POST /mcp-hub/servers/:name/toggle
Content-Type: application/json

{"enabled": false}
```

**Response (200)** — Same format as Add Server.

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

## Startup Behavior

The ClientManager uses a **non-blocking startup** pattern to avoid delaying the Exhub application supervisor tree:

1. **`init/1`** — Loads config from `priv/mcp_servers.json`, creates the `DynamicSupervisor`, and marks all enabled servers as `:connecting`. Returns immediately.
2. **`handle_continue(:start_clients)`** — Spawns a `Task` for each enabled server under `Exhub.MCP.Hub.TaskSupervisor`. Each task:
   - Starts the `Anubis.Client` under the `DynamicSupervisor`
   - Performs the MCP handshake (initialize)
   - Discovers available tools (with retry, up to 5 attempts with 1s backoff)
   - Reports the result back to the `ClientManager`
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

All configuration changes (add, update, remove, toggle) are automatically persisted to `priv/mcp_servers.json`. The file is rewritten on every modification, so manual edits to the file are picked up on the next application restart.

---

## Security Considerations

- Upstream server failures are isolated — a crashed upstream client does not affect the Hub or other servers.
- The `DynamicSupervisor` uses `max_restarts: 100` in 60 seconds to handle flaky connections.
- Virtual route proxy sessions are tracked in an ETS table (`:mcp_hub_proxy_sessions`) with `mcp-session-id` headers.
- Environment variables in server configs may contain secrets — the config file should be protected accordingly.
