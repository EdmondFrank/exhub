# exhub-todo — MCP Multi-Tenant Todo List Management

## Overview

`Exhub.MCP.TodoServer` is an MCP server that provides a **multi-tenant, in-memory todo list** service over HTTP. It is designed for use by LLM agents and AI assistants that need a lightweight, task-scoped progress tracker — without requiring any external database.

Each tenant's list is scoped by a `tenant_id` — a short, stable string chosen by the caller (e.g. a conversation ID, username, or a brief task slug like `"refactor-auth"`). The same `tenant_id` must be used consistently across all tool calls that belong to the same task. Todo lists that have not been updated for more than **2 hours** are automatically purged by the backing `Exhub.MCP.TodoStore` GenServer.

**HTTP Endpoint:** `POST /todo/mcp`

---

## Typical Workflow

The intended usage pattern for LLM agents:

1. **Start of task** — call `set_items` to record the full plan and the user's original request.
2. **After each step** — call `update_item_completion` to mark the finished item as `completed: true`.
3. **Resuming work** — call `get_items` to reload the current state before continuing.
4. **Task finished** — call `clear_items` to clean up, or let the list expire automatically after 2 hours of inactivity.

---

## About `tenant_id`

Every tool requires a `tenant_id` that scopes the todo list to a specific conversation or task. Rules:

- **Use a short, stable string** — e.g. a conversation ID, the user's name, or a task slug like `"refactor-auth"` or `"fix-issue-42"`.
- **Keep it consistent** — all tool calls for the same task must use the exact same `tenant_id` (case-sensitive).
- **Lists are fully isolated** — different `tenant_id` values never share data.
- **Auto-expiry** — lists that have not been updated for more than 2 hours are silently removed.

---

## Architecture

```
Exhub.MCP.TodoServer          ← Anubis MCP server (streamable HTTP)
  ├── TodoSetItems             ← set_items tool
  ├── TodoGetItems             ← get_items tool
  ├── TodoUpdateItem           ← update_item_completion tool
  └── TodoClearItems           ← clear_items tool

Exhub.MCP.TodoStore           ← GenServer backed by ETS (:todo_store table)
  ├── Periodic cleanup every 30 minutes
  └── Expires entries older than 2 hours
```

---

## MCP Tools

### `set_items`

Create or replace the todo list for a task.

Call this at the **start** of a multi-step task to record the full plan and the user's original request. Each item represents one step or sub-task.

> **IMPORTANT:** This call **replaces** any existing list for the given `tenant_id`. Do not call it mid-task unless you intend to rewrite the entire plan. To update a single item's status, use `update_item_completion` instead.

**Parameters:**

| Parameter             | Type   | Required | Description                                                                                                                                                                                                           |
|-----------------------|--------|----------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `tenant_id`           | string | ✅       | A short, stable string scoping this list to a specific task or conversation (e.g. a conversation ID, username, or task slug like `"refactor-auth"`). Must stay the same across all todo tool calls for the same task. |
| `items`               | array  | —        | The ordered list of steps/tasks. Each item needs a `name` (required) and an optional `completed` flag (defaults to `false`).                                                                                          |
| `initial_user_prompt` | string | —        | The user's original request, copied verbatim. Stored for context and returned by `get_items`.                                                                                                                         |

**Response:**
```json
{
  "success": true,
  "tenant_id": "session-abc",
  "count": 3,
  "message": "Todo list set successfully."
}
```

---

### `get_items`

Read the current todo list for a task.

Call this to reload your plan and check progress — for example when resuming a task, before deciding what to do next, or to verify the current state of the list. Returns all items with their `completed` status, the saved `initial_user_prompt`, and the time of the last update.

If no list exists for the given `tenant_id` (not yet created, or expired after 2 hours of inactivity), an **empty list is returned — not an error**.

**Parameters:**

| Parameter   | Type   | Required | Description |
|-------------|--------|----------|-------------|
| `tenant_id` | string | ✅        | The same stable string used when the list was created with `set_items`. Must match exactly (case-sensitive). |

**Response:**
```json
{
  "tenant_id": "session-abc",
  "initial_user_prompt": "Build a REST API",
  "items": [
    { "name": "Design schema", "completed": true },
    { "name": "Write controllers", "completed": false }
  ],
  "count": 2,
  "updated_at": "2026-04-03T14:45:00Z"
}
```

---

### `update_item_completion`

Mark a single todo item as completed or not completed.

Call this **immediately after finishing a step** to keep the list up to date. The item is looked up by its exact `name` — the value must match what was passed to `set_items` character-for-character.

Requires an existing list — call `set_items` first if none exists yet. Returns the **full updated list** so you can see the current state at a glance.

**Parameters:**

| Parameter   | Type    | Required | Description                                                                                                  |
|-------------|---------|----------|--------------------------------------------------------------------------------------------------------------|
| `tenant_id` | string  | ✅       | The same stable string used when the list was created with `set_items`. Must match exactly (case-sensitive). |
| `name`      | string  | ✅       | The exact name of the item to update, character-for-character as it was given to `set_items`.                |
| `completed` | boolean | ✅       | `true` to mark the item done; `false` to reopen it.                                                          |

**Response:**
```json
{
  "success": true,
  "tenant_id": "session-abc",
  "updated_item": "Design schema",
  "completed": true,
  "items": [...],
  "count": 2
}
```

---

### `clear_items`

Remove all todo items from a task's list.

Call this when a task is **fully complete** and you no longer need the list, or when you want to **start over** with a fresh plan (follow up with `set_items` to create a new one).

The list entry itself is kept in the store (with an empty items array), so a subsequent `get_items` call returns an empty list rather than a "not found" response. The entry will be fully removed after 2 hours of inactivity regardless.

**Parameters:**

| Parameter   | Type   | Required | Description                                                                                                  |
|-------------|--------|----------|--------------------------------------------------------------------------------------------------------------|
| `tenant_id` | string | ✅       | The same stable string used when the list was created with `set_items`. Must match exactly (case-sensitive). |

**Response:**
```json
{
  "success": true,
  "tenant_id": "session-abc",
  "message": "All todo items cleared successfully."
}
```

---

## Data Model

Each tenant entry in the ETS store has the following shape:

```elixir
%{
  items: [%{name: String.t(), completed: boolean()}],
  initial_user_prompt: String.t(),
  updated_at: DateTime.t()
}
```

Items are normalised on write — both atom-keyed and string-keyed maps are accepted, and missing `completed` fields default to `false`.

---

## Lifecycle & Expiry

- The `TodoStore` GenServer starts automatically with the application.
- A cleanup task runs **every 30 minutes**, removing any tenant entries whose `updated_at` timestamp is older than **2 hours**.
- Expired entries are logged at `debug` level; the count of cleaned-up entries is logged at `info` level.

---

## Usage Example (MCP Client)

```json
// 1. Create a todo list
{
  "tool": "set_items",
  "params": {
    "tenant_id": "user-42",
    "initial_user_prompt": "Implement the login feature",
    "items": [
      { "name": "Add login route", "completed": false },
      { "name": "Write auth middleware", "completed": false },
      { "name": "Add tests", "completed": false }
    ]
  }
}

// 2. Mark an item done
{
  "tool": "update_item_completion",
  "params": {
    "tenant_id": "user-42",
    "name": "Add login route",
    "completed": true
  }
}

// 3. Retrieve the list
{
  "tool": "get_items",
  "params": { "tenant_id": "user-42" }
}

// 4. Clear all items when done
{
  "tool": "clear_items",
  "params": { "tenant_id": "user-42" }
}
```

---

## Configuration

The Todo MCP server requires no additional configuration — it uses an in-memory ETS table and starts automatically. No API keys or external services are needed.

To adjust expiry or cleanup intervals, edit the module-level constants in `lib/exhub/mcp/todo_store.ex`:

```elixir
@cleanup_interval_ms 30 * 60 * 1_000   # run cleanup every 30 minutes
@expiry_seconds      2 * 60 * 60        # expire entries after 2 hours
```

---

## Source Files

| File                                      | Description                                   |
|-------------------------------------------|-----------------------------------------------|
| `lib/exhub/mcp/todo_server.ex`            | MCP server definition (Anubis)                |
| `lib/exhub/mcp/todo_store.ex`             | GenServer + ETS-backed store with TTL cleanup |
| `lib/exhub/mcp/tools/todo_set_items.ex`   | `set_items` tool implementation               |
| `lib/exhub/mcp/tools/todo_get_items.ex`   | `get_items` tool implementation               |
| `lib/exhub/mcp/tools/todo_update_item.ex` | `update_item_completion` tool implementation  |
| `lib/exhub/mcp/tools/todo_clear_items.ex` | `clear_items` tool implementation             |
