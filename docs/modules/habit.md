# exhub-habit

The `exhub-habit` module provides MCP-based user habit and environment configuration storage with protected keys.

## Setup

The habit server is built into the Exhub application and exposes an MCP endpoint at `/mcp`.

## Features

- **Persistent Storage**: Habits are stored in `~/.config/exhub/habits.json`
- **Protected Keys**: Sensitive keys (user_id, email, api_key, password, secret, token) are protected by default
- **Metadata Tracking**: Each habit includes modifiable status, description, category, and timestamps
- **MCP Tools**: Two MCP tools are available:
  - `read_habits`: Query habits by key, category, or retrieve all
  - `update_habits`: Create, update, or delete modifiable habits

## Configuration

The habit server starts automatically with the Exhub application. No additional configuration is required.

## MCP Endpoint

The habit server is accessible at:

```
POST /mcp
```

This endpoint accepts MCP protocol messages for tool invocations.

## Default Habits

On first startup, the following default habit is initialized:

```json
{
  "environment.timezone": {
    "value": "CST",
    "description": "User's preferred timezone",
    "category": "environment",
    "modifiable": true
  }
}
```
