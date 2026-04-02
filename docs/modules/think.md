# exhub-think

The `exhub-think` module provides MCP-based reasoning scratchpad tools for LLMs.

## Setup

The think server is built into the Exhub application and exposes an MCP endpoint at `/think/mcp`.

## Features

- **Think Tool**: Allows the LLM to record thoughts for complex reasoning without obtaining new information or changing state
- **Plan Tool**: Allows the LLM to plan steps for multi-step tasks without side effects
- **MCP Tools**: Two MCP tools are available:
  - `think`: Append a thought to the reasoning log
  - `plan`: Append a plan of next steps to the reasoning log

## Configuration

No additional configuration is required. The server starts automatically with the Exhub application.

## MCP Endpoint

The think server is accessible at:

```
POST /think/mcp
```

This endpoint accepts MCP protocol messages for tool invocations.

## Tool Parameters

### think

- `thought` (required): A thought to think about.

### plan

- `plan` (required): A plan of next steps.
