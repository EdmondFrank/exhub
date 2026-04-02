# exhub-web-tools

The `exhub-web-tools` module provides MCP-based web search and content fetching capabilities.

## Setup

The web tools server is built into the Exhub application and exposes an MCP endpoint at `/web-tools/mcp`.

## Features

- **Web Search**: Search the web using Gitee AI's web search API
- **Web Fetch**: Fetch and parse content from URLs or local files
- **MCP Tools**: Two MCP tools are available:
  - `web_search`: Search the web with query, count, summary, and freshness options
  - `web_fetch`: Fetch content from URLs (http/https) or local files (file://)

## Configuration

The web tools server requires a Gitee AI API key:

```bash
mix scr.insert dev giteeai_api_key "your-gitee-ai-api-key"
```

The server starts automatically with the Exhub application.

## MCP Endpoint

The web tools server is accessible at:

```
POST /web-tools/mcp
```

This endpoint accepts MCP protocol messages for tool invocations.

## Tool Parameters

### web_search

- `query` (required): The search query string
- `count` (optional): Number of results to return (1-50, default: 10)
- `summary` (optional): Enable AI-generated summary (default: false)
- `freshness` (optional): Filter by freshness — `noLimit`, `oneDay`, `oneWeek`, `oneMonth`, `oneYear`

### web_fetch

- `url` (required): The URL to fetch (http/https) or file path (file://)
- `method` (optional): HTTP method — GET, POST, HEAD (default: GET)
- `headers` (optional): HTTP headers as key-value pairs
- `body` (optional): Request body for POST requests
