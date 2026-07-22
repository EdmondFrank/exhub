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
- `render_js` (optional): Render JavaScript via headless Chrome before extracting text (default: false)

#### render_js mode

When `render_js` is `true`, the tool uses the Kuri browser daemon to load the page in a real Chrome instance, wait for JavaScript to finish rendering, and then extract the visible text. This is useful for SPAs and JavaScript-heavy pages that return empty or loading-indicator content with a plain HTTP fetch.

**Behavior:**
- Opens a new tab via Kuri's HTTP API (`/tab/new`)
- Polls the `/text` endpoint every 1 second until meaningful content appears (non-trivial text, not a loading indicator)
- Times out after 10 seconds if no meaningful content is detected
- Closes the tab automatically after extraction (best-effort)
- Falls back to plain HTTP fetch if Kuri is unavailable or unhealthy

**Authentication:** Bearer token resolved from `KURI_API_TOKEN` env → `KURI_SECRET` env → `~/.kuri/api.token` file (via `Exhub.KuriDaemon.api_token/0`).

**Prerequisites:** Kuri daemon must be running (auto-managed by `Exhub.KuriDaemon`).
