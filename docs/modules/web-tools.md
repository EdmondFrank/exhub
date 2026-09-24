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
- `filter` (optional): Smart Decide relevance filtering (default: true); `false` returns the raw search results

### Relevance filtering

After the web page results are retrieved, `web_search` applies a second, sharper
pass powered by the Smart Decide (System One) `noul` model: each candidate page
is judged with a single yes/no question (`Result: <title>` plus URL, snippet and
summary) and only the relevant pages are kept. Set `filter: false` to skip it and
get the raw search results.

Filtering widens the API pool to `candidate_limit` first (`count` is raised to
`max(count, candidate_limit)`, capped at 50), then narrows the judged set back
down to `count`, so the tool still returns at most `count` pages. When the cut
discards relevant results the summary line says so, e.g.
`Smart Decide relevance filter judged 20/20 result(s) relevant; returning 5.`

The pass is fault-tolerant:

- a per-result failure is treated as **relevant** (fail-open, preserving recall)
  and counted in `:errors`;
- a blank query or an empty candidate list skips the pass entirely;
- if **no** page is judged relevant the raw results are returned
  (`fallback: true`), so callers still receive the best-ranked guesses.

Images and videos are not filtered.

Configuration (in-code defaults in `Exhub.MCP.WebTools.Relevance`, overridable
under `config :exhub, Exhub.MCP.WebTools.Relevance` in `config/config.exs`):

| Key | Default | Meaning |
|-----|---------|---------|
| `enabled` | `true` | Master switch (`web_search.filter` overrides per call) |
| `candidate_limit` | `20` | API pool judged when filtering (always ≥ `count`) |
| `max_concurrency` | `8` | Concurrent System One requests (one result per request) |
| `threshold` | `0.5` | Minimum `noul` probability to keep a result |
| `timeout` | `30_000` | Per-request timeout in ms |
| `state_char_limit` | `18000` | Result text truncation, to stay within the ~8k context |
| `query_char_limit` | `3200` | Query truncation in the question |
| `fallback` | `true` | Return the raw results when nothing is judged relevant |

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
