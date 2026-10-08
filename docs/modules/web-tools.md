# exhub-web-tools

The `exhub-web-tools` module provides MCP-based web search and content fetching capabilities.

## Setup

The web tools server is built into the Exhub application and exposes an MCP endpoint at `/web-tools/mcp`.

## Features

- **Web Search**: Search the web using Gitee AI's web search API
- **Web Fetch**: Fetch and parse content from URLs or local files
- **Smart-decided egress**: `web_fetch` proxies only when Smart Decide judges a
  blocked route — see *Proxy decision (Smart Decide)* below
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
| `state_char_limit` | `18000` | Result text truncation, to stay within the model's context |
| `query_char_limit` | `3200` | Query truncation in the question |
| `fallback` | `true` | Return the raw results when nothing is judged relevant |

### web_fetch

- `url` (required): The URL to fetch (http/https) or file path (file://)
- `method` (optional): HTTP method — GET, POST, HEAD (default: GET)
- `headers` (optional): HTTP headers as key-value pairs
- `body` (optional): Request body for POST requests
- `render_js` (optional): Render JavaScript via headless Chrome before extracting text (default: false)

#### Proxy decision (Smart Decide)

`web_fetch` does not attach the configured `:exhub, :proxy` to every request. It
starts **direct**, and only a failure that looks like a blocked route is
escalated to one Smart Decide call, which may approve exactly one retry through
a proxy it judged reachable and worth the leak risk. This mirrors the Desktop
shell tools: `Exhub.MCP.Desktop.ProxyEnv` does the judging and owns the verdict
cache, so a retry already decided for the same URL is reused without a second
model call. Layered and fail-closed:

1. **Transport gate** — `Exhub.MCP.WebTools.Proxy.transport_failure/1` maps
   hackney/HTTPoison errors onto the blocked-route vocabulary (`:timeout` →
   `connection timed out`, `{:failed_connect, [... :econnrefused]}` →
   `connection refused`, `{:tls_alert, {:handshake_failure, _}}` → `unable to
   establish ssl connection`, …). A 4xx/5xx status, a rejected certificate or a
   parse error is **not** a transport failure, so it never asks for a proxy.
2. **Hard guards, before any API call** — a credential in the URL
   (`https://user:pass@…`, `?token=…`) stops the decision outright; a loopback or
   `NO_PROXY` target short-circuits to `{:noop, :bypassed, …}` on every path, the
   judged one included (unlike the shell tools, where that list is advisory
   evidence — here the only way to honour it is to not set `proxy:`); a candidate
   that does not TCP-connect is never used. `judge/4` accepts
   `respect_bypass: false` to put an exempt host in front of the model anyway.
3. **Smart Decide** — the `needs_proxy` / `mechanism` / `leak_risk` questions in
   one call, with the request rendered as `curl -fsSL -X GET <url>` evidence and
   the measured direct probe of the target host (see
   [`desktop.md`](desktop.md) → *Proxy-environment gate*).
4. **Fail closed** — an abstention, a timeout or a model error leaves the request
   exactly as it was, and the returned error names the verdict it reached.

`mechanism == direct_no_proxy` is the one verdict that can *remove* a proxy: if
the first attempt was already proxied (`:pre` mode, a cached verdict, or the
legacy static proxy), that advice triggers the single direct retry.

When a proxy was used, the structured response carries the decision:

```json
{"success": true, "url": "…", "status_code": 200, "content": "…",
 "proxy": {"proxy_url": "http://127.0.0.1:7890", "decision": "smart_decide",
           "needs_proxy": 0.9998, "mechanism": "proxy_env",
           "leak_risk": 0.0, "attempt": 2}}
```

Configuration (`config :exhub, Exhub.MCP.WebTools.Proxy`; mode, thresholds,
probe budgets, network premise/notes and the cache all come from
`Exhub.MCP.Desktop.ProxyEnv`):

| Key | Default | Meaning |
|-----|---------|---------|
| `enabled` | `true` | Master switch. `false` restores the legacy behaviour: the static `:exhub, :proxy` on every request, with no model call and no direct fallback. |
| `shared_proxy_candidate` | `true` | Offer `:exhub, :proxy` (the egress proxy the router's LLM routes use, which `ProxyEnv` does not read itself) as the first candidate to the decision. |

A proxy candidate must be `http://` (or socks). hackney 1.23.0 cannot CONNECT an
`https://` target through an `https://` proxy — it returns
`:invalid_proxy_transport` — so keep `:exhub, :proxy`, `ProxyEnv`'s `proxy_url`
and its `fallback_proxies` as `http://…`/socks URLs.

Tests stay offline because `ProxyEnv` is disabled in `config/test.exs`: with the
decision engine off, requests go direct and `judge/4` only ever abstains. The
gate itself is exercised with an injected decider/probe in
`test/exhub/mcp/web_tools/proxy_test.exs`.

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
