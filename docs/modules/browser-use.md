# exhub-browser-use

The `exhub-browser-use` module provides MCP-based browser automation by wrapping
[kuri-agent](https://github.com/justrach/kuri) — an agentic Chrome CLI that
drives Chrome via the Chrome DevTools Protocol (CDP).

## Prerequisites

### 1. Install kuri-agent

`kuri-agent` is a native binary built with Zig. You must build or install it and
ensure it is available on your `PATH` **before** starting the Exhub server.

**Build from source:**

```bash
git clone https://github.com/justrach/kuri
cd kuri
zig build agent -Doptimize=ReleaseFast
# Binary is at ./zig-out/bin/kuri-agent
```

**Install to PATH (choose one):**

```bash
# Option A — copy to a directory already on PATH
cp ./zig-out/bin/kuri-agent /usr/local/bin/kuri-agent

# Option B — add the build output directory to PATH in your shell profile
echo 'export PATH="$HOME/kuri-agent/zig-out/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

**Verify:**

```bash
which kuri-agent        # must print a path
kuri-agent --help       # must print usage
```

> ⚠️ If `kuri-agent` is not on `PATH`, all `browser_*` MCP tools will fail with
> a "Failed to run kuri-agent" error.

### 2. Chrome is auto-managed by KuriDaemon

Chrome is now **automatically started and managed** by `KuriDaemon` — no manual
Chrome startup is required. The daemon:

- Auto-detects the `kuri` binary (from PATH, `~/Code/kuri/zig-out/bin/kuri`, or `:kuri_binary` config)
- Starts Chrome in headless mode on port 18080 (configurable via `:kuri_port`)
- **Deep health checks**: Verifies both HTTP liveness (`/health`) and Chrome/CDP functionality (`/tabs` with Bearer auth). Detects "zombie" state where kuri is up but Chrome is dead
- **`:degraded` status**: Reported when HTTP is reachable but Chrome/CDP is unresponsive
- **Auto-recovery**: After 3 consecutive failed deep checks, kills and restarts kuri automatically
- **Exponential backoff**: Restart delay grows 5s → 10s → 20s → 40s (cap 60s), resets on successful startup
- **Proper process management**: Tracks the OS pid via `lsof` for clean SIGTERM; falls back to killing the Elixir spawn pid
- **API token resolution**: `Exhub.KuriDaemon.api_token/0` resolves the Bearer token from `KURI_API_TOKEN` env → `KURI_SECRET` env → `~/.kuri/api.token` file

**Optional: Build kuri server for KuriDaemon:**

```bash
# From the kuri repo
zig build -Doptimize=ReleaseFast
cp ./zig-out/bin/kuri /usr/local/bin/kuri
```

**Configuration (application env):**

| Key              | Default       | Description                    |
|------------------|---------------|--------------------------------|
| `:kuri_enabled`  | `true`        | Enable/disable the daemon      |
| `:kuri_port`     | `18080`       | HTTP listen port               |
| `:kuri_host`     | `"127.0.0.1"` | Bind address                  |
| `:kuri_headless` | `true`        | Run Chrome headless            |
| `:kuri_binary`   | `nil`         | Explicit path to `kuri` binary |

**Verify KuriDaemon is running:**

```bash
curl http://localhost:18080/health
```

You should see a healthy response from the kuri server.

## Features

- **Tab Management**: Discover open Chrome tabs and attach a session
- **Navigation**: Navigate to URLs, go back/forward, reload
- **Page Inspection**: A11y snapshots with element refs, text extraction, JavaScript evaluation, screenshots
- **Element Interaction**: Click, type, fill, select, hover, focus, scroll using `@eN` refs from snapshots
- **Security Testing**: Cookie enumeration, security header checks, full audits, localStorage/sessionStorage dump, JWT scanning, authenticated fetches, IDOR probing
- **Auth Headers**: Persist custom HTTP headers (e.g. `Authorization`) across all CDP connections

## MCP Endpoint

```
POST /browser-use/mcp
```

## Tools

### `browser_tabs` — Tab discovery & session management

Manage Chrome tabs and attach a session. Always call this first.

| Parameter | Type    | Required | Description                                            |
|-----------|---------|----------|--------------------------------------------------------|
| `command` | string  | ✓        | `tabs` \| `use` \| `status`                            |
| `ws_url`  | string  |          | WebSocket URL of the tab (required for `use`)          |
| `port`    | integer |          | Chrome DevTools port (default: 9222, used with `tabs`) |

**Typical first steps:**

```json
{ "command": "tabs" }
```
```json
{ "command": "use", "ws_url": "ws://127.0.0.1:9222/devtools/page/ABC..." }
```

---

### `browser_navigate` — Navigation

| Parameter | Type   | Required | Description                             |
|-----------|--------|----------|-----------------------------------------|
| `command` | string | ✓        | `go` \| `back` \| `forward` \| `reload` |
| `url`     | string |          | URL to navigate to (required for `go`)  |

---

### `browser_inspect` — Page inspection

| Parameter     | Type    | Required | Description                                |
|---------------|---------|----------|--------------------------------------------|
| `command`     | string  | ✓        | `snap` \| `text` \| `eval` \| `shot`       |
| `interactive` | boolean |          | (`snap`) Only interactive elements         |
| `text_mode`   | boolean |          | (`snap`) Plain-text output                 |
| `depth`       | integer |          | (`snap`) Limit a11y tree depth             |
| `selector`    | string  |          | (`text`) CSS selector to scope extraction  |
| `expression`  | string  |          | (`eval`) JavaScript expression to evaluate |
| `out`         | string  |          | (`shot`) Output file path for screenshot   |

> Always run `snap` before using `browser_interact` — it saves `@eN` element refs to the session.

---

### `browser_interact` — Element interaction

Requires a prior `browser_inspect` snap to populate `@eN` refs.

| Parameter | Type   | Required | Description                                                                   |
|-----------|--------|----------|-------------------------------------------------------------------------------|
| `command` | string | ✓        | `click` \| `type` \| `fill` \| `select` \| `hover` \| `focus` \| `scroll`     |
| `ref`     | string |          | Element ref from snap (e.g. `e3` or `@e3`). Required for all except `scroll`. |
| `value`   | string |          | Text/value to input. Required for `type`, `fill`, `select`.                   |

---

### `browser_security` — Security testing

| Parameter     | Type    | Required | Description                                                                   |
|---------------|---------|----------|-------------------------------------------------------------------------------|
| `command`     | string  | ✓        | `cookies` \| `headers` \| `audit` \| `storage` \| `jwt` \| `fetch` \| `probe` |
| `scope`       | string  |          | (`storage`) `local` \| `session` \| `all` (default: `all`)                    |
| `method`      | string  |          | (`fetch`) HTTP method (default: `GET`)                                        |
| `url`         | string  |          | (`fetch`) URL to request. (`probe`) URL template with `{id}` placeholder.     |
| `data`        | string  |          | (`fetch`) JSON body for POST requests                                         |
| `probe_start` | integer |          | (`probe`) Start of ID range (inclusive)                                       |
| `probe_end`   | integer |          | (`probe`) End of ID range (inclusive)                                         |

---

### `browser_auth_headers` — Persistent auth headers

Headers set here are applied via `Network.setExtraHTTPHeaders` on every subsequent CDP connection.

| Parameter | Type   | Required | Description                                                        |
|-----------|--------|----------|--------------------------------------------------------------------|
| `command` | string | ✓        | `set_header` \| `show_headers` \| `clear_headers`                  |
| `name`    | string |          | Header name (required for `set_header`). Example: `Authorization`  |
| `value`   | string |          | Header value (required for `set_header`). Example: `Bearer eyJ...` |

## Typical Workflow

```
1. browser_tabs      command=tabs
2. browser_tabs      command=use  ws_url=<ws from step 1>
3. browser_navigate  command=go   url=https://example.com
4. browser_inspect   command=snap interactive=true
5. browser_interact  command=click  ref=e2
6. browser_interact  command=type   ref=e3  value="hello world"
7. browser_inspect   command=shot
```

## Security Trajectory Example

```
1. browser_navigate      command=go      url=https://target.example.com
2. browser_auth_headers  command=set_header  name=Authorization  value="Bearer eyJ..."
3. browser_security      command=audit
4. browser_security      command=jwt
5. browser_security      command=probe   url=https://api.example.com/v2/items/{id}  probe_start=1  probe_end=20
```

## Response Format

All tools return a JSON object:

```json
{
  "command": "kuri-agent tabs",
  "stdout": "...",
  "stderr": "...",
  "exit_status": 0
}
```

`exit_status` is included for diagnostics. A non-zero value does not necessarily
indicate failure — `kuri-agent` may exit with status 1 on some successful commands.
