# Recent Enhancements

## Nanoxml Local Office File Extraction

- **New Module**: `Exhub.MCP.Desktop.Nanoxml` wraps the [nanoxml](https://github.com/justrach/nanoxml) CLI binary for fast, local Office Open XML text extraction — no API calls or network access required
- **Affected Tools**: `read_file` and `read_multiple_files` now route `.docx`, `.xlsx`, and `.pptx` files to nanoxml instead of the Gitee AI API
- **Extraction Behavior**:
  - `.docx` / `.pptx` → plain text via `nanoxml text`
  - `.xlsx` → all worksheets extracted as CSV via `nanoxml sheets` + `nanoxml csv`, joined with sheet-name headers
- **Unchanged**: `.pdf`, `.doc`, and image files (`.png`, `.jpg`, etc.) still use the Gitee AI PaddleOCR-VL-1.5 API
- **Prerequisite**: `nanoxml` binary must be on `PATH` (`cargo install nanoxml` or see [installation](https://github.com/justrach/nanoxml))
- **Shared Helper**: `Exhub.MCP.Tools.DocExtract.Client` gained `office_type?/1` to distinguish Office Open XML files from other document types
- **Files Changed**:
  - `lib/exhub/mcp/desktop/nanoxml.ex` — NEW: nanoxml CLI wrapper module
  - `lib/exhub/mcp/tools/doc_extract/client.ex` — Added `office_type?/1` + `@office_extensions`
  - `lib/exhub/mcp/tools/desktop/read_file.ex` — Office file routing to nanoxml
  - `lib/exhub/mcp/tools/desktop/read_multiple_files.ex` — Office file routing to nanoxml

---

## Agent Hub MCP Tools Temporarily Disabled

- **Status**: All Agent Hub MCP tools (`agent_hub_list`, `agent_hub_start`, `agent_hub_chat`, `agent_hub_status`, `agent_hub_reset`, `agent_hub_stop`) are temporarily commented out in `Exhub.Sagents.AgentHubServer`
- **Impact**: The HTTP REST chat API (`POST /agent-hub/agents/{name}/chat`) and Web UI remain fully operational; only the MCP endpoint (`/agent-hub/mcp`) is affected
- **Reason**: Temporary disablement during refactoring; tools will be re-enabled in a future commit

---

## Listen MCP Server (Audio Transcription via Gitee AI)

- **New MCP Server**: `Exhub.MCP.ListenServer` provides audio-to-text transcription over MCP at `/listen/mcp`
- **Gitee AI / moark.com Backend**: Calls the OpenAI-compatible `/v1/audio/transcriptions` endpoint at `https://ai.gitee.com/v1/audio/transcriptions`
- **Supported Models**:
  - `whisper-large-v3-turbo` (default) — OpenAI Whisper multilingual real-time recognition
  - `whisper-large-v3` — Whisper standard
- **Supported Audio Formats**: MP3, WAV, M4A, FLAC, OGG, OPUS, WebM, MP4, AAC, MPEG, OGA
- **MCP Tool**: `listen` — accepts a file path (absolute or `~` shorthand), optional `model` and `language` hint
- **Shared API key**: Reuses the existing `giteeai_api_key` SecretVault entry — no new secrets needed if already configured
- **Multipart Upload**: Uses manual multipart/form-data encoding matching the `DocExtract.Client` pattern (no external multipart library)
- **HTTP Endpoint**: Exposed at `/listen/mcp`
- **Full Docs**: [docs/modules/listen.md](docs/modules/listen.md)

---

## Mac-use MCP Server (Native macOS App Automation)

- **New MCP Server**: `Exhub.MCP.MacUseServer` provides Playwright/Puppeteer-style control over native macOS applications via the Accessibility API
- **Powered by axcli**: Uses the [axcli](https://github.com/andelf/axcli) Rust CLI (installed via `cargo install axcli`) which wraps Apple's Accessibility and ScreenCaptureKit frameworks
- **17 MCP Tools** across six categories:
  - **App Discovery (1)**: `list_apps` — list running macOS applications
  - **Inspection (2)**: `snapshot` (accessibility tree), `get_attribute` (element attribute values)
  - **Interaction (6)**: `click`, `dblclick`, `input`, `fill`, `press`, `hover`, `focus` — all background-safe by default (no focus steal, no cursor movement)
  - **Scrolling (2)**: `scroll_to` (scroll element into view), `scroll` (scroll within element)
  - **Screenshots (1)**: `screenshot` — occlusion-proof capture via ScreenCaptureKit with optional OCR
  - **Utility (2)**: `wait` (element polling or sleep), `activate` (bring app to foreground)
  - **Global Input (2)**: `mouse` (pos/move/click/scroll), `keyboard` (type/press)
- **CSS-like Locator Syntax**: Supports selectors like `.SearchButton`, `AXButton[title="Send"]`, `text~="partial"`, chaining with `>>`, pseudo-classes `:has-text()`, `:visible`, `:nth-child()`
- **Background-safe by default**: Uses `CGEventPostToPid` to deliver events directly to the target process — no focus steal, no real cursor movement
- **Exile Integration**: Uses `Exile.stream/2` for robust process execution with timeout and error handling
- **TOON Encoding**: All responses are TOON-encoded for 30–60% token reduction vs JSON
- **Prerequisites**: `axcli` binary on PATH, terminal app granted Accessibility + Screen Recording permissions
- **Supervisor Integration**: `Exhub.MCP.MacUseServer` registered in the application supervisor with streamable HTTP transport
- **HTTP Endpoint**: Exposed at `/mac-use/mcp`
- **Unit Tests**: 79 tests across 3 test files in `test/exhub/mcp/mac_use/`
- **Full Docs**: [docs/modules/mac-use.md](docs/modules/mac-use.md)

---

## MCP Hub — Built-in Server Integration & Modularization

- **New Feature**: MCP Hub now auto-registers all 14 built-in MCP servers for direct in-process tool execution, bypassing HTTP loopback

### Built-in Server Registry (`Exhub.MCP.Hub.BuiltInRegistry`)
- **Zero-latency execution**: Built-in servers (habit, time, think, web-tools, archery, browser-use, image-gen, doc-extract, look, listen, todo, desktop, agent, brain, exhub) are accessed directly via function calls — no HTTP handshake or Anubis.Client connection needed
- **Auto-registration**: Built-in configs are merged with external configs from `~/.config/exhub/mcp_servers.json` (or `$EXHUB_MCP_SERVERS_CONFIG`) at startup; external configs take precedence on name collision
- **Protection**: Built-in servers cannot be removed or toggled via the REST API (`:cannot_remove_builtin`, `:cannot_toggle_builtin`)
- **Config persistence**: Built-in servers are excluded from the config file — always regenerated from the registry

### Centralized ETS Store (`Exhub.MCP.Hub.Store`)
- **New GenServer**: Owns and manages ETS tables for the MCP Hub (`:mcp_hub_search_index`, `:mcp_hub_proxy_sessions`)
- **Startup ordering**: `Hub.Store` starts before `ClientManager` and `Hub.Server` in the supervisor tree
- **Replaces ad-hoc ETS creation**: `ProxyPlug` no longer creates its own ETS table; all table access goes through `Hub.Store`

### Modular Hub Components
- **`Exhub.MCP.Tools.Hub.RetrieveTools`**: Extracted from `Hub.Server` into a standalone `Anubis.Server.Component` for TF-IDF tool search
- **`Exhub.MCP.Tools.Hub.CallTools`**: New `Anubis.Server.Component` for executing tools on upstream servers with automatic prefix-stripping retry
- **`Hub.Server` simplification**: Removed inline `tools/1` and `do_handle_tool_call("retrieve_tools", ...)` — now uses `component` declarations

### ClientManager Improvements
- **Search index auto-rebuild**: Index rebuilds after each successful upstream client connection
- **Improved error handling**: `extract_error_text/1` parses MCP response content for cleaner error messages
- **Built-in server tool counts**: Server status API correctly reports tool counts for built-in servers via `BuiltInRegistry`

### Files Changed
- `lib/exhub/mcp/hub/built_in_registry.ex` — NEW: Built-in server registry
- `lib/exhub/mcp/hub/store.ex` — NEW: Centralized ETS table owner
- `lib/exhub/mcp/tools/hub/retrieve_tools.ex` — NEW: Component extracted from Hub.Server
- `lib/exhub/mcp/tools/hub/call_tools.ex` — NEW: Tool execution component
- `lib/exhub/mcp/hub/client_manager.ex` — Built-in integration, search index rebuild, error handling
- `lib/exhub/mcp/hub/server.ex` — Component registration, removed inline handlers
- `lib/exhub/mcp/hub/server_config.ex` — Added `builtin` field
- `lib/exhub/mcp/hub/proxy_plug.ex` — Uses Hub.Store for session management
- `lib/exhub/application.ex` — Hub.Store in supervision tree

- **Full Docs**: [docs/modules/mcp-hub.md](modules/mcp-hub.md)

---

## MCP Hub Enhancement — Tool Search & Health Monitoring

- **New Feature**: MCP Hub now includes intelligent tool search, health monitoring, and auto-reconnect capabilities inspired by [mcpproxy-go](https://github.com/smart-mcp-proxy/mcpproxy-go)

### Tool Search & Discovery (`retrieve_tools`)
- **TF-IDF Search**: In-memory TF-IDF search index built from tool names and descriptions
- **Natural Language Queries**: Search with natural language (e.g., "read file", "search web") instead of browsing all tools
- **HTTP Endpoint**: `GET /mcp-hub/tools/search?query=<q>&limit=<n>` returns ranked results
- **Meta-Tool**: `retrieve_tools` is automatically exposed at the unified MCP endpoint for client-side discovery
- **Auto-Rebuild**: Index rebuilds automatically when servers connect/disconnect or tools change

### Health Monitoring & Auto-Reconnect
- **Periodic Health Checks**: Every 30 seconds, all connected clients are checked
- **Health Status Tracking**: Each client tracks `:healthy`, `:degraded`, or `:unhealthy` status
- **Auto-Reconnect**: Failed clients are automatically reconnected with exponential backoff (5s → 10s → 20s → 40s)
- **Max 3 Attempts**: After 3 failed reconnects, manual intervention is required
- **Structured Logging**: All tool calls are logged with timing information in both `Hub.Server` and `ClientManager`

### Files Changed
- `lib/exhub/mcp/hub/tool_search.ex` — NEW: TF-IDF search module
- `lib/exhub/mcp/hub/client_manager.ex` — Health checks, auto-reconnect, search API, logging
- `lib/exhub/mcp/hub/server.ex` — `retrieve_tools` meta-tool, tool call logging
- `lib/exhub/controllers/mcp_hub_controller.ex` — Search endpoint
- `lib/exhub/router.ex` — Route for `/mcp-hub/tools/search`
- `test/exhub/mcp/hub/tool_search_test.exs` — Search tests

- **Full Docs**: [docs/modules/mcp-hub.md](docs/modules/mcp-hub.md)

## MCP Tool Filtering via Headers
- **New Feature**: All MCP servers now support filtering the `tools/list` response via `x-include-tools` and `x-exclude-tools` HTTP headers
- **Include Filter**: `x-include-tools` accepts a comma-separated list of tool names; only matching tools are returned
- **Exclude Filter**: `x-exclude-tools` accepts a comma-separated list of tool names; matching tools are removed after the include filter
- **Case-Sensitive**: Tool names are trimmed but matched case-sensitively; unknown names are silently ignored
- **Combined Usage**: Both headers can be used together — exclusion is applied after inclusion
- **Universal Coverage**: Applied automatically to all MCP servers via `Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools/3`, including Agent, Archery, Brain, BrowserUse, Desktop, DocExtract, Exhub, Habit, Hub, ImageGen, Listen, Look, Think, Time, Todo, and WebTools servers
- **Full Docs**: [docs/modules/mcp-hub.md](docs/modules/mcp-hub.md)

## KuriDaemon — Auto-managed Chrome CDP Backend
- **New Daemon**: `Exhub.KuriDaemon` auto-starts and manages the `kuri` HTTP server binary via Exile
- **Zig-based Kuri Server**: Kuri is a Zig-based browser automation server that manages Chrome via CDP and exposes an HTTP API (tabs, navigate, snapshot, action, etc.)
- **Auto-detection**: Automatically finds the `kuri` binary from PATH, `~/Code/kuri/zig-out/bin/kuri`, or explicit `:kuri_binary` config
- **Health Monitoring**: Performs health checks every 30 seconds against the `/health` endpoint with automatic restart on failure
- **Graceful Shutdown**: Sends SIGTERM to the kuri process on Exhub shutdown for clean cleanup
- **Configuration**:
  | Key              | Default       | Description                    |
  |------------------|---------------|--------------------------------|
  | `:kuri_enabled`  | `true`        | Enable/disable the daemon      |
  | `:kuri_port`     | `18080`       | HTTP listen port               |
  | `:kuri_host`     | `"127.0.0.1"` | Bind address                  |
  | `:kuri_headless` | `true`        | Run Chrome headless            |
  | `:kuri_binary`   | `nil`         | Explicit path to `kuri` binary |
- **No Manual Chrome Startup**: Chrome is now auto-managed — no need to launch Chrome with `--remote-debugging-port` manually
- **Browser Backend**: Provides the browser backend for `kuri-agent` and BrowserUse MCP tools
- **Supervisor Integration**: Registered in `Exhub.Application` supervisor for automatic lifecycle management
- **Full Docs**: [lib/exhub/kuri_daemon.ex](../lib/exhub/kuri_daemon.ex) (moduledoc)

## MiMo AI Models Integration
- **New Provider**: Added support for Xiaomi MiMo AI models (`mimo-v2.5-pro`, `mimo-v2.5`) via OpenAI-compatible endpoint
- **Dedicated Endpoint**: Routes to `https://token-plan-sgp.xiaomimimo.com/v1` (Singapore region)
- **API Key**: Configured via `mimo_api_key` SecretVault entry
- **OpenAI Proxy**: Requests with `"model": "mimo-v2.5-pro"` or `"model": "mimo-v2.5"` are automatically routed through `/openai/v1/*path`
- **Setup**:
  ```bash
  mix scr.insert dev mimo_api_key "your-mimo-api-key"
  ```
- **Configuration**: LLM config entries added to `config/runtime.exs` for LangChain integration

## Org-mode Password Vault (exhub-vault)
- **New Feature**: Emacs org-mode password vault integrated with Exhub backend for managing encrypted secrets as org links
- **AES-256-GCM Encryption**: Secrets encrypted on the Elixir server using AES-256-GCM with the existing `SECRET_VAULT_PASSWORD` (SHA-256 key derivation, random 12-byte IV per encryption)
- **Org Link Integration**: Secrets stored as `[[exhub-vault:BASE64_CIPHERTEXT][description]]` links — native org-mode rendering with custom face (forest green, underlined)
- **Three Core Commands**:
  - `exhub-vault-insert-secret` (`C-c v i`) — prompt for description + secret, encrypt, insert as org link
  - `exhub-vault-decrypt-and-copy` (`C-c v c`) — decrypt link at point or selected region, copy to clipboard
  - `exhub-vault-decrypt-and-show` (`C-c v s`) — decrypt and display in minibuffer
- **Org Link Follow**: `C-c C-o` on vault links triggers decrypt & copy via `org-link-set-parameters`
- **WebSocket Handler**: `Exhub.ResponseHandlers.ExhubVault` processes encrypt/decrypt via the existing WebSocket channel (same pattern as `exhub-tool`, `exhub-file`, etc.)
- **Zero Additional Configuration**: Reuses the existing `SECRET_VAULT_PASSWORD` — no new secrets or environment variables needed
- **Minor Mode**: `exhub-vault-mode` provides `C-c v` keybinding prefix; can be enabled globally or via `org-mode-hook`
- **New Files**: `exhub-vault.el` (Emacs client), `lib/exhub/response_handlers/exhub_vault.ex` (Elixir handler)
- **Modified Files**: `lib/exhub/default_response_handler.ex` (registered vault handler)
- **Full Docs**: [docs/modules/vault.md](docs/modules/vault.md)

## ACP Agent MCP Server (AI Coding Agents Integration)
- **New MCP Server**: `Exhub.MCP.AgentServer` provides an MCP-based bridge to ACP (Agent Communication Protocol) agents like Claude Code, Gemini CLI, OpenCode, and Codex
- **14 MCP Tools** across five categories:
  - **Lifecycle (5)**: `agent_initialize`, `agent_shutdown`, `agent_list_running`, `agent_get_status`, `agent_set_status` — spawn and manage agent processes
  - **Sessions (4)**: `agent_new_session`, `agent_load_session`, `agent_list_sessions`, `agent_close_session` — session lifecycle management
  - **Prompts (3)**: `agent_prompt_start` (non-blocking), `agent_prompt_events` (long-poll with collect window), `agent_cancel` — interact with agents via prompts
  - **Permissions (1)**: `agent_grant_permission` — operator mode for approving tool usage requests
  - **Modes (1)**: `agent_set_mode` — switch agent operating modes (auto/manual)
- **AgentStore GenServer**: `Exhub.MCP.Agent.Store` manages running agents with event queues, session tracking, and permission state
- **ACP Protocol Handler**: `Exhub.MCP.Agent.Handler` implements `ExMCP.ACP.Client.Handler` for session updates, permission requests, and file operations
- **Configuration**: Agent definitions loaded from `~/.config/exhub/agents.json` (override via `EXHUB_AGENTS_CONFIG` env var)
- **Multi-Agent Support**: Run multiple agents simultaneously with isolated sessions and event queues
- **Event-Driven Architecture**: Asynchronous event polling with waiter pattern for streaming responses
- **HTTP Endpoint**: Exposed at `/agent/mcp` for MCP protocol communication
- **Full Docs**: [docs/modules/agent-mcp.md](docs/modules/agent-mcp.md) and [docs/agent-mcp-quickstart.md](docs/agent-mcp-quickstart.md)

## Document Extraction (PDF, DOCX, Images via Gitee AI)
- **New MCP Server**: `Exhub.MCP.DocExtractServer` exposes document text extraction over MCP at `/doc-extract/mcp`
- **Gitee AI Backend**: Calls the Gitee AI Async Document Parse API (`https://ai.gitee.com/v1/async/documents/parse`) powered by PaddleOCR-VL-1.5 — no external scripts required
- **Async Polling Flow**: Submits a parse task, polls `https://ai.gitee.com/v1/task/{task_id}` every 5 seconds, up to 60 attempts (5-minute maximum), then returns the full extracted content
- **Supported Input**: Local file paths and remote http/https URLs
- **Supported Formats**: PDF, DOCX, DOC, PNG, JPG, JPEG, TIFF, BMP, GIF, WEBP, and other document/image types
- **Output Formats**: Markdown (default, layout-preserving) or plain text
- **Extended Timeout**: Transport `request_timeout` set to 300 s (5 minutes) to accommodate long-running async extractions
- **Shared API key**: Reuses the existing `giteeai_api_key` SecretVault entry — no new secrets needed if already configured
- **HTTP Endpoint**: Exposed at `/doc-extract/mcp`
- **Full Docs**: [docs/modules/doc-extract.md](docs/modules/doc-extract.md)

## Desktop MCP Server (Filesystem & Process Commander)
- **New MCP Server**: `Exhub.MCP.DesktopServer` exposes local filesystem and process management over MCP at `/desktop/mcp`
- **16 Tools** across three categories:
  - **Filesystem (8)**: `read_file`, `write_file`, `list_directory`, `delete_file`, `move_file`, `create_directory`, `get_file_info`, `edit_block`
  - **Search (1)**: `search_files` — ripgrep → grep → native Elixir fallback; per-match independent context windows (never merges overlapping windows)
  - **Process (7)**: `execute_command`, `start_process`, `read_process_output`, `kill_process`, `list_managed_processes`, `list_processes`, `terminate_process`
- **ProcessStore**: `Exhub.MCP.Desktop.ProcessStore` GenServer tracks long-running managed processes with automatic 1-hour inactivity cleanup
- **TOON Encoding**: All responses are TOON-encoded (30–60% token reduction vs JSON) with automatic JSON fallback
- **Path Expansion**: All `path` parameters support `~` and `~/...` expansion to the user home directory
- **edit_block highlights**: exact case-sensitive match, LF/CRLF/CR line-ending normalization, `expected_replacements` guard, fuzzy-match fallback with Levenshtein similarity score and character-level diff `{-removed-}{+added+}`, >50-line warning
- **search_files context fix**: `context_lines` parameter correctly slices an independent `±N` line window per match — overlapping windows between nearby matches never merge or broadcast to each other
- **Supervisor Integration**: `Exhub.MCP.Desktop.ProcessStore` and `Exhub.MCP.DesktopServer` registered in the application supervisor
- **HTTP Endpoint**: Exposed at `/desktop/mcp`
- **Unit Tests**: 47 tests across 9 test files in `test/exhub/mcp/tools/desktop/`
- **Full Docs**: [docs/desktop_mcp_server.md](docs/desktop_mcp_server.md) and [docs/modules/desktop.md](docs/modules/desktop.md)

## MCP Todo Server (Multi-Tenant Todo List Management)
- **New MCP Server**: `Exhub.MCP.TodoServer` exposes a multi-tenant todo list service over MCP at `/todo/mcp`
- **In-Memory ETS Store**: Backed by `Exhub.MCP.TodoStore` — a GenServer using an ETS table (`:todo_store`) for fast, concurrent reads
- **Automatic TTL Expiry**: Todo lists that have not been updated for more than **2 hours** are automatically purged; a cleanup task runs every **30 minutes**
- **Four MCP Tools**:
  - `set_items` — Initialise or overwrite a tenant's todo list (accepts `items` array + `initial_user_prompt`)
  - `get_items` — Retrieve a tenant's current todo list along with the original prompt and item count
  - `update_item_completion` — Toggle the `completed` flag of a single item by name; returns the full updated list
  - `clear_items` — Remove all items from a tenant's list while keeping the tenant entry alive
- **Multi-Tenant Isolation**: Each tenant is identified by a `tenant_id` string; lists are fully isolated
- **No External Dependencies**: Pure in-memory implementation — no database, no API keys required
- **Supervisor Integration**: `Exhub.MCP.TodoStore` and `Exhub.MCP.TodoServer` registered in the application supervisor with streamable HTTP transport
- **HTTP Endpoint**: Exposed at `/todo/mcp` for MCP protocol communication
- **Full Docs**: [docs/modules/todo.md](docs/modules/todo.md)

## AI Image Generation
- **New MCP Server**: `Exhub.MCP.ImageGenServer` exposes AI image generation over MCP at `/image-gen/mcp`
- **Gitee AI Backend**: Calls the Gitee AI image generation API (OpenAI-compatible at `https://ai.gitee.com/v1/images/generations`) directly via HTTPoison — no external scripts required
- **5 Models Supported**:
  - `Qwen-Image` (default) — negative prompt + inference steps
  - `Kolors` — inference steps + guidance scale
  - `GLM-Image` — negative prompt + inference steps + guidance scale
  - `FLUX.2-dev` — negative prompt + inference steps + guidance scale
  - `HunyuanDiT-v1.2-Diffusers-Distilled` — negative prompt + inference steps + guidance scale
- **Per-model defaults**: Each model has sensible default `num_inference_steps` and `guidance_scale` values; unsupported params are silently filtered
- **10 output sizes**: From `256x256` to `2048x2048`, including landscape, portrait, and widescreen ratios
- **Shared API key**: Reuses the existing `giteeai_api_key` SecretVault entry — no new secrets needed if already configured
- **HTTP Endpoint**: Exposed at `/image-gen/mcp`
- **Full Docs**: [docs/modules/image-gen.md](docs/modules/image-gen.md)

## Browser Automation via kuri-agent
- **New MCP Server**: `Exhub.MCP.BrowserUseServer` exposes Chrome browser automation over MCP at `/browser-use/mcp`
- **Powered by kuri-agent**: Uses the `kuri-agent` CLI binary (Chrome CDP) via the `Exile` library for safe, back-pressure-aware process I/O
- **Tool Set** (6 focused tools replacing a single monolithic tool):
  - `browser_tabs`         — Discover open Chrome tabs, attach a session (`tabs` / `use` / `status`)
  - `browser_navigate`     — Navigate the browser (`go` / `back` / `forward` / `reload`)
  - `browser_inspect`      — Inspect page content with a11y snapshots, text extraction, JS eval, screenshots (`snap` / `text` / `eval` / `shot`)
  - `browser_interact`     — Interact with elements via `@eN` refs from a prior snap (`click` / `type` / `fill` / `select` / `hover` / `focus` / `scroll`)
  - `browser_security`     — Security testing: cookies, headers, full audit, storage dump, JWT scanning, authenticated fetch, IDOR probing
  - `browser_auth_headers` — Manage persistent CDP auth headers (`set_header` / `show_headers` / `clear_headers`)
- **Exile Integration**: Uses `Exile.stream/2` (not `stream!/2`) to tolerate non-zero exit codes from `kuri-agent` without raising
- **New Dependency**: `{:exile, "~> 0.10"}` added to `mix.exs`
- **Prerequisites**: `kuri-agent` binary must be on `PATH`. Chrome is now auto-managed by `KuriDaemon` — no manual startup required.
- **Full Docs**: [docs/modules/browser-use.md](docs/modules/browser-use.md)

## Archery SQL Audit Integration
- **New MCP Server**: New `Exhub.MCP.ArcheryServer` module providing MCP-compliant integration with the [Archery](https://github.com/hhyo/Archery) SQL audit platform
- **Active Tools**:
  - `get_instances` — retrieve the list of database instances
  - `get_databases` — list databases for a given instance
  - `query_execute` — execute read-only SQL queries (SELECT and safe operations only)
  - `get_query_history` — retrieve past query execution history
  - `get_resource_groups` — list available resource groups
  - `get_group_instances` — list instances belonging to a resource group
- **Temporarily Disabled Tools**: `sql_check`, `sql_review`, `get_workflow_list`, `get_workflow_detail`, `check_sql`, and `submit_workflow` are commented out pending further testing
- **HTTP Endpoint**: Exposed at `/archery/mcp` for MCP protocol communication
- **Configuration**: Configure via SecretVault:
  ```bash
  mix scr.insert dev archery_url "https://your-archery-instance.com"
  mix scr.insert dev archery_username "your-username"
  mix scr.insert dev archery_password "your-password"
  ```

## MCP Think/Plan Server
- **Think & Plan Tools**: New `Exhub.MCP.ThinkServer` module providing MCP-compliant reasoning scratchpad tools
- **Zero Side Effects**: Both tools simply echo their input back — no database writes, no external calls
- **Dual Tools**: `think` for recording reasoning thoughts and `plan` for outlining next steps
- **HTTP Endpoint**: Exposed at `/think/mcp` for MCP protocol communication
- **Supervisor Integration**: `Exhub.MCP.ThinkServer` registered in the application supervisor with streamable HTTP transport

## MCP Web Tools Server
- **Web Search & Fetch**: New `Exhub.MCP.WebToolsServer` module providing MCP-compliant web search and content fetching
- **Gitee AI Integration**: Web search powered by Gitee AI's web search API
- **Dual Tools**: `web_search` for AI-powered web searches and `web_fetch` for URL/file content retrieval
- **HTTP Endpoint**: Exposed at `/web-tools/mcp` for MCP protocol communication
- **Timeout Configuration**: Increased MCP transport timeout to 120s for long-running web operations
- **Schema Fix**: Fixed `count` parameter type from `:number` to `:integer` for proper validation

## Health Check Feature
- **URL Monitoring**: New `Exhub.HealthCheck` module monitors target URLs with scheduled checks
- **Webhook Notifications**: Supports Feishu/Lark and generic webhook providers for failure alerts
- **Flexible Configuration**: Configure multiple targets with custom expected status codes, timeouts, and HTTP methods
- **Quantum Scheduler Integration**: Uses cron syntax for scheduling health check intervals
- **Provider Support**: Built-in support for Feishu bot webhooks and generic JSON webhooks

## Habit Management Feature
- **MCP-Based Storage**: New `Exhub.MCP.HabitServer` and `Exhub.MCP.HabitStore` modules provide MCP-compliant habit management
- **Persistent Storage**: Habits stored in `~/.config/exhub/habits.json` with atomic writes
- **Protected Keys**: Automatically protects sensitive keys (user_id, email, api_key, password, secret, token)
- **MCP Tools**: Two tools available — `read_habits` for querying and `update_habits` for modifications
- **Metadata Support**: Each habit includes modifiable status, description, category, and timestamps
- **HTTP Endpoint**: Exposed at `/mcp` for MCP protocol communication

## Dependency Updates
- **Hermes MCP**: Updated from git dependency to hex package `~> 0.14.1`
- **Hermes Client API**: Updated to use `Hermes.Client.Base` module for tool operations

## Mac Keep Alive Feature
- **Bluetooth Connection Maintenance**: New `Exhub.MacKeepAlive` module automatically maintains Bluetooth connections using Quantum scheduler
- **Scheduled Health Checks**: Runs periodic connection checks every 5 minutes (configurable via cron syntax)
- **Device Management**: Connects to configured Bluetooth devices by name to prevent disconnection
- **macOS Integration**: Uses `blueutil` for Bluetooth operations on macOS
- **Configuration**: Set `device_name` in config to enable automatic reconnection

## Dependencies Update
- **Quantum Scheduler**: Added `{:quantum, "~> 3.0"}` for job scheduling capabilities
- **Supporting Dependencies**: Added `crontab`, `gen_stage`, and `telemetry_registry` for quantum requirements

## Configuration Improvements
- **DRY Configuration Approach**: Common API base and key values are now stored in variables for easier maintenance
- **Enhanced LLM Support**: Added support for new models including:
  - `qwen3-next-80b-a3b-instruct`
  - `deepseek-v3_1-terminus`
  - `deepseek-v3.2-exp`
  - `glm-4.6`

## Advanced Configuration Management
- **Improved LLM Config Server**: Enhanced validation, error handling, and type specifications
- **Better State Management**: Robust configuration state handling with fallback mechanisms
- **Type Safety**: Added proper type specifications for better code reliability

## Router Enhancements
- **Extended Model Support**: Updated router to support the new `qwen3-next-80b-a3b-instruct` model
- **Consistent API Mapping**: Improved API base and key mapping for all supported models
