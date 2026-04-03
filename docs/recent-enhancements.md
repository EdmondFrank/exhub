# Recent Enhancements

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
- **Prerequisites**: `kuri-agent` binary must be on `PATH` and Chrome must be running with `--remote-debugging-port=9222`
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
