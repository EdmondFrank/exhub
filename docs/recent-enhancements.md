# Recent Enhancements

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
