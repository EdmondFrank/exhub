# Exhub

Exhub is an Elixir-powered enhancement plugin for Emacs, based on WebSocket communication. It facilitates real-time interaction and communication between Emacs and the Elixir server.

## Features

- **WebSocket Communication**: Establishes a bi-directional connection between Emacs and Elixir using WebSockets.
- **Message Handling**: Enables sending and receiving messages between Emacs and the Elixir server.
- **Erlang/Elixir Backend**: Leverages Elixir and Erlang for robust backend processing.
- **Emacs Integration**: Provides Emacs Lisp functions to interact seamlessly with the Elixir server.
- **Agent Integration**: Allows integration with agents for enhanced functionality.
- **MCP Tools Integration**: Provides integration with MCP Tools for extended functionality.
- **Mac Keep Alive**: Maintains Bluetooth connections on macOS using scheduled health checks.
- **Health Check**: Monitors target URLs with scheduled checks and sends webhook notifications (supports Feishu/Lark and generic webhooks).
- **Habit Management**: MCP-based user habit and environment configuration storage with protected keys and persistent storage.
- **Think/Plan Tools**: MCP-based reasoning scratchpad tools (`think` and `plan`) for LLM chain-of-thought and step planning without side effects.
- **Archery SQL Audit**: MCP-based integration with the Archery SQL audit platform, exposing tools for querying instances, databases, executing read-only SQL, and retrieving query history and resource groups.
- **Time Tools**: MCP-based timezone-aware time utilities for getting current time and converting between timezones.
- **Code Completion**: LLM-powered code completion with dual modes: specialized prompts and various enhancements for chat-based LLMs on code completion tasks, and fill-in-the-middle (FIM) completion for compatible models.
- **Advanced Configuration Management**: Enhanced LLM configuration server with validation, error handling, and type specifications.

## Installation

### Elixir Server

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/edmondfrank/exhub.git
   cd exhub
   ```

2. **Install Dependencies**:
   ```bash
   mix deps.get
   ```

   **Note**: For Mac Keep Alive feature, ensure `blueutil` is installed:
   ```bash
   brew install blueutil
   ```

3. **Configuration**:

   Secrets (API keys, cookies, etc.) are managed via **SecretVault** and loaded at runtime from encrypted vault files. No secrets are hardcoded in `config/config.exs`.

   #### SecretVault Setup

   **a. Set the master password:**
   ```bash
   export SECRET_VAULT_PASSWORD="your-secure-master-password"
   ```

   **b. Create secrets** using the provided setup script (recommended):
   ```bash
   chmod +x scripts/setup_secrets.sh
   ./scripts/setup_secrets.sh
   ```

   Or insert secrets individually with `mix scr.insert`:
   ```bash
   mix scr.insert dev gitee_api_key        "your-gitee-ai-api-key"
   mix scr.insert dev openai_api_key       "your-openai-api-key"
   mix scr.insert dev gitee_cookie         "your-gitee-cookie-string"
   mix scr.insert dev siliconflow_api_key  "your-siliconflow-key"
   mix scr.insert dev mistral_api_key      "your-mistral-key"
   mix scr.insert dev codestral_api_key    "your-codestral-key"
   mix scr.insert dev anthropic_api_key    "your-anthropic-key"
   mix scr.insert dev groq_api_key         "your-groq-key"
   mix scr.insert dev gemini_api_key       "your-gemini-key"
   mix scr.insert dev cohere_api_key       "your-cohere-key"
   mix scr.insert dev samba_api_key        "your-samba-key"
   ```

   All LLM configurations and the Gitee Cat cookie are automatically assembled from these secrets at runtime via `config/runtime.exs`. Secrets are encrypted at rest using AES-256-GCM — only the encrypted `.vault_secret` files are committed to Git.

   > 📖 See [docs/SECRETS.md](docs/SECRETS.md) for full documentation on creating, editing, listing, and auditing secrets.
   > 🔄 Migrating from a previous hardcoded config? See [docs/MIGRATION.md](docs/MIGRATION.md).

   #### Customizing API Base URLs

   The default API base URLs for LLM providers are configured in `config/runtime.exs`. If you need to change the API endpoint for a specific provider (e.g., to use a proxy or different region), you can modify `config/runtime.exs`:

   ```elixir
   # Example: Change the API base for a specific model
   "openai/Qwen2.5-72B-Instruct" => %{
     api_base: "https://your-custom-endpoint.com/v1",  # Change this
     api_key: giteeai_api_key,
     model: "openai/Qwen2.5-72B-Instruct"
   }
   ```

   The `api_base` field can be customized per model while still using SecretVault-managed API keys.

   #### Other Optional Configuration

   The following optional settings can be placed in `config/config.exs`:

   ```elixir
   # Mac Keep Alive Configuration (optional)
   config :exhub, Exhub.MacKeepAlive,
     device_name: "Your Device Name",  # Must be a paired Bluetooth device
     jobs: [
       {"*/5 * * * *", {Exhub.MacKeepAlive, :run_keep_alive_check, []}}
     ]

   # Health Check Configuration (optional)
   config :exhub, Exhub.HealthCheck,
     targets: [
       [name: "Example API", url: "https://api.example.com/health"],
       [name: "Main Site", url: "https://example.com", expected_status: 200]
     ],
     webhook_url: "https://open.feishu.cn/open-apis/bot/v2/hook/your-webhook-token",
     webhook_provider: :feishu,  # or :default for generic webhooks
     jobs: [
       {"*/5 * * * *", {Exhub.HealthCheck, :run_health_checks, []}}
     ]

   # MCP Habit Server Configuration (optional)
   # The habit server provides an MCP endpoint at /mcp for managing user preferences
   # Habits are stored in ~/.config/exhub/habits.json with metadata tracking
   ```

4. **Build**:
   ```bash
   MIX_ENV=prod mix release
   ```

5. **Run the Server**:
   ```bash
   _build/prod/rel/exhub/bin/exhub start
   ```

### Emacs Setup

1. **Install the Emacs Package**:
   Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
   ```elisp
   (add-to-list 'load-path (expand-file-name "site-lisp/exhub" user-emacs-directory))
   (require 'exhub)
   (exhub-start-elixir)
   (exhub-start)
   ```

## Basic

### Sending Messages

Use the `exhub-send` function to send messages to the Elixir server:
```elisp
(exhub-send "your message here")
```

## Documentation

| Module             | Description                                                 | Doc                                                          |
|--------------------|-------------------------------------------------------------|--------------------------------------------------------------|
| exhub-tool         | MCP tool server integration (Git, File, K8s, Gitee, GitHub) | [docs/modules/tool.md](docs/modules/tool.md)                 |
| exhub-chat         | Chat, code generation, translation, and document polishing  | [docs/modules/chat.md](docs/modules/chat.md)                 |
| exhub-agent        | Agent-based interactions and tool orchestration             | [docs/modules/agent.md](docs/modules/agent.md)               |
| exhub-translate    | Symbol and region translation utilities                     | [docs/modules/translate.md](docs/modules/translate.md)       |
| exhub-file         | File operations and Markdown preview                        | [docs/modules/file.md](docs/modules/file.md)                 |
| exhub-fim          | LLM-powered fill-in-the-middle code completion              | [docs/modules/fim.md](docs/modules/fim.md)                   |
| exhub-keep-alive   | macOS Bluetooth connection maintenance                      | [docs/modules/keep-alive.md](docs/modules/keep-alive.md)     |
| exhub-health-check | URL monitoring with webhook notifications                   | [docs/modules/health-check.md](docs/modules/health-check.md) |
| exhub-habit        | MCP-based habit/environment config storage                  | [docs/modules/habit.md](docs/modules/habit.md)               |
| exhub-web-tools    | MCP web search and URL fetch tools                          | [docs/modules/web-tools.md](docs/modules/web-tools.md)       |
| exhub-think        | MCP reasoning scratchpad (think & plan tools)               | [docs/modules/think.md](docs/modules/think.md)               |
| exhub-archery      | MCP Archery SQL audit platform integration                  | [docs/modules/archery.md](docs/modules/archery.md)           |
| exhub-time         | MCP time utilities (timezone conversion, current time)      | [docs/modules/time.md](docs/modules/time.md)                 |

For a full changelog see [docs/recent-enhancements.md](docs/recent-enhancements.md).
For secrets management see [docs/SECRETS.md](docs/SECRETS.md) and [docs/MIGRATION.md](docs/MIGRATION.md).

## Contributing

Feel free to contribute to Exhub by opening issues or pull requests on the [GitHub repository](https://github.com/edmondfrank/exhub).

## License

Exhub is licensed under the [MIT License](LICENSE).
