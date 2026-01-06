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

The configuration for Exhub is managed in `config/config.exs`. Here are the relevant settings:

- **LLM Configuration with DRY Approach**:
  ```elixir
  # Store common values in variables instead of module attributes
  gitee_api_base = "https://ai.gitee.com/v1"
  gitee_api_key = "your api key"

  # Define LLM configurations using the DRY approach
  llms_config = %{
    "openai/Qwen2.5-72B-Instruct" => %{
      api_base: gitee_api_base,
      api_key: gitee_api_key,
      model: "openai/Qwen2.5-72B-Instruct"
    },
    "openai/Qwen3-235B-A22B" => %{
      api_base: gitee_api_base,
      api_key: gitee_api_key,
      model: "openai/Qwen3-235B-A22B"
    },
    "openai/qwen3-next-80b-a3b-instruct" => %{
      api_base: gitee_api_base,
      api_key: gitee_api_key,
      model: "openai/qwen3-next-80b-a3b-instruct"
    },
    "openai/qwen3-235b-a22b-instruct-2507" => %{
      api_base: gitee_api_base,
      api_key: gitee_api_key,
      model: "openai/qwen3-235b-a22b-instruct-2507"
    },
    "openai/qwen3-coder-480b-a35b-instruct" => %{
      api_base: gitee_api_base,
      api_key: gitee_api_key,
      model: "openai/qwen3-coder-480b-a35b-instruct"
    },
    "openai/kimi-k2-instruct" => %{
      api_base: gitee_api_base,
      api_key: gitee_api_key,
      model: "openai/kimi-k2-instruct"
    },
    "openai/cursor/gpt-4o-mini" => %{
      api_base: "http://127.0.0.1:9069/openai/v1",
      api_key: "your token",
      model: "openai/cursor/gpt-4o-mini"
    },
    "openai/gpt-4o-mini" => %{
      api_base: "http://localhost:4444/v1",
      api_key: "edmondfrank",
      model: "openai/gpt-4o-mini"
    },
    "openai/deepseek-v3_1-terminus" => %{
      api_base: gitee_api_base,
      api_key: gitee_api_key,
      model: "openai/deepseek-v3_1-terminus"
    },
    "openai/deepseek-v3.2-exp" => %{
      api_base: gitee_api_base,
      api_key: gitee_api_key,
      model: "openai/deepseek-v3.2-exp"
    },
    "openai/glm-4.6" => %{
      api_base: gitee_api_base,
      api_key: gitee_api_key,
      model: "openai/glm-4.6"
    },
    "openai/DeepSeek-V3" => %{
      api_base: gitee_api_base,
      api_key: gitee_api_key,
      model: "openai/DeepSeek-V3"
    },
    "openai/QwQ-32B" => %{
      api_base: "http://localhost:9069/samba/v1",
      api_key: "your token",
      model: "openai/QwQ-32B"
    },
    "openai/gemini-2.5-pro" => %{
      api_base: "http://127.0.0.1:9069/openai/v1",
      api_key: "your token",
      model: "openai/gemini-2.5-pro"
    },
    "openai/Qwen/Qwen2.5-Coder-32B-Instruct" => %{
      api_base: "https://api.siliconflow.cn/v1",
      api_key: "your token",
      model: "openai/Qwen/Qwen2.5-Coder-32B-Instruct"
    },
    "openai/Qwen/Qwen2.5-32B-Instruct" => %{
      api_base: "https://api.siliconflow.cn/v1",
      api_key: "your token",
      model: "openai/Qwen/Qwen2.5-32B-Instruct"
    },
    "codestral/codestral-latest" => %{
      api_base: "https://codestral.mistral.ai/v1",
      api_key: "your token",
      model: "mistral/codestral-latest"
    },
    "anthropic/claude-3-5-sonnet-latest" => %{
      api_base: "http://127.0.0.1:9069/anthropic/v1",
      api_key: "your token",
      model: "anthropic/claude-3-5-sonnet-latest"
    },
    "mistral/mistral-small-latest" => %{
      api_base: "https://api.mistral.ai/v1",
      api_key: "your token",
      model: "mistral/mistral-small-latest"
    },
    "mistral/mistral-large-latest" => %{
      api_base: "https://api.mistral.ai/v1",
      api_key: "your token",
      model: "mistral/mistral-large-latest"
    },
    "groq/llama-3.3-70b-versatile" => %{
      api_base: "http://127.0.0.1:9069/groq/v1",
      api_key: "your token",
      model: "openai/llama-3.3-70b-versatile"
    },
    "gemini/gemini-2.0-flash" => %{
      api_base: "http://127.0.0.1:9069/google/v1",
      api_key: "your token",
      model: "google/gemini-2.0-flash"
    },
    "command-r-plus" => %{
      api_base: "http://127.0.0.1:9069/cohere/v1",
      api_key: "your token",
      model: "openai/command-r-plus"
    },
    "command-a-03-2025" => %{
      api_base: "http://127.0.0.1:9069/cohere/v1",
      api_key: "your token",
      model: "openai/command-a-03-2025"
    }
  }

  config :exhub,
    gemini_api_base: "http://localhost:8765/v1",
    giteeai_api_key: gitee_api_key,
    openai_api_key: "your token",
    llms: llms_config,
    proxy: "http://127.0.0.1:7890",
     gitee_cat: %{
       endpoint: "https://api.gitee.com/",
       auth: %{cookie: "your cookie"} # or %{access_token: "your acccess token"}
     }

   # Mac Keep Alive Configuration (optional)
   config :exhub, Exhub.MacKeepAlive,
     device_name: "Your Device Name",  # Must be a paired Bluetooth device
     jobs: [
       {"*/5 * * * *", {Exhub.MacKeepAlive, :run_keep_alive_check, []}}
     ]
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

## exhub-tool

The `exhub-tool` package provides integration with MCP Tools using Exhub.

### Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-tool)
```

### Usage

#### Start Tool MCP Server

- `exhub-start-git-mcp-server`: Start the Git MCP server (required `pip install mcp_server_git` at first).
- `exhub-start-file-mcp-server`: Start the File MCP server (required `npx` at first).
- `exhub-start-k8s-mcp-server`: Start the K8s MCP server (required `npx` and `kubectl` at first).
- `exhub-start-gitee-mcp-server`: Start the Gitee MCP server (required `mcp-gitee` at first).
- `exhub-start-github-mcp-server`: Start the GitHub MCP server (required `npx` at first).
- `exhub-start-gitee-mcp-ent-server`: Start the Gitee Enterprise MCP server (required `mcp-gitee-ent` at first).

#### Stop Tool MCP Server

- `exhub-stop-git-mcp-server`: Stop the Git MCP server.
- `exhub-stop-file-mcp-server`: Stop the File MCP server.
- `exhub-stop-gitee-mcp-server`: Stop the Gitee MCP server.
- `exhub-stop-github-mcp-server`: Stop the GitHub MCP server.
- `exhub-stop-k8s-mcp-server`: Stop the Kubernetes MCP server.
- `exhub-stop-gitee-mcp-ent-server`: Stop the Gitee Enterprise MCP server.
- `exhub-stop-all-mcp-servers`: Stop all MCP servers.

#### Chat with Tools

- `exhub-chat-with-git`: Chat with Exhub using a registered Git server.
- `exhub-chat-with-tools`: Chat with Exhub using all tools.

## exhub-config

The `exhub-config` package provides configuration management for Exhub.

### Setup
```elisp
;; Built-in extension of exhub package
(require 'exhub)
```

### Usage

#### Switch Model

- `exhub-switch-model`: Switch the model by calling the Exhub configuration to list available models and prompt the user to select one.

## exhub-gitee

The `exhub-gitee` package provides Gitee integration for Emacs using Exhub.

### Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-gitee)
```

### Usage

- `gitee-open-issues-buffer`: Open a new Org-mode buffer to display Gitee issues.
- `gitee-open-issue-detail-buffer`: Open a new Org-mode buffer to display a Gitee issue detail.
- `gitee-open-pulls-buffer`: Open a new Org-mode buffer to display Gitee pulls.## exhub-chat

The `exhub-chat` package provides chat functionality for Emacs using Exhub.

### Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-chat)
```

### Usage

#### Chat with Exhub

- `exhub-chat`: Start a chat session with Exhub.
- `exhub-chat-with-temp-buffer`: Start a chat session with Exhub in a new temporary buffer.
- `exhub-chat-with-multiline`: Start a chat session with Exhub using a multiline input buffer.
- `exhub-chat-with-multiline-with-temp-buffer`: Start a chat session with Exhub using a new temporary buffer.
- `exhub-chat-optimize-prompts`: Optimize prompts using Exhub.

#### Code Generation

- `exhub-chat-generate-code`: Generate code using Exhub.
- `exhub-chat-adjust-code`: Adjust code using Exhub.
- `exhub-chat-comment-code`: Add comments to code using Exhub.
- `exhub-chat-refactory-code`: Refactor code using Exhub.
- `exhub-chat-format-code`: Format code using Exhub.

#### Document Polishing

- `exhub-chat-polish-document`: Polish and proofread a document using Exhub.
- `exhub-chat-improve-document`: Improve and correct grammar and spelling errors in a document using Exhub.

#### Code Explanation

- `exhub-chat-explain-code`: Explain code using Exhub.

#### Commit Message Generation

- `exhub-chat-generate-commit-message`: Generate a commit message using Exhub.
- `exhub-chat-generate-pull-desc`: Generate a pull request description using Exhub.
- `exhub-chat-generate-pull-review`: Generate a pull request review using Exhub.

#### Translation

- `exhub-chat-translate-into-chinese`: Translate text into Chinese using Exhub.
- `exhub-chat-translate-into-english`: Translate text into English using Exhub.

#### Draft Management

- `exhub-chat-choose-drafts`: Choose from saved drafts using Exhub.

## exhub-translate

The `exhub-translate` package provides translation functionality for Emacs using Exhub.

### Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-translate)
```

### Usage

#### Insert Translations

- `exhub-translate-insert`: Insert translation based on the current mode.
- `exhub-translate-insert-original-translation`: Insert original translation.
- `exhub-translate-insert-with-line`: Insert translation with line style.
- `exhub-translate-insert-with-underline`: Insert translation with underline style.
- `exhub-translate-insert-with-camel`: Insert translation with camel case style.

#### Replace Translations

- `exhub-translate-replace`: Replace the current symbol with its English translation.
- `exhub-translate-replace-with-line`: Replace with line style.
- `exhub-translate-replace-with-underline`: Replace with underline style.
- `exhub-translate-replace-with-camel`: Replace with camel case style.
- `exhub-translate-replace-zh`: Translate and replace the selected region to Chinese.

#### Posframe Translation

- `exhub-translate-posframe`: Show translation in a posframe.

## exhub-file

The `exhub-file` package provides file operations for Emacs using Exhub.

### Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-file)
```

### Usage

#### Markdown Rendering

- `exhub-preview-markdown`: Preview the current buffer with markdown using Exhub.

## exhub-agent

The `exhub-agent` package provides agent integration for Emacs using Exhub.

### Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-agent)
```

### Usage

#### Chat with Agents

- `exhub-agent-chat`: Chat with existing or new agents in the Exhub world.
- `exhub-agent-tool-reply`: Reply to an existing agent using the output of a shell command.
- `exhub-agent-init-tools`: Initialize tools with existing or new agents in the Exhub world.
- `exhub-agent-tool-call`: Interact with tools using existing or new agents in the Exhub world.
- `exhub-agent-kill`: Kill an existing agent in the Exhub world.

#### Agent Keybindings

When in `exhub-agent-mode`, the following keybindings are available:

- `C-c c`: `exhub-agent-chat`
- `C-c r`: `exhub-agent-tool-reply`
- `C-c i`: `exhub-agent-init-tools`
- `C-c k`: `exhub-agent-kill`
- `C-c t`: `exhub-agent-tool-call`

## exhub-fim

The `exhub-fim` package provides LLM-powered code completion with dual modes: specialized prompts and various enhancements for chat-based LLMs on code completion tasks, and fill-in-the-middle (FIM) completion for compatible models.

### Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-fim)
```

### Usage

#### Code Completion

- `exhub-fim-show-suggestion`: Show code suggestion using overlay at point.
- `exhub-fim-next-suggestion`: Cycle to next suggestion.
- `exhub-fim-previous-suggestion`: Cycle to previous suggestion.
- `exhub-fim-accept-suggestion`: Accept the current overlay suggestion.
- `exhub-fim-dismiss-suggestion`: Dismiss the current overlay suggestion.
- `exhub-fim-accept-suggestion-line`: Accept N lines of the current suggestion.
- `exhub-fim-complete-with-minibuffer`: Complete using minibuffer interface.

#### Automatic Suggestion

- `exhub-fim-auto-suggestion-mode`: Toggle automatic code suggestions.

#### Provider Configuration

- `exhub-fim-configure-provider`: Configure a exhub-fim provider interactively.

#### Using a Customize Gemini Proxy

If you are running the Elixir proxy server locally (default port 9069), set the Gemini provider to use the proxy endpoint:

```elisp
(setq exhub-fim-provider 'gemini)
;; The default :end-point in exhub-fim-gemini-options is already
;; "http://localhost:9069/google/v1/models", so no further change is needed.
```

Ensure the environment variable `GEMINI_API_KEY` is exported in the shell that launches Emacs:

```bash
export GEMINI_API_KEY="your-gemini-key"
```

## exhub-keep-alive

The `exhub-keep-alive` module provides automatic Bluetooth connection maintenance for macOS using the Quantum scheduler.

### Setup

The keep-alive feature is built into the Exhub application. No additional Emacs configuration is required.

### Configuration

Configure the keep-alive feature in `config/config.exs`:

```elixir
config :exhub, Exhub.MacKeepAlive,
  device_name: "Your Bluetooth Device Name",
  jobs: [
    {"*/5 * * * *", {Exhub.MacKeepAlive, :run_keep_alive_check, []}}
  ]
```

#### Configuration Options

- `device_name`: The name of your Bluetooth device to connect to (must be paired first)
- `jobs`: Quantum scheduler jobs (cron syntax). Default runs every 5 minutes.

### Prerequisites

- **macOS**: This feature uses macOS-specific Bluetooth commands
- **blueutil**: Install via Homebrew: `brew install blueutil`
- **Paired Device**: The Bluetooth device must be already paired with your Mac

### Usage

The keep-alive feature runs automatically based on the configured schedule. To manually trigger a connection check:

```bash
# The server will automatically run health checks based on the configured schedule
```

### Troubleshooting

- If Bluetooth is off, the connection will fail with `{:error, :bluetooth_off}`
- If the device is not paired, the connection will fail with `{:error, :not_paired}`
- Check server logs for detailed connection status information

## Recent Enhancements

### Mac Keep Alive Feature
- **Bluetooth Connection Maintenance**: New `Exhub.MacKeepAlive` module automatically maintains Bluetooth connections using Quantum scheduler
- **Scheduled Health Checks**: Runs periodic connection checks every 5 minutes (configurable via cron syntax)
- **Device Management**: Connects to configured Bluetooth devices by name to prevent disconnection
- **macOS Integration**: Uses `blueutil` for Bluetooth operations on macOS
- **Configuration**: Set `device_name` in config to enable automatic reconnection

### Dependencies Update
- **Quantum Scheduler**: Added `{:quantum, "~> 3.0"}` for job scheduling capabilities
- **Supporting Dependencies**: Added `crontab`, `gen_stage`, and `telemetry_registry` for quantum requirements

### Configuration Improvements
- **DRY Configuration Approach**: Common API base and key values are now stored in variables for easier maintenance
- **Enhanced LLM Support**: Added support for new models including:
  - `qwen3-next-80b-a3b-instruct`
  - `deepseek-v3_1-terminus`
  - `deepseek-v3.2-exp`
  - `glm-4.6`

### Advanced Configuration Management
- **Improved LLM Config Server**: Enhanced validation, error handling, and type specifications
- **Better State Management**: Robust configuration state handling with fallback mechanisms
- **Type Safety**: Added proper type specifications for better code reliability

### Router Enhancements
- **Extended Model Support**: Updated router to support the new `qwen3-next-80b-a3b-instruct` model
- **Consistent API Mapping**: Improved API base and key mapping for all supported models

## Contributing

Feel free to contribute to Exhub by opening issues or pull requests on the [GitHub repository](https://github.com/edmondfrank/exhub).

## License

Exhub is licensed under the [MIT License](LICENSE).
