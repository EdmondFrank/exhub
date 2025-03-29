
# Exhub

Exhub is an Elixir-powered enhancement plugin for Emacs, based on WebSocket communication. It facilitates real-time interaction and communication between Emacs and the Elixir server.

## Features

- **WebSocket Communication**: Establishes a bi-directional connection between Emacs and Elixir using WebSockets.
- **Message Handling**: Enables sending and receiving messages between Emacs and the Elixir server.
- **Erlang/Elixir Backend**: Leverages Elixir and Erlang for robust backend processing.
- **Emacs Integration**: Provides Emacs Lisp functions to interact seamlessly with the Elixir server.
- **Agent Integration**: Allows integration with agents for enhanced functionality.
- **MCP Tools Integration**: Provides integration with MCP Tools for extended functionality.
- **Code Completion**: LLM-powered code completion with dual modes: specialized prompts and various enhancements for chat-based LLMs on code completion tasks, and fill-in-the-middle (FIM) completion for compatible models.

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

3. **Configuration**:

The configuration for Exhub is managed in `config/config.exs`. Here are the relevant settings:

- **LLM Configuration**:
  ```elixir
  config :exhub,
    llms: %{
      "qwen2.5-32b-instruct" => %{
        api_base: "https://ai.gitee.com/v1",
        api_key: "your api key",
        model: "qwen2.5-32b-instruct"
      },
      "gpt-4o" => %{
        api_base: "https://api.openai.com/v1",
        api_key: "your api key",
        model: "gpt-4o-mini"
      },
      # ...
    },
    gitee_cat: %{
      endpoint: "https://api.gitee.com/",
      auth: %{cookie: "your cookie"} # or %{access_token: "your acccess token"}
    }
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
- `gitee-open-pulls-buffer`: Open a new Org-mode buffer to display Gitee pulls.

## exhub-chat

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
- `exhub-start-file-mcp-server`: Start the File MCP server (required `npx` at first)
- `exhub-start-k8s-mcp-server`: Start the K8s MCP server (required `npx` and `kubectl` at first).
- `exhub-start-gitee-mcp-server`: Start the Gitee MCP server (required `mcp-gitee` at first).
- `exhub-start-github-mcp-server`: Start the GitHub MCP server (required `npx` at first).

#### Chat with Tools

- `exhub-chat-with-git`: Chat with Exhub using a registered Git server.
- `exhub-chat-with-tools`: Chat with Exhub using all mcp tools.

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

## Contributing

Feel free to contribute to Exhub by opening issues or pull requests on the [GitHub repository](https://github.com/edmondfrank/exhub).

## License

Exhub is licensed under the [MIT License](LICENSE).
