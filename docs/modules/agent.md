# exhub-agent

The `exhub-agent` package provides agent integration for Emacs using Exhub.

## Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-agent)
```

## Usage

### Chat with Agents

- `exhub-agent-chat`: Chat with existing or new agents in the Exhub world.
- `exhub-agent-tool-reply`: Reply to an existing agent using the output of a shell command.
- `exhub-agent-init-tools`: Initialize tools with existing or new agents in the Exhub world.
- `exhub-agent-tool-call`: Interact with tools using existing or new agents in the Exhub world.
- `exhub-agent-kill`: Kill an existing agent in the Exhub world.

### Agent Keybindings

When in `exhub-agent-mode`, the following keybindings are available:

| Key     | Command                   |
|---------|---------------------------|
| `C-c c` | `exhub-agent-chat`        |
| `C-c r` | `exhub-agent-tool-reply`  |
| `C-c i` | `exhub-agent-init-tools`  |
| `C-c k` | `exhub-agent-kill`        |
| `C-c t` | `exhub-agent-tool-call`   |
