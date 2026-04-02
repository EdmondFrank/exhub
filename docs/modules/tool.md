# exhub-tool

The `exhub-tool` package provides integration with MCP Tools using Exhub.

## Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-tool)
```

## Usage

### Start Tool MCP Server

- `exhub-start-git-mcp-server`: Start the Git MCP server (required `pip install mcp_server_git` at first).
- `exhub-start-file-mcp-server`: Start the File MCP server (required `npx` at first).
- `exhub-start-k8s-mcp-server`: Start the K8s MCP server (required `npx` and `kubectl` at first).
- `exhub-start-gitee-mcp-server`: Start the Gitee MCP server (required `mcp-gitee` at first).
- `exhub-start-github-mcp-server`: Start the GitHub MCP server (required `npx` at first).
- `exhub-start-gitee-mcp-ent-server`: Start the Gitee Enterprise MCP server (required `mcp-gitee-ent` at first).

### Stop Tool MCP Server

- `exhub-stop-git-mcp-server`: Stop the Git MCP server.
- `exhub-stop-file-mcp-server`: Stop the File MCP server.
- `exhub-stop-gitee-mcp-server`: Stop the Gitee MCP server.
- `exhub-stop-github-mcp-server`: Stop the GitHub MCP server.
- `exhub-stop-k8s-mcp-server`: Stop the Kubernetes MCP server.
- `exhub-stop-gitee-mcp-ent-server`: Stop the Gitee Enterprise MCP server.
- `exhub-stop-all-mcp-servers`: Stop all MCP servers.

### Chat with Tools

- `exhub-chat-with-git`: Chat with Exhub using a registered Git server.
- `exhub-chat-with-tools`: Chat with Exhub using all tools.
