# Emacs MCP Server

## Overview

The Emacs MCP Server provides buffer management capabilities for AI agents through the Model Context Protocol. It allows agents to interact with Emacs buffers by listing, reading, writing, and closing buffers.

## Architecture

The server is built on the existing Exhub architecture:

1. **Communication Layer**: Uses the WebSocket connection between Elixir and Emacs established by `Exhub.SocketHandler`
2. **MCP Framework**: Built on `Anubis.Server` like other MCP servers in the project
3. **Tool Pattern**: Follows the same component pattern as `BrainServer`, `DesktopServer`, etc.

## Tools

### 1. `emacs_list_buffers`

Lists open buffers in Emacs with filtering and pagination.

**Parameters:**
- `include_details` (boolean, optional): Whether to include buffer details like size and mode (default: false)
- `limit` (integer, optional): Maximum number of buffers to return (default: 20, 0 for no limit)
- `keyword` (string, optional): Filter buffers by keyword (case-insensitive match on buffer name)
- `include_total` (boolean, optional): Whether to include total buffer count (default: true)

**Examples:**
```json
{
  "include_details": false
}
```

```json
{
  "keyword": "el",
  "limit": 5,
  "include_details": true
}
```

```json
{
  "limit": 0,
  "include_total": true
}
```

### 2. `emacs_read_buffer`

Reads the content of a specific buffer.

**Parameters:**
- `buffer_name` (string, required): Name of the buffer to read
- `start_line` (integer, optional): Start line number (1-based)
- `end_line` (integer, optional): End line number (1-based)

**Example:**
```json
{
  "buffer_name": "*scratch*",
  "start_line": 1,
  "end_line": 10
}
```

### 3. `emacs_write_buffer`

Writes content to a specific buffer.

**Parameters:**
- `buffer_name` (string, required): Name of the buffer to write to
- `content` (string, required): Content to write
- `mode` (string, optional): Write mode - "replace" (default), "insert", or "append"
- `position` (integer, optional): Position for insert mode (1-based)

**Example:**
```json
{
  "buffer_name": "*scratch*",
  "content": "Hello, World!",
  "mode": "append"
}
```

### 4. `emacs_close_buffer`

Closes a buffer with option to save or discard changes.

**Parameters:**
- `buffer_name` (string, required): Name of the buffer to close
- `action` (string, required): Action - "save", "discard", or "close"

**Example:**
```json
{
  "buffer_name": "myfile.el",
  "action": "save"
}
```

## Implementation Details

### Communication Mechanism

The server communicates with Emacs by:

1. Sending Elisp commands through `Exhub.send_message/1`
2. Using a request-response pattern with unique request IDs
3. Waiting for responses via the Registry mechanism

### Elisp Command Execution

Commands are wrapped in error handling to catch and report Emacs errors:

```elisp
(let ((result (condition-case err
                (eval (read "<escaped-command>"))
                (error (format "Error: %s" (error-message-string err))))))
  (exhub-send-response "<request-id>" (format "%s" result)))
```

### Response Parsing

Responses are parsed from Elisp string representation:
- Buffer lists are parsed from `("buffer1" "buffer2" ...)` format
- String content is unescaped (newlines, tabs, quotes)

## Usage

### Starting the Server

The server is automatically started with the application. It's accessible at `/emacs/mcp`.

### Route Configuration

The server is mounted at `/emacs/mcp` in the router:

```elixir
forward("/emacs/mcp",
  to: Exhub.MCP.LazyPlug,
  init_opts: [server: Exhub.MCP.EmacsServer, request_timeout: 120_000]
)
```

### Agent Integration

Agents can use the tools through the MCP protocol:

```elixir
# Example: List recent buffers (default: 20 most recent)
{:ok, response} = Exhub.MCP.Hub.ClientManager.call_tool(
  "emacs_list_buffers",
  %{}
)

# Example: List buffers with details
{:ok, response} = Exhub.MCP.Hub.ClientManager.call_tool(
  "emacs_list_buffers",
  %{"include_details" => true}
)

# Example: Filter buffers by keyword
{:ok, response} = Exhub.MCP.Hub.ClientManager.call_tool(
  "emacs_list_buffers",
  %{"keyword" => "el", "limit" => 5}
)

# Example: List all buffers (no limit)
{:ok, response} = Exhub.MCP.Hub.ClientManager.call_tool(
  "emacs_list_buffers",
  %{"limit" => 0}
)

# Example: Read a buffer
{:ok, response} = Exhub.MCP.Hub.ClientManager.call_tool(
  "emacs_read_buffer",
  %{"buffer_name" => "*scratch*"}
)
```

## Future Enhancements

1. **Buffer Navigation**: Tools for moving between buffers
2. **Buffer Search**: Search within buffers
3. **Buffer Modification**: More advanced editing operations
4. **Window Management**: Tools for managing Emacs windows
5. **Mode-specific Operations**: Operations that depend on major mode

## Dependencies

- Exhub WebSocket connection must be established
- Emacs must have the `exhub-send-response` function defined
- The `exhub` Emacs package must be loaded

## Error Handling

The server handles various error conditions:
- Timeout waiting for Emacs response
- Invalid buffer names
- Buffer modification conflicts
- Elisp evaluation errors

All errors are returned as MCP error responses with descriptive messages.