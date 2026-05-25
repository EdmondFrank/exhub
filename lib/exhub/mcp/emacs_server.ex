defmodule Exhub.MCP.EmacsServer do
  @moduledoc """
  MCP Server for Emacs buffer operations.

  This server exposes buffer management tools via the Model Context Protocol,
  allowing AI agents to interact with Emacs buffers. It communicates with Emacs
  through the existing WebSocket connection established by Exhub.

  ## Purpose
  Provides tools for listing, reading, writing, and managing Emacs buffers.
  Designed for AI agents that need to interact with Emacs editor state.

  ## Tools
  - `emacs_list_buffers` — List all open buffers in Emacs
  - `emacs_read_buffer` — Read the content of a specific buffer
  - `emacs_write_buffer` — Write content to a specific buffer
  - `emacs_close_buffer` — Close a buffer with option to save or discard changes

  ## Communication
  The server communicates with Emacs by sending Elisp commands through the
  existing WebSocket connection and waiting for responses via a request-response
  mechanism built on top of the Registry pattern.

  The server is accessible at `/emacs/mcp`.
  """

  use Anubis.Server,
    name: "exhub-emacs-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Buffer operation tools
  component Exhub.MCP.Tools.Emacs.ListBuffers
  component Exhub.MCP.Tools.Emacs.ReadBuffer
  component Exhub.MCP.Tools.Emacs.WriteBuffer
  component Exhub.MCP.Tools.Emacs.CloseBuffer

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end

  @impl true
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end