defmodule Exhub.MCP.DesktopServer do
  @moduledoc """
  MCP Server for desktop/filesystem/process control — inspired by DesktopCommanderMCP.

  ## Purpose
  Provides a comprehensive set of tools for interacting with the local filesystem
  and running system processes. Designed for AI agents that need to read, write,
  edit, search, and manage files, as well as execute and monitor shell commands.

  ## Tool Categories

  ### Filesystem
  - `read_file`        — Read file contents with optional line offset/limit
  - `read_multiple_files` — Read multiple files in a single call
  - `write_file`       — Write or append content to a file
  - `create_directory` — Create a directory (and parents) recursively
  - `list_directory`   — List directory contents with optional recursion depth
  - `move_file`        — Move or rename a file or directory
  - `get_file_info`    — Get metadata (size, type, timestamps) for a path
  - `delete_file`      — Delete a file or directory (optionally recursive)

  ### Editing
  - `edit_block`       — Targeted find-and-replace edit within a text file

  ### Process / Terminal
  - `execute_command`  — Run a shell command and return its output (uses Exile)
  - `start_process`    — Start a long-running command, returns process_id
  - `read_process_output` — Read output from a managed process
  - `interact_with_process` — Send input to an interactive process
  - `list_managed_processes` — List all tracked processes
  - `list_processes`   — List all system processes (uses Exile)
  - `kill_process`     — Kill a process by OS pid or managed process_id

  ### Search
  - `search_files`     — Search for files by name or search inside file contents

  The server is accessible at `/desktop/mcp`.
  """

  use Anubis.Server,
    name: "exhub-desktop-server",
    version: "1.1.0",
    capabilities: [:tools]

  # Filesystem tools
  component Exhub.MCP.Tools.Desktop.ReadFile
  component Exhub.MCP.Tools.Desktop.ReadMultipleFiles
  component Exhub.MCP.Tools.Desktop.WriteFile
  component Exhub.MCP.Tools.Desktop.CreateDirectory
  component Exhub.MCP.Tools.Desktop.ListDirectory
  component Exhub.MCP.Tools.Desktop.MoveFile
  component Exhub.MCP.Tools.Desktop.GetFileInfo
  component Exhub.MCP.Tools.Desktop.DeleteFile

  # Edit tools
  component Exhub.MCP.Tools.Desktop.EditBlock

  # Process / Terminal tools
  component Exhub.MCP.Tools.Desktop.ExecuteCommand
  component Exhub.MCP.Tools.Desktop.StartProcess
  component Exhub.MCP.Tools.Desktop.ReadProcessOutput
  component Exhub.MCP.Tools.Desktop.InteractWithProcess
  component Exhub.MCP.Tools.Desktop.ListManagedProcesses
  component Exhub.MCP.Tools.Desktop.ListProcesses
  component Exhub.MCP.Tools.Desktop.KillProcess

  # Search tools
  component Exhub.MCP.Tools.Desktop.SearchFiles

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
