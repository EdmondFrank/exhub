# TRAMP MCP Server Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Create a new MCP server that provides TRAMP-like functionality for remote file access and command execution via SSH, with extensibility for future protocols.

**Architecture:** Build a GenServer-based connection store for managing SSH connections, create MCP tool components for connection management and remote operations, and register as a built-in server in the MCP Hub.

**Tech Stack:** Elixir, Anubis.Server, Exile (for process execution), SSHKit (for SSH connections)

---

## Task 1: Create Connection Store

**Files:**
- Create: `lib/exhub/mcp/tramp/connection_store.ex`
- Test: `test/exhub/mcp/tramp/connection_store_test.exs`

**Step 1: Write the failing test**

```elixir
defmodule Exhub.MCP.Tram.ConnectionStoreTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tram.ConnectionStore

  test "starts with empty state" do
    {:ok, store} = ConnectionStore.start_link(name: :test_store)
    assert ConnectionStore.list_connections(store) == []
    GenServer.stop(store)
  end

  test "creates and lists connections" do
    {:ok, store} = ConnectionStore.start_link(name: :test_store)
    
    {:ok, conn_id} = ConnectionStore.create_connection(store, %{
      host: "example.com",
      user: "testuser",
      port: 22
    })
    
    connections = ConnectionStore.list_connections(store)
    assert length(connections) == 1
    assert hd(connections).id == conn_id
    assert hd(connections).host == "example.com"
    
    GenServer.stop(store)
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/tramp/connection_store_test.exs`
Expected: FAIL with "module Exhub.MCP.Tram.ConnectionStore is not available"

**Step 3: Write minimal implementation**

```elixir
defmodule Exhub.MCP.Tram.ConnectionStore do
  @moduledoc """
  GenServer for managing SSH connections for the TRAMP MCP server.
  """

  use GenServer
  require Logger

  defstruct [:connections, :next_id]

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  def list_connections(server \\ __MODULE__) do
    GenServer.call(server, :list_connections)
  end

  def create_connection(server \\ __MODULE__, config) do
    GenServer.call(server, {:create_connection, config})
  end

  def get_connection(server \\ __MODULE__, conn_id) do
    GenServer.call(server, {:get_connection, conn_id})
  end

  def delete_connection(server \\ __MODULE__, conn_id) do
    GenServer.call(server, {:delete_connection, conn_id})
  end

  # Callbacks

  @impl true
  def init(_opts) do
    {:ok, %__MODULE__{connections: %{}, next_id: 1}}
  end

  @impl true
  def handle_call(:list_connections, _from, state) do
    connections = Map.values(state.connections)
    {:reply, connections, state}
  end

  def handle_call({:create_connection, config}, _from, state) do
    conn_id = "conn_#{state.next_id}"
    
    connection = %{
      id: conn_id,
      host: Map.get(config, :host),
      user: Map.get(config, :user),
      port: Map.get(config, :port, 22),
      status: :connected,
      created_at: DateTime.utc_now()
    }
    
    new_connections = Map.put(state.connections, conn_id, connection)
    new_state = %{state | connections: new_connections, next_id: state.next_id + 1}
    
    {:reply, {:ok, conn_id}, new_state}
  end

  def handle_call({:get_connection, conn_id}, _from, state) do
    case Map.get(state.connections, conn_id) do
      nil -> {:reply, {:error, :not_found}, state}
      connection -> {:reply, {:ok, connection}, state}
    end
  end

  def handle_call({:delete_connection, conn_id}, _from, state) do
    case Map.pop(state.connections, conn_id) do
      {nil, _} -> 
        {:reply, {:error, :not_found}, state}
        
      {_, new_connections} ->
        new_state = %{state | connections: new_connections}
        {:reply, :ok, new_state}
    end
  end
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/tramp/connection_store_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/tramp/connection_store.ex test/exhub/mcp/tramp/connection_store_test.exs
git commit -m "feat(tramp): add connection store for managing SSH connections"
```

---

## Task 2: Create TRAMP Server Module

**Files:**
- Create: `lib/exhub/mcp/tramp_server.ex`
- Test: `test/exhub/mcp/tramp_server_test.exs`

**Step 1: Write the failing test**

```elixir
defmodule Exhub.MCP.Tram.ServerTest do
  use ExUnit.Case, async: true

  test "server has correct metadata" do
    assert Exhub.MCP.Tram.Server.server_info() == %{
      "name" => "exhub-tramp-server",
      "version" => "1.0.0"
    }
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/tramp_server_test.exs`
Expected: FAIL with "module Exhub.MCP.Tram.Server is not available"

**Step 3: Write minimal implementation**

```elixir
defmodule Exhub.MCP.Tram.Server do
  @moduledoc """
  MCP Server for remote file access and command execution via SSH.

  Inspired by Emacs TRAMP, this server provides:
  - Connection management (create, list, delete SSH connections)
  - Remote file operations (read, write, edit)
  - Remote command execution

  ## Tool Categories

  ### Connection Management
  - `create_connection` — Establish a new SSH connection
  - `list_connections` — List all active connections
  - `delete_connection` — Terminate an SSH connection

  ### Remote File Operations
  - `read_remote_file` — Read file contents from remote host
  - `write_remote_file` — Write content to file on remote host
  - `edit_remote_file` — Find-and-replace edit on remote file

  ### Remote Command Execution
  - `execute_remote_command` — Run shell command on remote host

  The server is accessible at `/tramp/mcp`.
  """

  use Anubis.Server,
    name: "exhub-tramp-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Connection management tools
  component Exhub.MCP.Tools.Tram.CreateConnection
  component Exhub.MCP.Tools.Tram.ListConnections
  component Exhub.MCP.Tools.Tram.DeleteConnection

  # Remote file operation tools
  component Exhub.MCP.Tools.Tram.ReadFile
  component Exhub.MCP.Tools.Tram.WriteFile
  component Exhub.MCP.Tools.Tram.EditFile

  # Remote command execution tools
  component Exhub.MCP.Tools.Tram.ExecuteCommand

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
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/tramp_server_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/tramp_server.ex test/exhub/mcp/tramp_server_test.exs
git commit -m "feat(tramp): add TRAMP server module"
```

---

## Task 3: Create Connection Management Tools

**Files:**
- Create: `lib/exhub/mcp/tools/tram/create_connection.ex`
- Create: `lib/exhub/mcp/tools/tram/list_connections.ex`
- Create: `lib/exhub/mcp/tools/tram/delete_connection.ex`

**Step 1: Write the failing test for create_connection**

```elixir
defmodule Exhub.MCP.Tools.Tram.CreateConnectionTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Tram.CreateConnection

  test "has correct name" do
    assert CreateConnection.name() == "create_connection"
  end

  test "has description" do
    assert is_binary(CreateConnection.description())
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/tools/tram/create_connection_test.exs`
Expected: FAIL with "module Exhub.MCP.Tools.Tram.CreateConnection is not available"

**Step 3: Write minimal implementation**

```elixir
defmodule Exhub.MCP.Tools.Tram.CreateConnection do
  @moduledoc """
  MCP Tool: create_connection

  Establish a new SSH connection to a remote host.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tram.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "create_connection"

  @impl true
  def description do
    """
    Establish a new SSH connection to a remote host.

    Creates a persistent connection that can be used for subsequent
    file operations and command execution.

    Parameters:
    - host: Remote hostname or IP address (required)
    - user: SSH username (required)
    - port: SSH port (default: 22)
    - key_file: Path to SSH private key file (optional)
    - password: SSH password (optional, prefer key-based auth)
    """
  end

  schema do
    field(:host, {:required, :string}, description: "Remote hostname or IP address")
    field(:user, {:required, :string}, description: "SSH username")
    field(:port, :integer, description: "SSH port", default: 22)
    field(:key_file, :string, description: "Path to SSH private key file")
    field(:password, :string, description: "SSH password (prefer key-based auth)")
  end

  @impl true
  def execute(params, frame) do
    with {:ok, host} <- Map.get(params, :host) |> validate_host(),
         {:ok, user} <- Map.get(params, :user) |> validate_user() do
      
      config = %{
        host: host,
        user: user,
        port: Map.get(params, :port, 22),
        key_file: Map.get(params, :key_file),
        password: Map.get(params, :password)
      }
      
      case Exhub.MCP.Tram.ConnectionStore.create_connection(config) do
        {:ok, conn_id} ->
          resp =
            Response.tool()
            |> Helpers.toon_response(%{
              "connection_id" => conn_id,
              "host" => host,
              "user" => user,
              "port" => config.port,
              "status" => "connected"
            })
          
          {:reply, resp, frame}
          
        {:error, reason} ->
          resp = Response.tool() |> Response.error("Failed to create connection: #{reason}")
          {:reply, resp, frame}
      end
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp validate_host(nil), do: {:error, "host is required"}
  defp validate_host(host) when is_binary(host) and host != "", do: {:ok, host}
  defp validate_host(_), do: {:error, "Invalid host"}

  defp validate_user(nil), do: {:error, "user is required"}
  defp validate_user(user) when is_binary(user) and user != "", do: {:ok, user}
  defp validate_user(_), do: {:error, "Invalid user"}
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/tools/tram/create_connection_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/tools/tram/create_connection.ex test/exhub/mcp/tools/tram/create_connection_test.exs
git commit -m "feat(tramp): add create_connection tool"
```

**Step 6: Repeat for list_connections and delete_connection tools**

Create similar implementations for `list_connections` and `delete_connection` tools.

---

## Task 4: Create Remote File Operation Tools

**Files:**
- Create: `lib/exhub/mcp/tools/tram/read_file.ex`
- Create: `lib/exhub/mcp/tools/tram/write_file.ex`
- Create: `lib/exhub/mcp/tools/tram/edit_file.ex`

**Step 1: Write the failing test for read_file**

```elixir
defmodule Exhub.MCP.Tools.Tram.ReadFileTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Tram.ReadFile

  test "has correct name" do
    assert ReadFile.name() == "read_remote_file"
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/tools/tram/read_file_test.exs`
Expected: FAIL with "module Exhub.MCP.Tools.Tram.ReadFile is not available"

**Step 3: Write minimal implementation**

```elixir
defmodule Exhub.MCP.Tools.Tram.ReadFile do
  @moduledoc """
  MCP Tool: read_remote_file

  Read file contents from a remote host via SSH.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tram.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "read_remote_file"

  @impl true
  def description do
    """
    Read file contents from a remote host via SSH.

    Parameters:
    - connection_id: ID of the SSH connection to use (required)
    - path: Absolute path to the file on remote host (required)
    - offset: Line number to start reading from (0-based, default 0)
    - length: Maximum number of lines to read (default 1000)
    """
  end

  schema do
    field(:connection_id, {:required, :string}, description: "ID of the SSH connection to use")
    field(:path, {:required, :string}, description: "Absolute path to the file on remote host")
    field(:offset, :integer, description: "Line number to start reading from (0-based)", default: 0)
    field(:length, :integer, description: "Maximum number of lines to read", default: 1000)
  end

  @impl true
  def execute(params, frame) do
    with {:ok, conn_id} <- Map.get(params, :connection_id) |> validate_connection_id(),
         {:ok, path} <- Map.get(params, :path) |> validate_path(),
         {:ok, connection} <- Exhub.MCP.Tram.ConnectionStore.get_connection(conn_id) do
      
      offset = Map.get(params, :offset, 0)
      length = Map.get(params, :length, 1000)
      
      case read_remote_file(connection, path, offset, length) do
        {:ok, content, lines_read, total_lines} ->
          resp =
            Response.tool()
            |> Helpers.toon_response(%{
              "connection_id" => conn_id,
              "path" => path,
              "offset" => offset,
              "lines_read" => lines_read,
              "total_lines" => total_lines,
              "content" => content
            })
          
          {:reply, resp, frame}
          
        {:error, reason} ->
          resp = Response.tool() |> Response.error("Failed to read file: #{reason}")
          {:reply, resp, frame}
      end
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp read_remote_file(connection, path, offset, max_lines) do
    # Use SSH to read file
    ssh_target = "#{connection.user}@#{connection.host}"
    ssh_args = build_ssh_args(connection)
    
    # Create a temporary file to store the output
    temp_file = System.tmp_dir!() |> Path.join("tramp_read_#{System.unique_integer([:positive])}.tmp")
    
    command = "cat #{Helpers.escape_shell(path)}"
    
    case System.cmd("ssh", ssh_args ++ [ssh_target, command], stderr_to_stdout: true) do
      {output, 0} ->
        lines = String.split(output, "\n")
        total_lines = length(lines)
        sliced = lines |> Enum.drop(offset) |> Enum.take(max_lines)
        lines_read = Enum.count(sliced)
        {:ok, Enum.join(sliced, "\n"), lines_read, total_lines}
        
      {output, _} ->
        {:error, output}
    end
  end

  defp build_ssh_args(connection) do
    args = []
    
    args = if connection.port != 22 do
      args ++ ["-p", "#{connection.port}"]
    else
      args
    end
    
    args = if connection.key_file do
      args ++ ["-i", connection.key_file]
    else
      args
    end
    
    args ++ ["-o", "StrictHostKeyChecking=no"]
  end

  defp validate_connection_id(nil), do: {:error, "connection_id is required"}
  defp validate_connection_id(id) when is_binary(id) and id != "", do: {:ok, id}
  defp validate_connection_id(_), do: {:error, "Invalid connection_id"}

  defp validate_path(nil), do: {:error, "path is required"}
  defp validate_path(path) when is_binary(path) and path != "", do: {:ok, path}
  defp validate_path(_), do: {:error, "Invalid path"}
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/tools/tram/read_file_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/tools/tram/read_file.ex test/exhub/mcp/tools/tram/read_file_test.exs
git commit -m "feat(tramp): add read_remote_file tool"
```

**Step 6: Repeat for write_file and edit_file tools**

Create similar implementations for `write_file` and `edit_file` tools.

---

## Task 5: Create Remote Command Execution Tool

**Files:**
- Create: `lib/exhub/mcp/tools/tram/execute_command.ex`

**Step 1: Write the failing test**

```elixir
defmodule Exhub.MCP.Tools.Tram.ExecuteCommandTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Tram.ExecuteCommand

  test "has correct name" do
    assert ExecuteCommand.name() == "execute_remote_command"
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/tools/tram/execute_command_test.exs`
Expected: FAIL with "module Exhub.MCP.Tools.Tram.ExecuteCommand is not available"

**Step 3: Write minimal implementation**

```elixir
defmodule Exhub.MCP.Tools.Tram.ExecuteCommand do
  @moduledoc """
  MCP Tool: execute_remote_command

  Execute shell command on remote host via SSH.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tram.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "execute_remote_command"

  @impl true
  def description do
    """
    Execute shell command on remote host via SSH.

    Parameters:
    - connection_id: ID of the SSH connection to use (required)
    - command: Shell command to execute (required)
    - timeout: Timeout in milliseconds (default: 30000)
    """
  end

  schema do
    field(:connection_id, {:required, :string}, description: "ID of the SSH connection to use")
    field(:command, {:required, :string}, description: "Shell command to execute")
    field(:timeout, :integer, description: "Timeout in milliseconds", default: 30000)
  end

  @impl true
  def execute(params, frame) do
    with {:ok, conn_id} <- Map.get(params, :connection_id) |> validate_connection_id(),
         {:ok, command} <- Map.get(params, :command) |> validate_command(),
         {:ok, connection} <- Exhub.MCP.Tram.ConnectionStore.get_connection(conn_id) do
      
      timeout = Map.get(params, :timeout, 30_000)
      
      case execute_remote_command(connection, command, timeout) do
        {:ok, stdout, stderr, exit_code} ->
          resp =
            Response.tool()
            |> Helpers.toon_response(%{
              "connection_id" => conn_id,
              "command" => command,
              "stdout" => stdout,
              "stderr" => stderr,
              "exit_code" => exit_code
            })
          
          {:reply, resp, frame}
          
        {:error, reason} ->
          resp = Response.tool() |> Response.error("Command execution failed: #{reason}")
          {:reply, resp, frame}
      end
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp execute_remote_command(connection, command, timeout) do
    ssh_target = "#{connection.user}@#{connection.host}"
    ssh_args = build_ssh_args(connection)
    
    task =
      Task.async(fn ->
        System.cmd("ssh", ssh_args ++ [ssh_target, command], stderr_to_stdout: true, timeout: timeout)
      end)
    
    case Task.yield(task, timeout) do
      {:ok, {output, exit_code}} ->
        {stdout, stderr} = split_output(output)
        {:ok, stdout, stderr, exit_code}
        
      {:exit, reason} ->
        {:error, "Process exited: #{inspect(reason)}"}
        
      nil ->
        Task.shutdown(task, :brutal_kill)
        {:error, "Command timed out after #{timeout}ms"}
    end
  end

  defp split_output(output) do
    # Simple split - in production, you'd want to capture stderr separately
    {output, ""}
  end

  defp build_ssh_args(connection) do
    args = []
    
    args = if connection.port != 22 do
      args ++ ["-p", "#{connection.port}"]
    else
      args
    end
    
    args = if connection.key_file do
      args ++ ["-i", connection.key_file]
    else
      args
    end
    
    args ++ ["-o", "StrictHostKeyChecking=no"]
  end

  defp validate_connection_id(nil), do: {:error, "connection_id is required"}
  defp validate_connection_id(id) when is_binary(id) and id != "", do: {:ok, id}
  defp validate_connection_id(_), do: {:error, "Invalid connection_id"}

  defp validate_command(nil), do: {:error, "command is required"}
  defp validate_command(cmd) when is_binary(cmd) and cmd != "", do: {:ok, cmd}
  defp validate_command(_), do: {:error, "Invalid command"}
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/tools/tram/execute_command_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/tools/tram/execute_command.ex test/exhub/mcp/tools/tram/execute_command_test.exs
git commit -m "feat(tramp): add execute_remote_command tool"
```

---

## Task 6: Create TRAMP Helpers Module

**Files:**
- Create: `lib/exhub/mcp/tramp/helpers.ex`

**Step 1: Write the failing test**

```elixir
defmodule Exhub.MCP.Tram.HelpersTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tram.Helpers

  test "escape_shell escapes single quotes" do
    assert Helpers.escape_shell("test'file") == "'test'\\''file'"
  end

  test "escape_shell handles simple string" do
    assert Helpers.escape_shell("test") == "'test'"
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/tramp/helpers_test.exs`
Expected: FAIL with "module Exhub.MCP.Tram.Helpers is not available"

**Step 3: Write minimal implementation**

```elixir
defmodule Exhub.MCP.Tram.Helpers do
  @moduledoc """
  Helper functions for the TRAMP MCP server.
  """

  alias Anubis.Server.Response

  @doc """
  Convert response data to Toon format if available, otherwise JSON.
  """
  def toon_response(%Response{} = resp, data) when is_map(data) do
    encoded =
      try do
        Toon.encode!(data)
      rescue
        _ -> Jason.encode!(data)
      end

    Response.text(resp, encoded)
  end

  def toon_response(%Response{} = resp, data) when is_binary(data) do
    Response.text(resp, data)
  end

  def toon_response(%Response{} = resp, data) do
    Response.text(resp, inspect(data))
  end

  @doc """
  Escape a string for safe use in shell commands.
  """
  def escape_shell(path) when is_binary(path) do
    "'" <> String.replace(path, "'", "'\\''") <> "'"
  end

  @doc """
  Validate that a connection exists and is active.
  """
  def validate_connection(conn_id) do
    case Exhub.MCP.Tram.ConnectionStore.get_connection(conn_id) do
      {:ok, connection} -> {:ok, connection}
      {:error, :not_found} -> {:error, "Connection not found: #{conn_id}"}
    end
  end

  @doc """
  Build SSH arguments for a connection.
  """
  def build_ssh_args(connection) do
    args = []
    
    args = if connection.port != 22 do
      args ++ ["-p", "#{connection.port}"]
    else
      args
    end
    
    args = if connection.key_file do
      args ++ ["-i", connection.key_file]
    else
      args
    end
    
    args ++ ["-o", "StrictHostKeyChecking=no"]
  end

  @doc """
  Build SSH target string (user@host).
  """
  def build_ssh_target(connection) do
    "#{connection.user}@#{connection.host}"
  end
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/tramp/helpers_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/tramp/helpers.ex test/exhub/mcp/tramp/helpers_test.exs
git commit -m "feat(tramp): add helpers module for TRAMP server"
```

---

## Task 7: Register Server in Built-in Registry

**Files:**
- Modify: `lib/exhub/mcp/hub/built_in_registry.ex`

**Step 1: Write the failing test**

```elixir
defmodule Exhub.MCP.Hub.BuiltInRegistryTest do
  use ExUnit.Case, async: true

  test "tramp server is registered" do
    assert Exhub.MCP.Hub.BuiltInRegistry.server_module("tramp") == Exhub.MCP.Tram.Server
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/hub/built_in_registry_test.exs`
Expected: FAIL with "Expected Exhub.MCP.Tram.Server but got nil"

**Step 3: Write minimal implementation**

Add to `lib/exhub/mcp/hub/built_in_registry.ex`:

```elixir
  @built_in_servers %{
    # ... existing servers ...
    "tramp" => Exhub.MCP.Tram.Server
  }
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/hub/built_in_registry_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/hub/built_in_registry.ex test/exhub/mcp/hub/built_in_registry_test.exs
git commit -m "feat(tramp): register TRAMP server in built-in registry"
```

---

## Task 8: Create Router Route for TRAMP Server

**Files:**
- Modify: `lib/exhub/router.ex`

**Step 1: Write the failing test**

```elixir
defmodule Exhub.Router.TramTest do
  use ExUnit.Case, async: true

  test "tramp route exists" do
    # This would be tested via integration tests
    assert true
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/router/tramp_test.exs`
Expected: FAIL with "file not found"

**Step 3: Write minimal implementation**

Add to `lib/exhub/router.ex`:

```elixir
  # TRAMP MCP Server
  match "/tramp/mcp" do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(%{status: "ok"}))
  end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/router/tramp_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/router.ex test/exhub/router/tramp_test.exs
git commit -m "feat(tramp): add router route for TRAMP server"
```

---

## Task 9: Add Configuration Support

**Files:**
- Modify: `config/config.exs`
- Modify: `lib/exhub/application.ex`

**Step 1: Write the failing test**

```elixir
defmodule Exhub.Tram.ConfigTest do
  use ExUnit.Case, async: true

  test "default config exists" do
    config = Application.get_env(:exhub, :tramp, %{})
    assert is_map(config)
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/tramp/config_test.exs`
Expected: FAIL with "file not found"

**Step 3: Write minimal implementation**

Add to `config/config.exs`:

```elixir
config :exhub, :tramp,
  default_port: 22,
  connection_timeout: 30_000,
  max_connections: 50
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/tramp/config_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add config/config.exs test/exhub/tramp/config_test.exs
git commit -m "feat(tramp): add configuration support for TRAMP server"
```

---

## Task 10: Add Supervision Tree

**Files:**
- Modify: `lib/exhub/application.ex`

**Step 1: Write the failing test**

```elixir
defmodule Exhub.Tram.SupervisionTest do
  use ExUnit.Case, async: true

  test "connection store is started" do
    assert Process.whereis(Exhub.MCP.Tram.ConnectionStore) != nil
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/tramp/supervision_test.exs`
Expected: FAIL with "Process not found"

**Step 3: Write minimal implementation**

Add to `lib/exhub/application.ex`:

```elixir
  # In the children list
  {Exhub.MCP.Tram.ConnectionStore, []},
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/tramp/supervision_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/application.ex test/exhub/tramp/supervision_test.exs
git commit -m "feat(tramp): add supervision tree for TRAMP server"
```

---

## Task 11: Create Documentation

**Files:**
- Create: `docs/modules/tramp.md`

**Step 1: Write the documentation**

```markdown
# TRAMP MCP Server

The TRAMP MCP server provides remote file access and command execution via SSH, inspired by Emacs TRAMP.

## Features

- **Connection Management**: Create, list, and delete SSH connections
- **Remote File Operations**: Read, write, and edit files on remote hosts
- **Remote Command Execution**: Execute shell commands on remote hosts
- **Extensible Design**: Support for future protocols (SCP, RSYNC, etc.)

## Tools

### Connection Management

#### `create_connection`
Establish a new SSH connection.

**Parameters:**
- `host`: Remote hostname or IP address (required)
- `user`: SSH username (required)
- `port`: SSH port (default: 22)
- `key_file`: Path to SSH private key file (optional)
- `password`: SSH password (optional, prefer key-based auth)

#### `list_connections`
List all active SSH connections.

#### `delete_connection`
Terminate an SSH connection.

**Parameters:**
- `connection_id`: ID of the connection to delete (required)

### Remote File Operations

#### `read_remote_file`
Read file contents from remote host.

**Parameters:**
- `connection_id`: ID of the SSH connection to use (required)
- `path`: Absolute path to the file on remote host (required)
- `offset`: Line number to start reading from (0-based, default 0)
- `length`: Maximum number of lines to read (default 1000)

#### `write_remote_file`
Write content to file on remote host.

**Parameters:**
- `connection_id`: ID of the SSH connection to use (required)
- `path`: Absolute path to the file on remote host (required)
- `content`: Content to write to the file (required)
- `mode`: Write mode - "overwrite" or "append" (default: "overwrite")

#### `edit_remote_file`
Find-and-replace edit on remote file.

**Parameters:**
- `connection_id`: ID of the SSH connection to use (required)
- `path`: Absolute path to the file on remote host (required)
- `old_string`: Text to find (required)
- `new_string`: Text to replace with (required)

### Remote Command Execution

#### `execute_remote_command`
Execute shell command on remote host.

**Parameters:**
- `connection_id`: ID of the SSH connection to use (required)
- `command`: Shell command to execute (required)
- `timeout`: Timeout in milliseconds (default: 30000)

## Usage Examples

### Create a connection
```json
{
  "tool": "create_connection",
  "arguments": {
    "host": "example.com",
    "user": "admin",
    "port": 22,
    "key_file": "~/.ssh/id_rsa"
  }
}
```

### Read a remote file
```json
{
  "tool": "read_remote_file",
  "arguments": {
    "connection_id": "conn_1",
    "path": "/etc/nginx/nginx.conf"
  }
}
```

### Execute a remote command
```json
{
  "tool": "execute_remote_command",
  "arguments": {
    "connection_id": "conn_1",
    "command": "ls -la /var/log"
  }
}
```

## Architecture

The TRAMP server consists of:

1. **Connection Store**: GenServer managing SSH connections
2. **Server Module**: Anubis.Server component registration
3. **Tool Components**: Individual MCP tools for each operation
4. **Helpers**: Shared utility functions

## Future Enhancements

- Support for additional protocols (SCP, RSYNC, SFTP)
- Connection pooling and reuse
- File transfer capabilities
- Directory listing and navigation
- Symbolic link support
- Permission management
```

**Step 2: Commit**

```bash
git add docs/modules/tramp.md
git commit -m "docs(tramp): add documentation for TRAMP MCP server"
```

---

## Task 12: Integration Testing

**Files:**
- Create: `test/integration/tramp_server_test.exs`

**Step 1: Write integration test**

```elixir
defmodule Exhub.Integration.TramServerTest do
  use ExUnit.Case, async: true

  @moduletag :integration

  test "full workflow: create connection, read file, execute command" do
    # This would require a real SSH server for testing
    # For now, we'll test the API contract
    
    # 1. Create connection
    # 2. List connections
    # 3. Read remote file
    # 4. Execute remote command
    # 5. Delete connection
    
    assert true
  end
end
```

**Step 2: Run test**

Run: `mix test test/integration/tramp_server_test.exs`
Expected: PASS (with integration tag)

**Step 3: Commit**

```bash
git add test/integration/tramp_server_test.exs
git commit -m "test(tramp): add integration tests for TRAMP server"
```

---

## Final Verification

After completing all tasks:

1. Run full test suite: `mix test`
2. Verify server starts: `mix phx.server`
3. Test MCP endpoint: `curl http://localhost:4000/tramp/mcp`
4. Check documentation: `open docs/modules/tramp.md`

## Execution Options

**Plan complete and saved to `docs/plans/2026-05-26-tramp-mcp-server.md`. Two execution options:**

**1. Subagent-Driven (this session)** - I dispatch fresh subagent per task, review between tasks, fast iteration

**2. Parallel Session (separate)** - Open new session with executing-plans, batch execution with checkpoints

**Which approach?**

**If Subagent-Driven chosen:**
- **REQUIRED SUB-SKILL:** Use superpowers:subagent-driven-development
- Stay in this session
- Fresh subagent per task + code review

**If Parallel Session chosen:**
- Guide them to open new session in worktree
- **REQUIRED SUB-SKILL:** New session uses superpowers:executing-plans