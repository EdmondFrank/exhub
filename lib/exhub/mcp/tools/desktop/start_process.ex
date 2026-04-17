defmodule Exhub.MCP.Tools.Desktop.StartProcess do
  @moduledoc """
  MCP Tool: start_process

  Start a long-running shell command and return a process ID for later interaction.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.PortListener
  alias Exhub.MCP.Desktop.ProcessStore

  use Anubis.Server.Component, type: :tool

  require Logger

  def name, do: "start_process"

  @impl true
  def description do
    """
    Start a long-running shell command and return a process ID for later interaction.

    Unlike execute_command which waits for completion, start_process spawns the
    command in the background and immediately returns a process_id. Use this for
    interactive commands, servers, or any process that may take a while.

    The process output can be read incrementally using read_process_output.
    Processes are automatically cleaned up after 1 hour of inactivity.

    Parameters:
    - command: The shell command to execute
    - working_dir: Working directory for the command. Required unless the command
      uses absolute paths or includes 'cd'. (Current server pwd: #{Helpers.current_pwd()})
    """
  end

  schema do
    field(:command, :string, description: "The shell command to execute")
    field(:working_dir, :string, description: "Working directory for the command")
    field(:interactive, :boolean, default: false, description: "Enable interactive mode for sending input")
  end

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)
    working_dir = Map.get(params, :working_dir)
    interactive = Map.get(params, :interactive, false)

    cond do
      is_nil(command) ->
        resp = Response.tool() |> Response.error("Missing required parameter: command")
        {:reply, resp, frame}

      is_nil(working_dir) and Helpers.needs_working_dir?(command) ->
        resp =
          Response.tool()
          |> Response.error(
            "Missing required parameter: working_dir. It must be provided unless the command is absolute or includes 'cd'."
          )

        {:reply, resp, frame}

      true ->
        working_dir = Helpers.expand_path(working_dir)
        process_id = generate_process_id()

        case start_managed_process(process_id, command, working_dir, interactive) do
          {:ok, entry} ->
            resp =
              Response.tool()
              |> Helpers.toon_response(%{
                "process_id" => process_id,
                "pid" => entry.pid,
                "interactive" => entry.interactive
              })

            {:reply, resp, frame}
        end
    end
  end

  defp start_managed_process(process_id, command, working_dir, interactive) do
    if interactive do
      start_interactive_process(process_id, command, working_dir)
    else
      start_streaming_process(process_id, command, working_dir)
    end
  end

  defp start_interactive_process(process_id, command, working_dir) do
    # Register the process FIRST so append_output calls don't get dropped
    entry_attrs = %{
      pid: nil,
      command: command,
      working_dir: working_dir,
      interactive: true
    }

    {:ok, _entry} = ProcessStore.register(process_id, entry_attrs)

    # Build the shell command using spawn_executable for proper output capture
    shell = Helpers.get_shell()
    shell_path = resolve_shell_path(shell)
    full_args = Helpers.shell_command_args(command, login: true)
    # shell_command_args returns [shell_name, "-l", "-c", command] or similar
    # We need to extract the args after the shell path
    [_shell | args] = full_args

    # Open the port with :spawn_executable for reliable output capture
    port_opts = [:binary, :exit_status, :use_stdio, :stderr_to_stdout, {:args, args}]

    port_opts =
      if working_dir do
        [{:cd, working_dir} | port_opts]
      else
        port_opts
      end

    # Start the listener process FIRST - it will own the port
    listener_pid = spawn_port_listener(process_id)

    # Open the port (owned by current process temporarily)
    port = Port.open({:spawn_executable, shell_path}, port_opts)

    # Transfer port ownership to the listener process so it survives after this process exits
    true = Port.connect(port, listener_pid)

    # Tell the listener which port to monitor
    send(listener_pid, {:set_port, port})

    # Store the port reference
    ProcessStore.set_port(process_id, port)

    # Get the real OS PID using Port.info
    case Port.info(port, :os_pid) do
      {:os_pid, os_pid} -> ProcessStore.update_pid(process_id, os_pid)
      _ -> :ok
    end

    entry = ProcessStore.get(process_id)
    {:ok, entry}
  end

  defp spawn_port_listener(process_id) do
    spawn(fn ->
      PortListener.loop(process_id, nil)
    end)
  end

  defp start_streaming_process(process_id, command, working_dir) do
    opts = build_opts(working_dir)

    # Register the process FIRST so append_output calls don't get dropped
    entry_attrs = %{
      pid: nil,
      command: command,
      working_dir: working_dir,
      interactive: false
    }

    {:ok, _entry} = ProcessStore.register(process_id, entry_attrs)

    # Start the background stream consumer
    {:ok, _task_pid} =
      Task.start(fn ->
        consume_process_stream(process_id, command, opts)
      end)

    # Best-effort: resolve system PID and update entry
    system_pid = get_system_pid(command)
    if system_pid, do: ProcessStore.update_pid(process_id, system_pid)

    entry = ProcessStore.get(process_id)
    {:ok, entry}
  end

  defp consume_process_stream(process_id, command, opts) do
    try do
      {stdout, stderr, exit_code} =
        Exile.stream(Helpers.shell_command_args(command), opts)
        |> Enum.reduce({"", "", nil}, fn
          {:stdout, data}, {out, err, code} ->
            ProcessStore.append_output(process_id, data)
            {out <> data, err, code}

          {:stderr, data}, {out, err, code} ->
            ProcessStore.append_output(process_id, data)
            {out, err <> data, code}

          {:exit, {:status, code}}, {out, err, _} ->
            ProcessStore.set_exit_code(process_id, code)
            {out, err, code}

          {:exit, :epipe}, {out, err, _} ->
            ProcessStore.set_exit_code(process_id, 0)
            {out, err, 0}

          _, acc ->
            acc
        end)

      Logger.debug("[StartProcess] Process #{process_id} completed with exit #{exit_code}")

      # Ensure exit code is set even if stream ended unexpectedly
      if is_nil(exit_code) do
        ProcessStore.set_exit_code(process_id, 0)
      end

      {stdout, stderr, exit_code}
    rescue
      e ->
        Logger.error("[StartProcess] Process #{process_id} error: #{Exception.message(e)}")
        ProcessStore.set_status(process_id, :error)
        {:error, Exception.message(e)}
    end
  end

  defp get_system_pid(command) do
    # Try to get the PID of the most recent matching process
    # This is a best-effort approach for streaming processes
    # Note: Interactive processes use Port.info(:os_pid) instead
    try do
      pattern = Regex.escape(command) |> String.slice(0, 50)

      {output, 0} =
        System.cmd("pgrep", ["-f", pattern], stderr_to_stdout: true)

      output
      |> String.trim()
      |> String.split("\n")
      |> List.first()
      |> case do
        nil -> nil
        "" -> nil
        pid_str -> String.to_integer(pid_str)
      end
    rescue
      _ -> nil
    end
  end

  defp build_opts(working_dir) do
    base_opts = [stderr: :consume, env: Helpers.clean_env()]

    if working_dir do
      Keyword.put(base_opts, :cd, working_dir)
    else
      base_opts
    end
  end

  defp generate_process_id do
    "proc_#{System.monotonic_time(:millisecond)}_#{:rand.uniform(9999)}"
  end

  defp resolve_shell_path(shell) do
    case System.find_executable(shell) do
      nil ->
        # Try common locations in order
        fallbacks =
          case shell do
            "zsh" -> ["/bin/zsh", "/usr/bin/zsh"]
            "bash" -> ["/bin/bash", "/usr/bin/bash"]
            "sh" -> ["/bin/sh", "/usr/bin/sh"]
            _ -> ["/bin/sh", "/usr/bin/sh"]
          end

        Enum.find(fallbacks, shell, fn path -> File.exists?(path) end)

      path ->
        path
    end
  end
end
