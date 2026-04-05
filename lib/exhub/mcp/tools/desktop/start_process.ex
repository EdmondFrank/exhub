defmodule Exhub.MCP.Tools.Desktop.StartProcess do
  @moduledoc """
  MCP Tool: start_process

  Start a long-running shell command and return a process ID for later interaction.
  """

  alias Anubis.Server.Response
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
    - working_dir: Working directory for the command (optional)
    """
  end

  schema do
    field(:command, {:required, :string}, description: "The shell command to execute")
    field(:working_dir, :string, description: "Working directory for the command (optional)")
  end

  @impl true
  def execute(params, frame) do
    command = Map.get(params, :command)
    working_dir = Map.get(params, :working_dir)

    process_id = generate_process_id()

    case start_managed_process(process_id, command, working_dir) do
      {:ok, entry} ->
        resp =
          Response.tool()
          |> Response.structured(%{
            "success" => true,
            "process_id" => process_id,
            "command" => command,
            "pid" => entry.pid,
            "status" => "running",
            "message" => "Process started. Use read_process_output with process_id to get output."
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to start process: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp start_managed_process(process_id, command, working_dir) do
    opts = build_opts(working_dir)

    # Start the process using Exile.stream with spawn mode
    # We use a Task to consume the stream asynchronously
    parent = self()

    {:ok, _task_pid} =
      Task.start(fn ->
        consume_process_stream(process_id, command, opts, parent)
      end)

    # Give the task a moment to start and get the system PID
    Process.sleep(100)

    # Get the actual system PID using pgrep or similar
    # For now, we use a placeholder that will be updated
    system_pid = get_system_pid(command)

    entry_attrs = %{
      pid: system_pid,
      command: command,
      working_dir: working_dir
    }

    ProcessStore.register(process_id, entry_attrs)
  end

  defp consume_process_stream(process_id, command, opts, _parent) do
    try do
      {stdout, stderr, exit_code} =
        Exile.stream(["sh", "-c", command], opts)
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
    # This is a best-effort approach
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

  defp build_opts(nil), do: [stderr: :consume]
  defp build_opts(working_dir), do: [stderr: :consume, cd: working_dir]

  defp generate_process_id do
    "proc_#{System.monotonic_time(:millisecond)}_#{:rand.uniform(9999)}"
  end
end
