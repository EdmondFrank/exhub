defmodule Exhub.MCP.Desktop.ProcessLauncher do
  @moduledoc """
  Spawns and tracks child processes for the desktop `execute_command` tool.

  A process is registered in `Exhub.MCP.Desktop.ProcessStore` *before* its child
  starts, so no output is dropped, and the machinery that owns the child (the
  stream consumer, the port listener, the erlexec listener) is deliberately not
  linked to the transient `tools/call` task that requested it — a child outlives
  the request that started it.

  ## Backends

  | `interactive` | `pty`   | Backend     | Used for                                                                |
  |---------------|---------|-------------|-------------------------------------------------------------------------|
  | `false`       | —       | Exile       | Commands whose output is streamed into `ProcessStore`                   |
  | `true`        | `false` | Erlang port | Pipe-based stdin/stdout (no TTY)                                        |
  | `true`        | `true`  | erlexec PTY | SSH, REPLs, anything that needs a TTY (default when `interactive: true`) |

  Only the Exile backend reports events back to the caller: while it streams the
  child's output into `ProcessStore` it also sends `{:exec_event, ref, event}`
  messages to the pid given as `:notify`, which is how `execute_command` can
  answer with the full result when a command finishes inside its wait window and
  with a `process_id` when it does not.
  """

  require Logger

  alias Exhub.MCP.Desktop.ExecListener
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.PortListener
  alias Exhub.MCP.Desktop.ProcessStore
  alias Exhub.MCP.Desktop.ProxyEnv

  @typedoc """
  A streaming-backend event forwarded to the `:notify` pid.
  """
  @type event ::
          {:stdout, binary()} | {:stderr, binary()} | {:exit, integer()} | {:error, String.t()}

  @doc """
  Returns a fresh managed-process id.
  """
  @spec new_process_id() :: String.t()
  def new_process_id do
    "proc_#{System.monotonic_time(:millisecond)}_#{:rand.uniform(9999)}"
  end

  @doc """
  Starts `command` in `working_dir` and registers it in `ProcessStore`.

  ## Options

    * `:interactive` — keep stdin open for `interact_with_process` (default
      `false`)
    * `:pty` — allocate a PTY; requires `:interactive` (defaults to
      `:interactive`)
    * `:notify` — pid that receives `{:exec_event, ref, event}` messages from
      the non-interactive streaming backend (default: no notifications)
    * `:ref` — term carried in those messages (defaults to the process id)
    * `:env` — environment for the child (keyword/charlist list). Defaults to
      `Helpers.clean_env/0` plus the cached proxy additions for `command`.

  Returns the registered entry; a backend that fails to start reports
  `{:error, reason}` only for the synchronous (PTY/port) backends — a streaming
  child that dies at spawn reports through the `:notify` event instead.
  """
  @spec start(String.t(), String.t(), String.t() | nil, keyword()) ::
          {:ok, struct()} | {:error, term()}
  def start(process_id, command, working_dir, opts \\ []) do
    interactive = Keyword.get(opts, :interactive, false)
    pty = Keyword.get(opts, :pty, interactive)

    cond do
      interactive and pty ->
        start_pty_process(process_id, command, working_dir)

      interactive ->
        start_interactive_process(process_id, command, working_dir)

      true ->
        start_streaming_process(process_id, command, working_dir, opts)
    end
  end

  defp ensure_erlexec_started do
    case Application.ensure_all_started(:erlexec) do
      {:ok, _apps} -> :ok
      {:error, {:already_started, _}} -> :ok
      _ -> :ok
    end
  end

  # `Exile.Process` needs the `:exile` application (its watcher supervisor) to be
  # running; without it the process GenServer crashes on start and no output or
  # pid is ever recorded.
  defp ensure_exile_started do
    case Application.ensure_all_started(:exile) do
      {:ok, _apps} -> :ok
      {:error, {:already_started, _}} -> :ok
      _ -> :ok
    end
  end

  defp start_interactive_process(process_id, command, working_dir) do
    ensure_erlexec_started()

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

    port_opts =
      case ProxyEnv.cached_additions(command) do
        [] ->
          port_opts

        additions ->
          [{:env, port_env(ProxyEnv.apply_to_env(Helpers.clean_env(), additions))} | port_opts]
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

  defp start_pty_process(process_id, command, working_dir) do
    ensure_erlexec_started()

    # Register the process FIRST so append_output calls don't get dropped
    entry_attrs = %{
      pid: nil,
      command: command,
      working_dir: working_dir,
      interactive: true,
      backend: :erlexec,
      ospid: nil
    }

    {:ok, _entry} = ProcessStore.register(process_id, entry_attrs)

    # Build erlexec options with PTY support
    # - :stdin    — enable stdin communication via exec:send/2
    # - :stdout   — capture stdout (sent to the calling process)
    # - {:stderr, :stdout} — merge stderr into stdout (PTY multiplexes both)
    # - :pty      — allocate a pseudo-terminal
    # - :pty_echo — enable terminal echo
    # - :monitor  — receive DOWN messages on process exit
    # Use {:pty, opts} instead of bare :pty to explicitly disable INLCR,
    # which translates input NL (\n) to CR (\r) — breaking line terminators
    # for SSH, REPLs, and other TTY-dependent programs.
    exec_opts = [
      :stdin,
      :stdout,
      {:stderr, :stdout},
      {:pty, [{:inlcr, false}]},
      :pty_echo,
      :monitor
    ]

    exec_opts =
      if working_dir do
        [{:cd, working_dir} | exec_opts]
      else
        exec_opts
      end

    # Spawn the listener process — it calls exec:run itself so it receives
    # all stdout/stderr/DOWN messages directly from erlexec
    parent = self()

    listener_pid =
      spawn(fn ->
        ExecListener.run(process_id, command, exec_opts, parent)
      end)

    # Wait for the listener to report the OsPid (or error)
    receive do
      {:exec_started, ospid} ->
        ProcessStore.set_ospid(process_id, ospid)
        ProcessStore.update_pid(process_id, ospid)

        entry = ProcessStore.get(process_id)
        {:ok, entry}

      {:exec_error, reason} ->
        ProcessStore.set_status(process_id, :error)

        Logger.error(
          "[ExecuteCommand] PTY process #{process_id} failed to start: #{inspect(reason)}"
        )

        {:error, inspect(reason)}
    after
      10_000 ->
        # Kill the listener process to prevent orphaned listener + OS process
        Process.exit(listener_pid, :kill)
        ProcessStore.set_status(process_id, :error)
        {:error, :timeout}
    end
  end

  # An Erlang port replaces the whole environment, so pass the merged list and
  # in its charlist form.
  defp port_env(env) do
    Enum.map(env, fn {key, value} ->
      {to_charlist(to_string(key)), to_charlist(to_string(value))}
    end)
  end

  defp spawn_port_listener(process_id) do
    spawn(fn ->
      PortListener.loop(process_id, nil)
    end)
  end

  defp start_streaming_process(process_id, command, working_dir, opts) do
    ex_opts = build_opts(working_dir, command, Keyword.get(opts, :env))

    # Register the process FIRST so append_output calls don't get dropped
    entry_attrs = %{
      pid: nil,
      command: command,
      working_dir: working_dir,
      interactive: false
    }

    {:ok, _entry} = ProcessStore.register(process_id, entry_attrs)

    # Start the background stream consumer as an *unlinked* task, so it (and the
    # Exile process it owns) outlives this transient request task. It reports
    # the child's real OS pid back so the response can carry it.
    ensure_exile_started()
    starter = self()
    notify = Keyword.get(opts, :notify)
    ref = Keyword.get(opts, :ref, process_id)

    {:ok, _task_pid} =
      Task.start(fn ->
        consume_process_stream(process_id, command, ex_opts, starter, {notify, ref})
      end)

    # Brief handshake — Exile reports the OS pid within milliseconds. On timeout
    # the entry is still returned (`pid: nil`) and the consumer fills it in.
    receive do
      {:stream_pid, ^process_id, _os_pid} -> :ok
    after
      2_000 -> :ok
    end

    entry = ProcessStore.get(process_id)
    {:ok, entry}
  end

  # Streams the child's output into ProcessStore and forwards each event to the
  # waiting caller (if any).
  #
  # This uses `Exile.Process` directly rather than `Exile.stream/2` so the real
  # OS pid is available (`Exile.Process.os_pid/1`). The previous implementation
  # shelled out to `pgrep -f <command>` and took the first match, which is the
  # *lowest* pid — for a short command like `ls` that matched an unrelated
  # process (a root daemon), so `kill_process` could signal the wrong one.
  defp consume_process_stream(process_id, command, ex_opts, starter, {notify, ref}) do
    try do
      {:ok, process} = Exile.Process.start_link(Helpers.shell_command_args(command), ex_opts)

      os_pid =
        case Exile.Process.os_pid(process) do
          {:ok, pid} when is_integer(pid) -> pid
          _ -> nil
        end

      if os_pid, do: ProcessStore.update_pid(process_id, os_pid)
      send(starter, {:stream_pid, process_id, os_pid})

      exit_code = pump_output(process_id, process, notify, ref)

      Logger.debug(
        "[ExecuteCommand] Process #{process_id} completed with exit #{inspect(exit_code)}"
      )

      # Ensure an exit code is recorded even if the stream ended unexpectedly,
      # then release the waiting caller — in that order, so a caller that reads
      # the entry on completion already sees the final status.
      ProcessStore.set_exit_code(process_id, exit_code || 0)
      notify(notify, ref, {:exit, exit_code || 0})
      exit_code
    rescue
      e ->
        Logger.error("[ExecuteCommand] Process #{process_id} error: #{Exception.message(e)}")
        ProcessStore.set_status(process_id, :error)
        send(starter, {:stream_pid, process_id, nil})
        notify(notify, ref, {:error, Exception.message(e)})
        {:error, Exception.message(e)}
    end
  end

  defp pump_output(process_id, process, notify, ref) do
    case Exile.Process.read_any(process) do
      {:ok, {stream, data}} ->
        ProcessStore.append_output(process_id, data)
        notify(notify, ref, {normalize_stream(stream), data})
        pump_output(process_id, process, notify, ref)

      :eof ->
        await_exit_code(process)

      {:error, errno} ->
        Logger.error("[ExecuteCommand] Process #{process_id} read error: #{inspect(errno)}")
        await_exit_code(process)
    end
  end

  defp normalize_stream(:stderr), do: :stderr
  defp normalize_stream(_stream), do: :stdout

  defp notify(pid, _ref, _event) when not is_pid(pid), do: :ok

  defp notify(pid, ref, event) do
    # The caller only waits for a bounded window; once it has replied it is gone
    # (Anubis runs each tools/call in a transient task) and the process keeps
    # streaming into ProcessStore for read_process_output.
    if Process.alive?(pid), do: send(pid, {:exec_event, ref, event})
    :ok
  end

  # `Exile.Process.await_exit/2` can `exit/1` if the owner is not the caller or
  # the program overruns the timeout, so keep it in a task-local try/catch and
  # treat an unknown status as "no exit code".
  defp await_exit_code(process) do
    case Exile.Process.await_exit(process, 5_000) do
      code when is_integer(code) -> code
      :epipe -> 0
      {:ok, code} when is_integer(code) -> code
      _ -> nil
    end
  rescue
    _ -> nil
  catch
    _, _ -> nil
  end

  # A command's proxy verdict is recorded by a blocking `execute_command` run, or
  # resolved up front when `ProxyEnv.mode() == :pre`; the streaming backend
  # starts before any failure is observable, so it reuses that verdict and
  # otherwise inherits the environment untouched.
  defp build_opts(working_dir, command, env) do
    env = env || ProxyEnv.apply_to_env(Helpers.clean_env(), ProxyEnv.cached_additions(command))
    base_opts = [stderr: :consume, env: env]

    if working_dir do
      Keyword.put(base_opts, :cd, working_dir)
    else
      base_opts
    end
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
