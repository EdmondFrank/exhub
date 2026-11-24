defmodule Exhub.MCP.Desktop.ProcessStore do
  @moduledoc """
  GenServer that tracks running shell processes spawned by the Desktop MCP server.

  Each process is stored with its ID, command, output buffer, exit status, and
  metadata. Processes are automatically cleaned up after 1 hour of inactivity.

  This enables long-running commands to be started, monitored, and have their
  output read incrementally across multiple tool calls.

  ## Bounded output buffer

  The per-process buffer is capped at `max_output_bytes/0` (1 MB). A runaway
  command (`find /`, `yes`) would otherwise grow it without bound, and every
  `append_output` cast copies the whole binary — so the GenServer falls behind
  for seconds and *every other* desktop tool call queues behind it. When the cap
  is exceeded the oldest complete lines are dropped; `get_output/2` reports how
  much was dropped (`truncated`, `dropped_bytes`, `dropped_lines`) so a reader
  knows the transcript does not start at the beginning.
  """

  use GenServer

  require Logger

  @cleanup_interval_ms 60_000
  # Hard TTL: an entry is dropped once it has been idle this long, whatever its
  # state (a long-running process is kept until it stops and goes idle).
  @max_age_ms 1_800_000
  # A finished entry (`:completed`/`:terminated`/`:error`) is dropped much
  # sooner: its output is still readable for a grace period, but it must not
  # accumulate in `list_managed_processes` for an hour.
  @finished_ttl_ms 600_000
  # A `:running` entry that never got a child attached this long after being
  # registered is an orphan (its owning request died before the consumer
  # started) and is dropped.
  @orphan_running_ms 300_000
  # Hard cap on a single process's output buffer; see `max_output_bytes/0`.
  @max_output_bytes 1_000_000

  defstruct [
    :id,
    :pid,
    :command,
    :output,
    :exit_code,
    :started_at,
    :last_read_at,
    :working_dir,
    :status,
    :port,
    :interactive,
    :backend,
    :ospid,
    :consumer,
    :dropped_bytes,
    :dropped_lines
  ]

  # ============================================================================
  # Public API
  # ============================================================================

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

  @doc """
  Hard cap on a single process's output buffer, in bytes.

  Once exceeded, the oldest complete lines are dropped and the amount is
  reported by `get_output/2` (`truncated`, `dropped_bytes`, `dropped_lines`).
  """
  @spec max_output_bytes() :: pos_integer()
  def max_output_bytes, do: @max_output_bytes

  @doc "Register a new process in the store"
  def register(id, attrs) do
    GenServer.call(__MODULE__, {:register, id, attrs})
  end

  @doc "Get a process entry by ID"
  def get(id) do
    GenServer.call(__MODULE__, {:get, id})
  end

  @doc "Append output to a process's buffer"
  def append_output(id, data) do
    GenServer.cast(__MODULE__, {:append_output, id, data})
  end

  @doc "Set the exit code for a process"
  def set_exit_code(id, code) do
    GenServer.cast(__MODULE__, {:set_exit_code, id, code})
  end

  @doc "Update process status (running, completed, error)"
  def set_status(id, status) do
    GenServer.cast(__MODULE__, {:set_status, id, status})
  end

  @doc "Update the system PID for a process"
  def update_pid(id, pid) do
    GenServer.cast(__MODULE__, {:update_pid, id, pid})
  end

  @doc "Touch a process to update its last_read_at timestamp"
  def touch(id) do
    GenServer.cast(__MODULE__, {:touch, id})
  end

  @doc "List all tracked processes"
  def list do
    GenServer.call(__MODULE__, :list)
  end

  @doc "Get output for a process with optional offset"
  def get_output(id, offset \\ 0) do
    GenServer.call(__MODULE__, {:get_output, id, offset})
  end

  @doc "Remove a process from the store"
  def remove(id) do
    GenServer.cast(__MODULE__, {:remove, id})
  end

  @doc "Kill a running process and update its status"
  def kill_process(id) do
    GenServer.call(__MODULE__, {:kill_process, id})
  end

  @doc "Set the port reference for an interactive process"
  def set_port(id, port) do
    GenServer.cast(__MODULE__, {:set_port, id, port})
  end

  @doc """
  Record the pid of the process that streams this entry's output.

  Marks the entry as attached, so the cleanup pass does not mistake a healthy
  streaming process whose OS pid is not observable for an orphan.
  """
  def set_consumer(id, pid) do
    GenServer.cast(__MODULE__, {:set_consumer, id, pid})
  end

  @doc "Set the erlexec OsPid for an interactive PTY process"
  def set_ospid(id, ospid) do
    GenServer.cast(__MODULE__, {:set_ospid, id, ospid})
  end

  @doc "Send input to an interactive process's stdin"
  def send_input(id, data) do
    GenServer.call(__MODULE__, {:send_input, id, data})
  end

  # ============================================================================
  # GenServer callbacks
  # ============================================================================

  @impl true
  def init(:ok) do
    schedule_cleanup()
    {:ok, %{}}
  end

  @impl true
  def handle_call({:register, id, attrs}, {caller, _ref}, state) do
    # A caller whose `register` call already timed out is dead; registering for
    # it would leave an entry nothing ever drives (it sits in
    # `list_managed_processes` as `running` with no pid). Refuse instead.
    if Process.alive?(caller) do
      entry = %__MODULE__{
        id: id,
        pid: attrs[:pid],
        command: attrs[:command],
        output: "",
        exit_code: nil,
        started_at: DateTime.utc_now(),
        last_read_at: now(),
        working_dir: attrs[:working_dir],
        status: :running,
        port: attrs[:port],
        interactive: attrs[:interactive] || false,
        backend: attrs[:backend] || :port,
        ospid: attrs[:ospid],
        consumer: nil,
        dropped_bytes: 0,
        dropped_lines: 0
      }

      {:reply, {:ok, entry}, Map.put(state, id, entry)}
    else
      {:reply, {:error, :caller_gone}, state}
    end
  end

  @impl true
  def handle_call({:get, id}, _from, state) do
    {:reply, Map.get(state, id), state}
  end

  @impl true
  def handle_call(:list, _from, state) do
    {:reply, state |> Map.values() |> Enum.reject(&is_nil/1), state}
  end

  @impl true
  def handle_call({:get_output, id, offset}, _from, state) do
    case Map.get(state, id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      entry ->
        output = entry.output

        # Fix: empty string should have 0 lines, not 1
        total_lines =
          if output == "" do
            0
          else
            length(String.split(output, "\n"))
          end

        sliced = String.split(output, "\n") |> Enum.drop(offset) |> Enum.join("\n")

        # Update last_read_at timestamp
        updated_entry = %{entry | last_read_at: now()}
        new_state = Map.put(state, id, updated_entry)

        {:reply,
         {:ok,
          %{
            output: sliced,
            total_lines: total_lines,
            exit_code: entry.exit_code,
            status: entry.status,
            truncated: entry.dropped_bytes > 0,
            dropped_bytes: entry.dropped_bytes,
            dropped_lines: entry.dropped_lines
          }}, new_state}
    end
  end

  @impl true
  def handle_call({:kill_process, id}, _from, state) do
    case Map.get(state, id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      %{backend: :erlexec, ospid: ospid, status: :running} when is_integer(ospid) ->
        _ =
          try do
            :exec.stop(ospid)
          rescue
            _ -> :ok
          end

        entry = Map.get(state, id)
        updated = %{entry | status: :terminated, exit_code: -1, last_read_at: now()}
        {:reply, {:ok, updated}, Map.put(state, id, updated)}

      %{pid: nil} ->
        {:reply, {:error, :no_pid}, state}

      %{pid: pid, status: :running} = entry when is_integer(pid) ->
        # Kill the system process
        _ =
          try do
            {_, 0} = System.cmd("kill", ["-TERM", to_string(pid)], stderr_to_stdout: true)
          rescue
            _ -> :ok
          end

        updated = %{entry | status: :terminated, exit_code: -1, last_read_at: now()}
        {:reply, {:ok, updated}, Map.put(state, id, updated)}

      entry ->
        {:reply, {:ok, entry}, state}
    end
  end

  @impl true
  def handle_call({:send_input, id, data}, _from, state) do
    case Map.get(state, id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      %{interactive: false} ->
        {:reply, {:error, :not_interactive}, state}

      %{backend: :erlexec, ospid: ospid, status: :running} when is_integer(ospid) ->
        case :exec.send(ospid, data) do
          :ok -> {:reply, :ok, state}
          {:error, reason} -> {:reply, {:error, reason}, state}
        end

      %{backend: :erlexec, ospid: nil} ->
        {:reply, {:error, :no_ospid}, state}

      # Completed erlexec process (ospid set but not running) —
      # catch before %{port: nil} to avoid misleading :no_port error
      %{backend: :erlexec} ->
        {:reply, {:error, :process_not_running}, state}

      %{port: nil} ->
        {:reply, {:error, :no_port}, state}

      %{port: port, status: :running} ->
        case Port.command(port, data) do
          true -> {:reply, :ok, state}
          false -> {:reply, {:error, :port_closed}, state}
        end

      _entry ->
        {:reply, {:error, :process_not_running}, state}
    end
  end

  @impl true
  def handle_cast({:append_output, id, data}, state) do
    state =
      update_entry(state, id, fn entry ->
        entry |> append_capped(data) |> Map.put(:last_read_at, now())
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:set_exit_code, id, code}, state) do
    state =
      update_entry(state, id, fn entry ->
        # Only transition to :completed if the process was still :running.
        # If it was explicitly terminated (:terminated) or errored (:error),
        # preserve that status and only update the exit code.
        new_status = if entry.status == :running, do: :completed, else: entry.status
        %{entry | exit_code: code, status: new_status, last_read_at: now()}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:set_status, id, status}, state) do
    state =
      update_entry(state, id, fn entry ->
        %{entry | status: status, last_read_at: now()}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:update_pid, id, pid}, state) do
    state =
      update_entry(state, id, fn entry ->
        %{entry | pid: pid}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:touch, id}, state) do
    state =
      update_entry(state, id, fn entry ->
        %{entry | last_read_at: now()}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:remove, id}, state) do
    {:noreply, Map.delete(state, id)}
  end

  @impl true
  def handle_cast({:set_port, id, port}, state) do
    state =
      update_entry(state, id, fn entry ->
        %{entry | port: port}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:set_ospid, id, ospid}, state) do
    state =
      update_entry(state, id, fn entry ->
        %{entry | ospid: ospid}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:set_consumer, id, pid}, state) do
    state =
      update_entry(state, id, fn entry ->
        %{entry | consumer: pid}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_info(:cleanup, state) do
    cutoff = now() - @max_age_ms
    finished_cutoff = now() - @finished_ttl_ms

    now_dt = DateTime.utc_now()

    {to_remove, to_keep} =
      Enum.split_with(state, fn {_id, entry} ->
        is_nil(entry) or entry.last_read_at < cutoff or
          (finished?(entry) and entry.last_read_at < finished_cutoff) or
          orphaned?(entry, now_dt)
      end)

    if length(to_remove) > 0 do
      Logger.info("[ProcessStore] Cleaning up #{length(to_remove)} expired processes")

      # Try to kill any still-running expired processes
      Enum.each(to_remove, fn {id, entry} ->
        # Kill via erlexec if PTY-based interactive process
        if not is_nil(entry) and entry.status == :running and entry.backend == :erlexec and
             is_integer(entry.ospid) do
          _ =
            try do
              :exec.stop(entry.ospid)
            rescue
              _ -> :ok
            end
        end

        # Kill via port if interactive (Port.close terminates the OS process)
        if not is_nil(entry) and entry.status == :running and entry.interactive and
             is_port(entry.port) do
          try do
            Port.close(entry.port)
          rescue
            _ -> :ok
          end
        end

        # Kill via system pid if available (skip for interactive - already handled above)
        if not is_nil(entry) and entry.status == :running and not entry.interactive and
             is_integer(entry.pid) do
          _ =
            try do
              {_, 0} = System.cmd("kill", ["-KILL", to_string(entry.pid)], stderr_to_stdout: true)
            rescue
              _ -> :ok
            end
        end

        Logger.debug("[ProcessStore] Removed expired process: #{id}")
      end)
    end

    schedule_cleanup()
    {:noreply, Map.new(to_keep)}
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup, @cleanup_interval_ms)
  end

  defp now, do: System.monotonic_time(:millisecond)

  defp finished?(%{status: status}), do: status in [:completed, :terminated, :error]
  defp finished?(_entry), do: false

  # A `:running` entry with no attached consumer, port, ospid or pid this long
  # after registration never got a child (its owning request died between
  # `register` and the launcher's handshake).
  defp orphaned?(%{status: :running} = entry, now_dt) do
    entry.consumer == nil and entry.port == nil and entry.ospid == nil and
      entry.pid == nil and is_struct(entry.started_at, DateTime) and
      DateTime.diff(now_dt, entry.started_at, :millisecond) > @orphan_running_ms
  end

  defp orphaned?(_entry, _now_dt), do: false

  # Bounded append: see the moduledoc. Dropping whole lines keeps offsets in
  # `get_output/2` meaningful (they index the retained transcript).
  defp append_capped(entry, data) do
    output = entry.output <> data
    size = byte_size(output)

    if size > @max_output_bytes do
      {dropped, kept} = drop_head(output, size - @max_output_bytes)

      %{
        entry
        | output: kept,
          dropped_bytes: entry.dropped_bytes + byte_size(dropped),
          dropped_lines: entry.dropped_lines + count_newlines(dropped)
      }
    else
      %{entry | output: output}
    end
  end

  # Drops at least `excess` bytes from the front, cutting at a newline so the
  # retained buffer still starts on a whole line. One line longer than the cap
  # has no boundary to cut at, so it is trimmed byte-wise.
  defp drop_head(output, excess) do
    size = byte_size(output)

    case :binary.match(output, "\n", scope: {excess, size - excess}) do
      {pos, 1} ->
        {binary_part(output, 0, pos + 1), binary_part(output, pos + 1, size - pos - 1)}

      :nomatch ->
        {binary_part(output, 0, excess), binary_part(output, excess, size - excess)}
    end
  end

  defp count_newlines(binary), do: length(:binary.matches(binary, "\n"))

  defp update_entry(state, id, fun) do
    case Map.get(state, id) do
      nil -> state
      entry -> Map.put(state, id, fun.(entry))
    end
  end
end
