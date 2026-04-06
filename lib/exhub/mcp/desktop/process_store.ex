defmodule Exhub.MCP.Desktop.ProcessStore do
  @moduledoc """
  GenServer that tracks running shell processes spawned by the Desktop MCP server.

  Each process is stored with its ID, command, output buffer, exit status, and
  metadata. Processes are automatically cleaned up after 1 hour of inactivity.

  This enables long-running commands to be started, monitored, and have their
  output read incrementally across multiple tool calls.
  """

  use GenServer

  require Logger

  @cleanup_interval_ms 60_000
  @max_age_ms 1_800_000

  defstruct [
    :id,
    :pid,
    :command,
    :output,
    :exit_code,
    :started_at,
    :last_read_at,
    :working_dir,
    :status
  ]

  # ============================================================================
  # Public API
  # ============================================================================

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, :ok, name: name)
  end

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

  # ============================================================================
  # GenServer callbacks
  # ============================================================================

  @impl true
  def init(:ok) do
    schedule_cleanup()
    {:ok, %{}}
  end

  @impl true
  def handle_call({:register, id, attrs}, _from, state) do
    entry = %__MODULE__{
      id: id,
      pid: attrs[:pid],
      command: attrs[:command],
      output: "",
      exit_code: nil,
      started_at: DateTime.utc_now(),
      last_read_at: now(),
      working_dir: attrs[:working_dir],
      status: :running
    }

    {:reply, {:ok, entry}, Map.put(state, id, entry)}
  end

  @impl true
  def handle_call({:get, id}, _from, state) do
    {:reply, Map.get(state, id), state}
  end

  @impl true
  def handle_call(:list, _from, state) do
    {:reply, Map.values(state), state}
  end

  @impl true
  def handle_call({:get_output, id, offset}, _from, state) do
    case Map.get(state, id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      entry ->
        output = entry.output
        total_lines = length(String.split(output, "\n"))
        sliced = String.split(output, "\n") |> Enum.drop(offset) |> Enum.join("\n")

        {:reply,
         {:ok,
          %{
            output: sliced,
            total_lines: total_lines,
            exit_code: entry.exit_code,
            status: entry.status
          }}, state}
    end
  end

  @impl true
  def handle_call({:kill_process, id}, _from, state) do
    case Map.get(state, id) do
      nil ->
        {:reply, {:error, :not_found}, state}

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
  def handle_cast({:append_output, id, data}, state) do
    state =
      Map.update(state, id, nil, fn
        nil -> nil
        entry -> %{entry | output: entry.output <> data, last_read_at: now()}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:set_exit_code, id, code}, state) do
    state =
      Map.update(state, id, nil, fn
        nil -> nil
        entry -> %{entry | exit_code: code, status: :completed, last_read_at: now()}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:set_status, id, status}, state) do
    state =
      Map.update(state, id, nil, fn
        nil -> nil
        entry -> %{entry | status: status, last_read_at: now()}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:update_pid, id, pid}, state) do
    state =
      Map.update(state, id, nil, fn
        nil -> nil
        entry -> %{entry | pid: pid}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:touch, id}, state) do
    state =
      Map.update(state, id, nil, fn
        nil -> nil
        entry -> %{entry | last_read_at: now()}
      end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:remove, id}, state) do
    {:noreply, Map.delete(state, id)}
  end

  @impl true
  def handle_info(:cleanup, state) do
    cutoff = now() - @max_age_ms

    {to_remove, to_keep} =
      Enum.split_with(state, fn {_id, entry} ->
        entry.last_read_at < cutoff
      end)

    if length(to_remove) > 0 do
      Logger.info("[ProcessStore] Cleaning up #{length(to_remove)} expired processes")

      # Try to kill any still-running expired processes
      Enum.each(to_remove, fn {id, entry} ->
        if entry.status == :running and is_integer(entry.pid) do
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
end
