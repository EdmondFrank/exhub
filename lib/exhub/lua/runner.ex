defmodule Exhub.Lua.Runner do
  @moduledoc """
  Resource bounds for a Lua evaluation.

  `run/2` executes a function in its own monitored (unlinked) process and returns
  its value, or `{:timeout, ms}` / `{:exit, reason}`. The caller is never linked,
  so a runaway evaluation or a heap kill cannot take it down.

  `cap_heap/1` sets the `max_heap_size` flag on the current process; call it
  *inside* the bounded process (before evaluating) so a heap kill is attributed
  to the sandbox rather than the caller.

  Legion's `Sandbox.Runner` additionally polls reductions, accounts off-heap
  binaries, lowers priority and kills on parent death; those are additive
  hardening, not parity requirements, and can be layered on here later.
  """

  @doc """
  Runs `fun` in a monitored, unlinked process under a wall-clock `timeout` (ms).

  Returns `{:ok, value}` when it finishes, `{:timeout, ms}` when it is killed at
  the deadline, or `{:exit, reason}` when it dies first (e.g. a heap kill).
  """
  @spec run((-> term()), timeout()) :: {:ok, term()} | {:timeout, timeout()} | {:exit, term()}
  def run(fun, timeout) when is_function(fun, 0) do
    parent = self()
    ref = make_ref()

    {pid, monitor} = spawn_monitor(fn -> send(parent, {ref, fun.()}) end)

    receive do
      {^ref, result} ->
        Process.demonitor(monitor, [:flush])
        {:ok, result}

      {:DOWN, ^monitor, :process, ^pid, reason} ->
        {:exit, reason}
    after
      timeout ->
        Process.exit(pid, :kill)

        receive do
          {:DOWN, ^monitor, :process, ^pid, _reason} -> :ok
        after
          1_000 -> :ok
        end

        {:timeout, timeout}
    end
  end

  @doc "Caps the current process's heap. `:infinity` / non-positive values are a no-op."
  @spec cap_heap(term()) :: :ok
  def cap_heap(:infinity), do: :ok

  def cap_heap(bytes) when is_integer(bytes) and bytes > 0 do
    words = div(bytes, :erlang.system_info(:wordsize))
    Process.flag(:max_heap_size, %{size: words, kill: true, error_logger: false})
    :ok
  end

  def cap_heap(_), do: :ok
end
