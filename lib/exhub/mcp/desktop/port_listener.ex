defmodule Exhub.MCP.Desktop.PortListener do
  @moduledoc """
  Listens for output from an Erlang port and forwards it to ProcessStore.

  This module is spawned as a separate process to receive messages from
  interactive ports started with start_process(interactive: true).
  """

  require Logger

  alias Exhub.MCP.Desktop.ProcessStore

  @doc """
  Main loop that receives port messages and forwards output to ProcessStore.
  """
  def loop(process_id, port) do
    # Trap exits to properly handle port termination
    Process.flag(:trap_exit, true)

    do_loop(process_id, port)
  end

  @port_timeout_ms 30_000

  defp do_loop(process_id, nil) do
    # Waiting for initial port assignment - use timeout to avoid hanging forever
    receive do
      {:set_port, new_port} ->
        # Port ownership transferred from caller - now enter main loop
        do_loop(process_id, new_port)
    after
      @port_timeout_ms ->
        Logger.error("[PortListener] No port received for process #{process_id} within 30 seconds")
        ProcessStore.set_status(process_id, :error)
        ProcessStore.set_exit_code(process_id, -1)
    end
  end

  defp do_loop(process_id, port) do
    receive do
      {:set_port, new_port} ->
        # Port ownership transferred from caller - now enter main loop
        do_loop(process_id, new_port)

      {^port, {:data, data}} when is_port(port) ->
        ProcessStore.append_output(process_id, data)
        do_loop(process_id, port)

      {^port, {:exit_status, status}} when is_port(port) ->
        ProcessStore.set_exit_code(process_id, status)
        ProcessStore.set_status(process_id, :completed)
        Logger.debug("[PortListener] Port exited for process #{process_id} with status #{status}")

      {^port, :closed} when is_port(port) ->
        ProcessStore.set_exit_code(process_id, 0)
        ProcessStore.set_status(process_id, :completed)
        Logger.debug("[PortListener] Port closed for process #{process_id}")

      {:EXIT, ^port, reason} when is_port(port) ->
        exit_code = exit_code_from_reason(reason)
        ProcessStore.set_exit_code(process_id, exit_code)
        ProcessStore.set_status(process_id, :completed)
        Logger.debug("[PortListener] Port exited for process #{process_id}: #{inspect(reason)}")

      {:EXIT, _pid, _reason} ->
        # Ignore exits from other processes
        do_loop(process_id, port)

      other ->
        Logger.debug("[PortListener] Received unexpected message: #{inspect(other)}")
        do_loop(process_id, port)
    end
  end

  defp exit_code_from_reason(:normal), do: 0
  defp exit_code_from_reason(:killed), do: 137
  defp exit_code_from_reason(_), do: 1
end
