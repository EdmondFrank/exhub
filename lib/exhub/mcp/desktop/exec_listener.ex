defmodule Exhub.MCP.Desktop.ExecListener do
  @moduledoc """
  Listens for output from erlexec-managed PTY processes and forwards it to ProcessStore.

  This module is spawned as a separate process. It calls `:exec.run/2` itself so
  that erlexec delivers stdout/stderr/DOWN messages directly to this process.
  The initial `{ok, Pid, OsPid}` result is sent back to the parent process.
  """

  require Logger

  alias Exhub.MCP.Desktop.ProcessStore

  @doc """
  Called by the spawned listener process. Runs the command via erlexec and
  enters a receive loop to forward output to ProcessStore.
  """
  def run(process_id, command, exec_opts, parent) do
    Process.flag(:trap_exit, true)
    _ref = Process.monitor(parent)

    case :exec.run(command, exec_opts) do
      {:ok, _erl_pid, ospid} ->
        send(parent, {:exec_started, ospid})
        do_loop(process_id, ospid, parent)

      {:error, reason} ->
        send(parent, {:exec_error, reason})
    end
  end

  defp do_loop(process_id, ospid, parent) do
    receive do
      {stream, ^ospid, data} when stream in [:stdout, :stderr] ->
        ProcessStore.append_output(process_id, data)
        do_loop(process_id, ospid, parent)

      {:DOWN, ^ospid, :process, _pid, reason} ->
        exit_code = exit_code_from_reason(reason)
        # set_exit_code handles status: preserves :terminated if process was
        # killed, otherwise sets :completed.
        ProcessStore.set_exit_code(process_id, exit_code)

        Logger.debug(
          "[ExecListener] Process #{process_id} (ospid: #{ospid}) exited: #{inspect(reason)}"
        )

      {:DOWN, _mon_ref, :process, ^parent, _reason} ->
        # Parent process died — stop the erlexec process and exit
        Logger.debug(
          "[ExecListener] Parent process died for #{process_id}, cleaning up ospid #{ospid}"
        )

        _ =
          try do
            :exec.stop(ospid)
          rescue
            _ -> :ok
          end

        :ok

      {:EXIT, _pid, _reason} ->
        do_loop(process_id, ospid, parent)

      other ->
        Logger.debug("[ExecListener] Received unexpected message: #{inspect(other)}")
        do_loop(process_id, ospid, parent)
    end
  end

  defp exit_code_from_reason(:normal), do: 0
  defp exit_code_from_reason({:exit_status, code}) when is_integer(code), do: code
  defp exit_code_from_reason(:killed), do: 137
  defp exit_code_from_reason(_), do: 1
end
