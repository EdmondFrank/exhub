defmodule Exhub.MCP.Desktop.ExecListener do
  @moduledoc """
  Listens for output from erlexec-managed PTY processes and forwards it to ProcessStore.

  This module is spawned as a separate process. It calls `:exec.run/2` itself so
  that erlexec delivers stdout/stderr/DOWN messages directly to this process.
  The initial `{ok, Pid, OsPid}` result is sent back to the parent process.

  ## Lifetime

  The listener is deliberately **not** linked to, and does **not** monitor, the
  process that spawned it. That process is Anubis's transient `tools/call` task,
  which exits as soon as `start_process` returns; monitoring it (as this module
  used to) meant the child was stopped microseconds after it started — every
  `pty: true` process died instantly and captured no output.

  Instead the listener lives exactly as long as the child: it exits when erlexec
  reports the child's `:DOWN`. The child is stopped on demand by `kill_process`
  or the ProcessStore TTL sweep, both of which use the recorded `ospid`. `parent`
  is used only for the one-shot `{:exec_started, ospid}` / `{:exec_error, reason}`
  handshake back to the starter.
  """

  require Logger

  alias Exhub.MCP.Desktop.ProcessStore

  @doc """
  Called by the spawned listener process. Runs the command via erlexec and
  enters a receive loop to forward output to ProcessStore.
  """
  def run(process_id, command, exec_opts, parent) do
    Process.flag(:trap_exit, true)

    case :exec.run(command, exec_opts) do
      {:ok, erl_pid, ospid} ->
        Logger.debug(
          "[ExecListener] Process #{process_id} started via erlexec: erl_pid=#{inspect(erl_pid)} ospid=#{ospid}"
        )

        send(parent, {:exec_started, ospid})
        do_loop(process_id, ospid)

      {:error, reason} ->
        send(parent, {:exec_error, reason})
    end
  end

  defp do_loop(process_id, ospid) do
    receive do
      {stream, ^ospid, data} when stream in [:stdout, :stderr] ->
        Logger.debug(
          "[ExecListener] Received #{stream} data (#{byte_size(data)} bytes) for process #{process_id}"
        )

        ProcessStore.append_output(process_id, data)
        do_loop(process_id, ospid)

      {:DOWN, ^ospid, :process, _pid, reason} ->
        exit_code = exit_code_from_reason(reason)
        # set_exit_code handles status: preserves :terminated if the process was
        # killed, otherwise sets :completed.
        ProcessStore.set_exit_code(process_id, exit_code)

        Logger.debug(
          "[ExecListener] Process #{process_id} (ospid: #{ospid}) exited: #{inspect(reason)}"
        )

      {:EXIT, _pid, _reason} ->
        do_loop(process_id, ospid)

      other ->
        Logger.debug("[ExecListener] Received unexpected message: #{inspect(other)}")
        do_loop(process_id, ospid)
    end
  end

  defp exit_code_from_reason(:normal), do: 0
  defp exit_code_from_reason({:exit_status, code}) when is_integer(code), do: code
  defp exit_code_from_reason(:killed), do: 137
  defp exit_code_from_reason(_), do: 1
end
