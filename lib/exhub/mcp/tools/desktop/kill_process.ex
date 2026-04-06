defmodule Exhub.MCP.Tools.Desktop.KillProcess do
  @moduledoc """
  MCP Tool: kill_process

  Send a signal to terminate a process — either a raw OS pid or a managed process_id.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.ProcessStore

  use Anubis.Server.Component, type: :tool

  def name, do: "kill_process"

  @impl true
  def description do
    """
    Send a signal to terminate a process.

    Two modes:
    1. By managed process_id: Provide the string ID returned by start_process.
       This updates the managed process status and sends a signal to the OS.
    2. By OS pid: Provide an integer process ID to send a signal directly.

    Sends SIGTERM by default (graceful shutdown). Use force: true to send
    SIGKILL for immediate termination.

    Parameters:
    - pid: Optional integer, the OS process ID
    - process_id: Optional string, the managed process ID from start_process
    - force: If true, send SIGKILL instead of SIGTERM (default false)

    Exactly one of pid or process_id must be provided.
    """
  end

  schema do
    field(:pid, :integer, description: "The OS process ID (integer)")
    field(:process_id, :string, description: "The managed process ID from start_process (string)")
    field(:force, :boolean, description: "If true, send SIGKILL instead of SIGTERM (default false)", default: false)
  end

  @impl true
  def execute(params, frame) do
    pid = Map.get(params, :pid)
    process_id = Map.get(params, :process_id)
    force = Map.get(params, :force, false)

    signal = if force, do: "-KILL", else: "-TERM"
    signal_name = if force, do: "SIGKILL", else: "SIGTERM"

    cond do
      process_id != nil ->
        kill_managed_process(process_id, signal, frame)

      pid != nil ->
        kill_os_process(pid, signal, signal_name, frame)

      true ->
        resp = Response.tool() |> Response.error("Provide either pid or process_id")
        {:reply, resp, frame}
    end
  end

  defp kill_managed_process(process_id, signal, frame) do
    case ProcessStore.kill_process(process_id) do
      {:ok, entry} ->
        # Also send OS signal if we have a pid
        if entry.pid != nil and is_integer(entry.pid) do
          _ =
            try do
              System.cmd("kill", [signal, to_string(entry.pid)], stderr_to_stdout: true)
            rescue
              _ -> :ok
            end
        end

        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "process_id" => process_id,
            "status" => "killed"
          })

        {:reply, resp, frame}

      {:error, :not_found} ->
        resp = Response.tool() |> Response.error("Process not found: #{process_id}")
        {:reply, resp, frame}

      {:error, :no_pid} ->
        resp = Response.tool() |> Response.error("Process has no associated system PID")
        {:reply, resp, frame}
    end
  end

  defp kill_os_process(pid, signal, signal_name, frame) do
    try do
      {output, exit_code} = System.cmd("kill", [signal, to_string(pid)], stderr_to_stdout: true)

      if exit_code == 0 do
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "pid" => pid,
            "signal" => signal_name
          })

        {:reply, resp, frame}
      else
        resp =
          Response.tool()
          |> Response.error("Failed to kill process #{pid} (exit #{exit_code}): #{String.trim(output)}")

        {:reply, resp, frame}
      end
    rescue
      e ->
        resp = Response.tool() |> Response.error("Error: #{Exception.message(e)}")
        {:reply, resp, frame}
    end
  end
end
