defmodule Exhub.MCP.Tools.Desktop.KillProcess do
  @moduledoc """
  MCP Tool: kill_process

  Send a signal to a running system process to terminate it.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "kill_process"

  @impl true
  def description do
    """
    Send a signal to a running system process to terminate it.

    Sends SIGTERM by default (graceful shutdown). Use force: true to send
    SIGKILL for immediate termination.

    Parameters:
    - pid: The process ID (PID) to kill
    - force: If true, send SIGKILL instead of SIGTERM (default false)
    """
  end

  schema do
    field(:pid, {:required, :integer}, description: "The process ID (PID) to kill")
    field(:force, :boolean, description: "If true, send SIGKILL instead of SIGTERM (default false)", default: false)
  end

  @impl true
  def execute(params, frame) do
    pid = Map.get(params, :pid)
    force = Map.get(params, :force, false)

    signal = if force, do: "-KILL", else: "-TERM"

    try do
      {_stdout, stderr, exit_code} =
        Exile.stream(["kill", signal, to_string(pid)], stderr: :consume)
        |> Enum.reduce({"", "", 0}, fn
          {:stdout, data}, {out, err, code} -> {out <> data, err, code}
          {:stderr, data}, {out, err, code} -> {out, err <> data, code}
          {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
          {:exit, :epipe}, {out, err, _} -> {out, err, 0}
          _, acc -> acc
        end)

      if exit_code == 0 do
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "success" => true,
            "pid" => pid,
            "signal" => if(force, do: "SIGKILL", else: "SIGTERM"),
            "message" => "Signal sent to process #{pid}."
          })

        {:reply, resp, frame}
      else
        resp =
          Response.tool()
          |> Response.error("Failed to kill process #{pid} (exit #{exit_code}): #{String.trim(stderr)}")

        {:reply, resp, frame}
      end
    rescue
      e ->
        resp = Response.tool() |> Response.error("Error: #{Exception.message(e)}")
        {:reply, resp, frame}
    end
  end
end
