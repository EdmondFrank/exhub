defmodule Exhub.MCP.Tools.Desktop.ListProcesses do
  @moduledoc """
  MCP Tool: list_processes

  List currently running system processes.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "list_processes"

  @impl true
  def description do
    """
    List currently running system processes.

    Returns a list of processes with PID, CPU/memory usage, VSZ, RSS, stat,
    time, command name, and full command path. Uses `ps aux` on Unix/macOS.

    Parameters:
      - filter: optional string to filter processes by command name (case-insensitive substring match)

    Returns per process:
      - pid: integer process ID
      - cpu: string CPU percentage (e.g. "50.2")
      - mem: string memory percentage (e.g. "3.0")
      - vsz: humanized virtual memory size (e.g. "395.5 MB")
      - rss: humanized resident set size (e.g. "491.5 MB")
      - stat: string process state (e.g. "R", "S")
      - time: string accumulated CPU time (e.g. "3776:37.31")
      - command: executable name (basename of cmd)
      - cmd: full executable path
    """
  end

  schema do
    field :filter, :string, default: nil
  end

  @impl true
  def execute(params, frame) do
    filter = Map.get(params, :filter)

    case list_processes(filter) do
      {:ok, processes} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{"processes" => processes})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to list processes: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp list_processes(filter) do
    try do
      {stdout, exit_code} = System.cmd("ps", ["aux"], stderr_to_stdout: true)

      if exit_code != 0 do
        {:error, "ps failed (exit #{exit_code}): #{stdout}"}
      else
        processes =
          stdout
          |> String.split("\n")
          |> Enum.drop(1)
          |> Enum.reject(&(&1 == ""))
          |> Enum.map(&parse_ps_line/1)
          |> Enum.reject(&is_nil/1)
          |> maybe_filter(filter)

        {:ok, processes}
      end
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  defp maybe_filter(processes, nil), do: processes
  defp maybe_filter(processes, filter) do
    filter_lower = String.downcase(filter)
    Enum.filter(processes, fn proc ->
      proc["command"] && String.contains?(String.downcase(proc["command"]), filter_lower)
    end)
  end

  defp parse_ps_line(line) do
    # Split into 11 parts: USER PID %CPU %MEM VSZ RSS TT STAT STARTED TIME COMMAND...
    parts = String.split(line, ~r/\s+/, parts: 11, trim: true)

    case parts do
      [_user, pid, cpu, mem, _vsz, _rss, _tt, _stat, _started, _time, command_col] ->
        cmd = command_col |> String.split(~r/\s+/, parts: 2) |> hd() |> String.trim()

        %{
          "pid"     => String.to_integer(pid),
          "cpu"     => cpu,
          "mem"     => mem,
          "command" => Path.basename(cmd)
        }

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

end
