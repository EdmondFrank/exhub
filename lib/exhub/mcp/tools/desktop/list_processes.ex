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

    Returns a list of processes with PID, CPU%, MEM%, and command. Uses `ps aux` on Unix/macOS.

    Parameters:
      - filter: optional regexp to filter by command (e.g. "ruby|rails", "^python")
      - limit: maximum number of results to return (default 20, 0 = unlimited)

    Returns per process: pid, cpu, mem, command.
    """
  end

  schema do
    field :filter, :string, default: nil
    field :limit, :integer, default: 20
  end

  @impl true
  def execute(params, frame) do
    filter = Map.get(params, :filter)
    limit  = Map.get(params, :limit, 20)

    case list_processes(filter) do
      {:ok, processes} ->
        limited = if limit > 0, do: Enum.take(processes, limit), else: processes

        resp =
          Response.tool()
          |> Helpers.toon_response(%{"processes" => limited})

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
         |> filter_processes(filter)

      {:ok, processes}
      end
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  defp filter_processes(list, nil), do: list

  defp filter_processes(list, pattern) do
    case Regex.compile(pattern, [:caseless]) do
      {:ok, re} -> Enum.filter(list, &Regex.match?(re, &1["command"]))
      {:error, _} -> list
    end
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
