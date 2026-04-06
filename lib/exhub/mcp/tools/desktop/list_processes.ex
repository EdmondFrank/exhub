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

    Returns a list of processes with their PID, command name, CPU usage,
    and memory usage. Uses `ps aux` on Unix/macOS via Exile.
    """
  end

  schema do
  end

  @impl true
  def execute(_params, frame) do
    case list_processes() do
      {:ok, processes} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "processes" => processes,
            "count" => length(processes)
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to list processes: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp list_processes do
    try do
      {stdout, stderr, exit_code} =
        Exile.stream(["ps", "aux"], stderr: :consume)
        |> Enum.reduce({"", "", 0}, fn
          {:stdout, data}, {out, err, code} -> {out <> data, err, code}
          {:stderr, data}, {out, err, code} -> {out, err <> data, code}
          {:exit, {:status, code}}, {out, err, _} -> {out, err, code}
          {:exit, :epipe}, {out, err, _} -> {out, err, 0}
          _, acc -> acc
        end)

      if exit_code == 0 do
        processes =
          stdout
          |> String.split("\n")
          |> Enum.drop(1)
          |> Enum.reject(&(&1 == ""))
          |> Enum.map(&parse_ps_line/1)
          |> Enum.reject(&is_nil/1)

        {:ok, processes}
      else
        {:error, "ps command failed (exit #{exit_code}): #{stderr}"}
      end
    rescue
      e -> {:error, Exception.message(e)}
    end
  end

  defp parse_ps_line(line) do
    parts = String.split(line, ~r/\s+/, trim: true)

    case parts do
      [_user, pid, cpu, mem | rest] ->
        %{
          "pid" => String.to_integer(pid),
          "cpu" => cpu,
          "memory" => mem,
          "command" => Enum.join(rest, " ")
        }

      _ ->
        nil
    end
  rescue
    _ -> nil
  end
end
