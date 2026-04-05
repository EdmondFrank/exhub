defmodule Exhub.MCP.Tools.Desktop.ListManagedProcesses do
  @moduledoc """
  MCP Tool: list_managed_processes

  List all processes currently being tracked by the Desktop MCP server.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.ProcessStore

  use Anubis.Server.Component, type: :tool

  def name, do: "list_managed_processes"

  @impl true
  def description do
    """
    List all processes currently being tracked by the Desktop MCP server.

    Returns a list of processes started with start_process, including their
    IDs, commands, status, and metadata. Processes are automatically cleaned
    up after 1 hour of inactivity.
    """
  end

  schema do
  end

  @impl true
  def execute(_params, frame) do
    processes =
      ProcessStore.list()
      |> Enum.map(fn entry ->
        %{
          "process_id" => entry.id,
          "command" => entry.command,
          "status" => to_string(entry.status),
          "exit_code" => entry.exit_code,
          "pid" => entry.pid,
          "started_at" => format_time(entry.started_at),
          "output_bytes" => byte_size(entry.output),
          "working_dir" => entry.working_dir
        }
      end)

    resp =
      Response.tool()
      |> Response.structured(%{
        "success" => true,
        "processes" => processes,
        "count" => length(processes)
      })

    {:reply, resp, frame}
  end

  defp format_time(timestamp) when is_integer(timestamp) do
    DateTime.from_unix!(div(timestamp, 1000), :millisecond)
    |> DateTime.to_iso8601()
  rescue
    _ -> to_string(timestamp)
  end

  defp format_time(other), do: to_string(other)
end
