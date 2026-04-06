defmodule Exhub.MCP.Tools.Desktop.ReadProcessOutput do
  @moduledoc """
  MCP Tool: read_process_output

  Read output from a previously started process.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.ProcessStore

  use Anubis.Server.Component, type: :tool

  def name, do: "read_process_output"

  @impl true
  def description do
    """
    Read output from a previously started process.

    Returns the stdout/stderr output from a process started with start_process.
    Use the offset parameter to read incrementally (for polling long-running
    commands). The process status and exit code are also returned.

    Processes are automatically cleaned up after 1 hour of inactivity.

    Parameters:
    - process_id: The process ID returned by start_process
    - offset: Line offset to start reading from (0 = from beginning, default: 0)
    - length: Maximum number of lines to return (default: 1000)
    """
  end

  schema do
    field(:process_id, {:required, :string}, description: "The process ID returned by start_process")
    field(:offset, :integer, description: "Line offset to start reading from (default 0)", default: 0)
    field(:length, :integer, description: "Maximum number of lines to return (default 1000)", default: 1000)
  end

  @impl true
  def execute(params, frame) do
    process_id = Map.get(params, :process_id)
    offset = Map.get(params, :offset, 0)
    length = Map.get(params, :length, 1000)

    case ProcessStore.get_output(process_id, offset) do
      {:ok, result} ->
        # Touch the process to reset its TTL
        ProcessStore.touch(process_id)

        # Slice to requested length
        lines = String.split(result.output, "\n")
        sliced = Enum.take(lines, length) |> Enum.join("\n")

        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "output" => sliced,
            "offset" => offset,
            "lines_returned" => min(length(lines), length),
            "total_lines" => result.total_lines,
            "status" => to_string(result.status),
            "exit_code" => result.exit_code,
            "has_more" => length(lines) > length or result.status == :running
          })

        {:reply, resp, frame}

      {:error, :not_found} ->
        resp =
          Response.tool()
          |> Response.error("Process not found: #{process_id}. Process may have expired (1 hour TTL).")

        {:reply, resp, frame}
    end
  end
end
