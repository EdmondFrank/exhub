defmodule Exhub.MCP.Tools.Exhub.GetStatus do
  @moduledoc """
  MCP Tool: exhub_get_status

  Returns runtime statistics for the running Exhub BEAM VM.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "exhub_get_status"

  @impl true
  def description do
    """
    Get runtime statistics for the running Exhub BEAM VM.

    Use this tool for health checks, capacity monitoring, or debugging performance
    issues. Returns key metrics about the VM's current state.

    Returns:
    - uptime_ms: How long the VM has been running (in milliseconds)
    - memory_bytes: Total memory used by the VM (in bytes)
    - process_count: Number of Erlang processes currently alive
    - atom_count: Number of atoms currently allocated
    - port_count: Number of open ports (file descriptors, sockets, etc.)

    High process counts may indicate process leaks. High memory usage could
    suggest large data structures or memory leaks. Atom count approaching the
    limit (default 1M) is a critical warning.

    No parameters required.
    """
  end

  schema do
    # No parameters required
  end

  @impl true
  def execute(_params, frame) do
    start_ms =
      :erlang.convert_time_unit(:erlang.system_info(:start_time), :native, :millisecond)

    result = %{
      uptime_ms: :erlang.monotonic_time(:millisecond) - start_ms,
      memory_bytes: :erlang.memory(:total),
      process_count: :erlang.system_info(:process_count),
      atom_count: :erlang.system_info(:atom_count),
      port_count: :erlang.system_info(:port_count)
    }

    resp = Response.tool() |> Helpers.toon_response(result)
    {:reply, resp, frame}
  end
end
