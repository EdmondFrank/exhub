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

    Returns uptime, memory usage, process count, atom count, and port count.
    Useful for health checks and capacity monitoring.
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
