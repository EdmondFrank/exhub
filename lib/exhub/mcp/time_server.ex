defmodule Exhub.MCP.TimeServer do
  @moduledoc """
  MCP Server for time-related functionality.

  This server exposes two tools via the Model Context Protocol:
  1. get_current_time - Get current time in a specific IANA timezone
  2. convert_time - Convert time between two IANA timezones

  The server uses HTTP transport and can be accessed at the /time/mcp endpoint.
  """

  use Anubis.Server,
    name: "exhub-time-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Register the tool components
  component(Exhub.MCP.Tools.GetCurrentTime)
  component(Exhub.MCP.Tools.ConvertTime)

  @impl true
  def init(client_info, frame) do
    _ = client_info

    {:ok, frame}
  end
end
