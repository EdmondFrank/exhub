defmodule Exhub.MCP.ExhubServer do
  @moduledoc """
  MCP Server for Exhub self-management.

  Exposes tools that allow AI agents to compile, hot-reload, restart, and
  introspect the running Exhub instance.

  ## Endpoints

  - `POST /exhub/mcp` — Streamable HTTP MCP endpoint

  ## Tools

  | Tool | Description |
  |------|-------------|
  | `exhub_hot_reload` | Zero-downtime BEAM code hot-reload |
  | `exhub_restart` | Schedule a soft or hard VM restart |
  | `exhub_get_status` | Runtime statistics (uptime, memory, processes) |
  | `exhub_get_version` | Version info for Exhub, Elixir, OTP, and ERTS |
  | `exhub_reload_keys` | Reload API keys from SecretVault |
  """

  use Anubis.Server,
    name: "exhub-self-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Management tools
  component Exhub.MCP.Tools.Exhub.HotReload
  component Exhub.MCP.Tools.Exhub.Restart
  component Exhub.MCP.Tools.Exhub.GetStatus
  component Exhub.MCP.Tools.Exhub.GetVersion
  # component Exhub.MCP.Tools.Exhub.ReloadKeys

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end

  @impl true
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end
