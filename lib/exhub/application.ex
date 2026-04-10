defmodule Exhub.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    load_secrets()

    children = [
      {Registry, keys: :unique, name: Exhub.Registry},
      {Exhub.Llm.World, name: Exhub.Llm.World},
      {Exhub.Llm.LlmConfigServer, name: Exhub.Llm.LlmConfigServer},
      {Exhub.Llm.Mcp.ServerManager, name: Exhub.Llm.Mcp.ServerManager},
      {Exhub.Llm.Mcp.ClientManager, name: Exhub.Llm.Mcp.ClientManager},
      Exhub.MacKeepAlive,
      Exhub.HealthCheck,
      {Exhub.MCP.HabitStore, name: Exhub.MCP.HabitStore},
      # MCP Habit Server — 24h session idle timeout to survive mcpproxy inactivity
      {Exhub.MCP.HabitServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Time Server
      {Exhub.MCP.TimeServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Think Server
      {Exhub.MCP.ThinkServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Web Tools Server
      {Exhub.MCP.WebToolsServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Archery Server (SQL audit platform)
      {Exhub.MCP.ArcheryServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Browser Server
      {Exhub.MCP.BrowserUseServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Image Generation Server
      {Exhub.MCP.ImageGenServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Document Extraction Server (PDF, DOCX, images via Gitee AI PaddleOCR-VL-1.5)
      {Exhub.MCP.DocExtractServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Todo Server (multi-tenant, 2-hour expiry)
      {Exhub.MCP.TodoStore, name: Exhub.MCP.TodoStore},
      {Exhub.MCP.TodoServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Desktop Server (filesystem, process, search tools)
      {Exhub.MCP.Desktop.ProcessStore, name: Exhub.MCP.Desktop.ProcessStore},
      {Exhub.MCP.DesktopServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Agent Server (ACP agent management)
      {Exhub.MCP.Agent.Store, name: Exhub.MCP.Agent.Store},
      {Exhub.MCP.AgentServer, transport: :streamable_http, request_timeout: 300_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Brain Server (Obsidian vault as second brain)
      {Exhub.MCP.BrainServer, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # MCP Hub - TaskSupervisor for non-blocking parallel client startup
      {Task.Supervisor, name: Exhub.MCP.Hub.TaskSupervisor},
      # MCP Hub - ClientManager for upstream server management
      {Exhub.MCP.Hub.ClientManager, name: Exhub.MCP.Hub.ClientManager},
      # MCP Hub Server - unified endpoint for all upstream MCP servers
      {Exhub.MCP.Hub.Server, transport: :streamable_http, request_timeout: 120_000, session_idle_timeout: 86_400_000 * 365},
      # Token Usage Tracking
      {Exhub.TokenUsage.TokenUsageStore, name: Exhub.TokenUsage.TokenUsageStore},
      cowboy_spec()
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Exhub.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp load_secrets do
    case SecretVault.Config.fetch_from_current_env(:exhub) do
      {:ok, config} ->
        SecretVault.Storage.to_persistent_term(config)

      {:error, reason} ->
        require Logger
        Logger.warning("SecretVault config not loaded: #{inspect(reason)}")
        :ok
    end
  end

  defp cowboy_spec do
    Plug.Cowboy.child_spec(
      scheme: :http,
      plug: Exhub.Router,
      options: [port: port(), dispatch: dispatch(), protocol_options: [idle_timeout: 1_800_000]]
    )
  end

  defp dispatch, do: PlugSocket.plug_cowboy_dispatch(Exhub.Router)
  defp port, do: Application.get_env(:bifrost, :port, 9069)
end
