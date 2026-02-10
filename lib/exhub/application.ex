defmodule Exhub.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: Exhub.Registry},
      {Exhub.Llm.World, name: Exhub.Llm.World},
      {Exhub.Llm.LlmConfigServer, name: Exhub.Llm.LlmConfigServer},
      {Exhub.Llm.Mcp.ServerManager, name: Exhub.Llm.Mcp.ServerManager},
      {Exhub.Llm.Mcp.ClientManager, name: Exhub.Llm.Mcp.ClientManager},
      Exhub.MacKeepAlive,
      Exhub.HealthCheck,
      Hermes.Server.Registry,
      # MCP Habit Configuration Server
      {Exhub.MCP.HabitStore, name: Exhub.MCP.HabitStore},
      {Exhub.MCP.HabitServer, transport: :streamable_http},
      cowboy_spec()
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Exhub.Supervisor]
    Supervisor.start_link(children, opts)
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
