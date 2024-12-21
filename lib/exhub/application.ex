defmodule Exhub.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: Exhub.Registry},
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
      options: [port: port(), dispatch: dispatch()])
  end


  defp dispatch, do: PlugSocket.plug_cowboy_dispatch(Exhub.Router)
  defp port, do: Application.get_env(:bifrost, :port, 9069)
end
