defmodule Exhub.MCP.LazyPlug do
  @moduledoc """
  A lazy wrapper around `Anubis.Server.Transport.StreamableHTTP.Plug` that defers
  `init/1` to request time. This is necessary because `Plug.Router.forward/2` calls
  `init/1` at compile time, but the anubis plug reads from `:persistent_term` which
  is only populated when the server supervisor starts at runtime.
  """

  @behaviour Plug

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, opts) do
    plug_opts = Anubis.Server.Transport.StreamableHTTP.Plug.init(opts)
    Anubis.Server.Transport.StreamableHTTP.Plug.call(conn, plug_opts)
  end
end
