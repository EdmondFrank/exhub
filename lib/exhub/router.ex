defmodule Exhub.Router do
  require Logger
  use Plug.Router
  use PlugSocket

  plug Plug.Parsers,
        parsers: [:json],
        pass:  ["application/json"],
        json_decoder: Jason
  socket "/exhub", Exhub.SocketHandler
  plug :match
  plug :dispatch

  get _ do
    send_resp(conn, 200, "<html><head></head><body>Connect to WS enpoint at \"/exhub\"</body></html>")
  end
end
