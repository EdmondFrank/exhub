defmodule Exhub.Router do
  alias Exhub.ProxyPlug
  @proxy Application.compile_env(:exhub, :proxy, "")
  require Logger
  use Plug.Router
  use PlugSocket

  plug Plug.Parsers,
        parsers: [:urlencoded, {:json, json_decoder: Jason}],
        pass:  ["*/*"]
  socket "/exhub", Exhub.SocketHandler
  plug :match
  plug :dispatch



  post "/groq/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.groq.com/openai/v1", client_options: [proxy: @proxy])
  end

  post "/google/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://generativelanguage.googleapis.com/", client_options: [proxy: @proxy])
  end

  post "/cohere/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.cohere.ai/compatibility/v1", client_options: [proxy: @proxy])
  end

  match _ do
    send_resp(conn, 200, "<html><head></head><body>Connect to WS enpoint at \"/exhub\"</body></html>")
  end
end
