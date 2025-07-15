defmodule Exhub.Router do
  alias Exhub.ProxyPlug
  require Logger
  use Plug.Router
  use PlugSocket

  @proxy Application.compile_env(:exhub, :proxy, "")
  @default_timeout Application.compile_env(:exhub, :default_timeout, 1_800_000)

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

  post "/samba/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.sambanova.ai/v1", client_options: [proxy: @proxy])
  end

  post "/openai/v1/*path" do
    # Extract model name from JSON body (if present)
    model_name =
      case Plug.Conn.get_req_header(conn, "content-type") do
        ["application/json" <> _] ->
          case conn.body_params do
            %{"model" => model} -> model
            _ -> nil
          end
        _ -> nil
      end

    # Model to target mapping (extendable)
    model_target_map = %{
      "kimi-k2-instruct" => "https://ai.gitee.com/v1",
      "qwen3-235b-a22b" => "https://ai.gitee.com/v1",
      "gemini-2.5-pro" => "http://localhost:8765/v1",
      "gemini-2.5-flash" => "http://localhost:8765/v1"
    }

    # Model to token mapping (extendable)
    model_token_map = %{
      "qwen3-235b-a22b" => Application.get_env(:exhub, :giteeai_api_key, ""),
      "kimi-k2-instruct" => Application.get_env(:exhub, :giteeai_api_key, "")
    }

    target_url = Map.get(model_target_map, model_name, "http://localhost:4444/v1")

    token = Map.get(model_token_map, model_name, Application.get_env(:exhub, :openai_api_key, ""))

    options = [custom_headers: [{"Authorization", "Bearer #{token}"}], client_options: [timeout: @default_timeout, recv_timeout: @default_timeout]]

    ProxyPlug.forward_upstream(
      conn,
      target_url,
      options
    )
  end

  post "/anthropic/v1/*path" do
    ProxyPlug.forward_upstream(conn, "https://api.anthropic.com/v1", client_options: [proxy: @proxy])
  end

  match _ do
    send_resp(conn, 200, "<html><head></head><body>Connect to WS enpoint at \"/exhub\"</body></html>")
  end
end
