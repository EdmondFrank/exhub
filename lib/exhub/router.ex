defmodule Exhub.Router do
  alias Exhub.ProxyPlug
  @proxy Application.compile_env(:exhub, :proxy, "")
  require Logger
  use Plug.Router
  use PlugSocket

  @model_targets %{
    "qwen3-235b-a22b" => Application.compile_env(:exhub, :giteeai_api_base),
    "gemini-2.5-pro" => Application.compile_env(:exhub, :gemini_api_base),
    "gemini-2.5-flash" => Application.compile_env(:exhub, :gemini_api_base)
  }

  @model_tokens %{
    "qwen3-235b-a22b" => Application.compile_env(:exhub, :giteeai_api_key),
    "gemini-2.5-pro" => Application.compile_env(:exhub, :gemini_api_key),
    "gemini-2.5-flash" => Application.compile_env(:exhub, :gemini_api_key)
  }


  plug Plug.Parsers,
    parsers: [:urlencoded, {:json, json_decoder: Jason}],
    pass:  ["*/*"]
  socket "/exhub", Exhub.SocketHandler
  plug :match
  plug :dispatch



  @doc """
  Helper function to handle proxy routing with consistent options
  """
  @spec proxy_route(Plug.Conn.t(), String.t()) :: Plug.Conn.t()
  defp proxy_route(conn, upstream) do
    ProxyPlug.forward_upstream(conn, upstream,
      client_options: [proxy: @proxy]
    )
  end

  post "/groq/v1/*path" do
    proxy_route(conn, "https://api.groq.com/openai/v1")
  end

  post "/google/v1/*path" do
    proxy_route(conn, "https://generativelanguage.googleapis.com/")
  end

  post "/cohere/v1/*path" do
    proxy_route(conn, "https://api.cohere.ai/compatibility/v1")
  end

  post "/samba/v1/*path" do
    proxy_route(conn, "https://api.sambanova.ai/v1")
  end

  post "/anthropic/v1/*path" do
    proxy_route(conn, "https://api.anthropic.com/v1")
  end

  match _ do
    send_resp(conn, 200, "<html><head></head><body>Connect to WS enpoint at \"/exhub\"</body></html>")
  end

  post "/openai/v1/*path" do
    model_name = get_model_from_request(conn)
    target_url = model_name |> get_target_url() |> validate_url()
    token = get_token_for_model(model_name)

    ProxyPlug.forward_upstream(
      conn,
      target_url,
      custom_headers: [{"Authorization", "Bearer #{token}"}]
    )
  end

  @doc """
  Extracts the model name from the request body parameters.
  Returns nil if the content-type is not application/json or if model is not specified.
  """
  @spec get_model_from_request(Plug.Conn.t()) :: String.t() | nil
  defp get_model_from_request(conn) do
    case Plug.Conn.get_req_header(conn, "content-type") do
      ["application/json" <> _] -> conn.body_params["model"]
      _ -> nil
    end
  end

  defp get_target_url(nil), do: Application.get_env(:exhub, :openai_api_base, "http://localhost:4444/v1")
  defp get_target_url(model), do: @model_targets[model] || Application.get_env(:exhub, :openai_api_base)

  defp get_token_for_model(nil), do: Application.get_env(:exhub, :openai_api_key, "")
  defp get_token_for_model(model), do: @model_tokens[model] || Application.get_env(:exhub, :openai_api_key)

  defp validate_url(url) when is_binary(url), do: url
  defp validate_url(_), do: raise(ArgumentError, "Invalid target URL")
end
