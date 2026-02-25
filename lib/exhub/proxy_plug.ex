defmodule Exhub.ProxyPlug do
  @moduledoc """
  Reverse proxy plug for forwarding requests to upstream servers.
  Handles request/response transformation and error handling.
  """

  require Logger

  @doc """
  Forward connection to upstream server with proper error handling.
  """
  def forward_upstream(conn, upstream, opts \\ []) do
    params =
      ReverseProxyPlug.init(
        upstream: upstream,
        response_mode: Keyword.get(opts, :response_mode, :stream),
        client_options: Keyword.get(opts, :client_options, []),
        preserve_host_header: Keyword.get(opts, :preserve_host_header, false)
      )

    custom_headers = Keyword.get(opts, :custom_headers, [])

    conn =
      Enum.reduce(custom_headers, conn, fn {header, value}, conn ->
        # Ensure headers are lowercase for consistency
        header = String.downcase(header)

        # Remove existing header if it exists
        conn = Plug.Conn.delete_req_header(conn, header)

        # Add the new header
        Plug.Conn.put_req_header(conn, header, value)
      end)

    # If conn.req_body is already set (from router), use it, else read as usual
    {pre_body, conn} =
      case Map.get(conn, :req_body) do
        nil -> ReverseProxyPlug.read_body(conn)
        body -> {body, conn}
      end

    body =
      cond do
      pre_body == "" && Plug.Conn.get_req_header(conn, "content-type") == ["application/json"] ->
        if List.first(conn.path_info) == "cohere" do
          Jason.encode!(Map.delete(conn.body_params, "n"))
        else
          Jason.encode!(conn.body_params)
        end

        case conn.body_params do
          %{"model" => "deepseek-v3.2"} ->
            Jason.encode!(
              conn.body_params
              |> Map.put("think", false)
              |> Map.put("max_tokens", 8192)
              |> Map.put("thinking_budget", 4096)
            )
          %{"model" => "minimax-m2.1", "messages" => messages} ->
            transformed_messages =
              Enum.map(messages, fn message ->
                if message["role"] == "system" do
                  Map.put(message, "role", "user")
                else
                  message
                end
              end)

            Jason.encode!(Map.put(conn.body_params, "messages", transformed_messages))
          %{"model" => "kimi-k2.5"} ->
            conn.body_params
            |> Map.put("temperature", 1)
            |> Jason.encode!

          _ ->
            Jason.encode!(conn.body_params)
        end

        pre_body == "" &&
            Plug.Conn.get_req_header(conn, "content-type") == ["application/x-www-form-urlencoded"] ->
          Plug.Conn.Query.encode(conn.body_params)

        true ->
          pre_body
      end

    conn
    |> Map.put(:path_info, conn.path_params["path"])
    |> ReverseProxyPlug.request(body, params)
    |> tap(fn resp ->
      case resp do
        {:ok, %{status: status, body: resp_body}} when status < 200 or status >= 300 ->
          Logger.warning(
            "Upstream request failed: status=#{status}, path=#{conn.request_path}, body=#{inspect(resp_body)}"
          )

        {:error, reason} ->
          Logger.error("Upstream request error: path=#{conn.request_path}, reason=#{inspect(reason)}")

        _ ->
          :ok
      end
    end)
    |> ReverseProxyPlug.response(conn, params)
  end
end
