defmodule Exhub.ProxyPlug do
  @doc """
  reverse conn proxy to upstream
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
        IO.inspect({header, value})
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

        pre_body == "" &&
          Plug.Conn.get_req_header(conn, "content-type") == [
          "application/x-www-form-urlencoded"
        ] ->
        Plug.Conn.Query.encode(conn.body_params)

      true ->
        pre_body
    end

    conn
    |> Map.put(:path_info, conn.path_params["path"])
    |> ReverseProxyPlug.request(body, params)
    |> ReverseProxyPlug.response(conn, params)
  end
end
