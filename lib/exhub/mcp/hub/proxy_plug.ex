defmodule Exhub.MCP.Hub.ProxyPlug do
  @moduledoc """
  MCP Streamable HTTP proxy Plug for virtual routing.

  This plug exposes individual upstream MCP servers at custom routes
  (e.g., `/desktop-commander/mcp`), directly proxying to the Anubis.Client
  without the `{server}__` prefix transformation.

  ## Session Management

  Sessions are managed via ETS table `:mcp_hub_proxy_sessions` with
  the following structure:
  - Key: session_id (binary)
  - Value: %{client_pid: pid(), group: String.t(), created_at: DateTime.t()}

  ## Protocol Support

  This plug implements a subset of the MCP Streamable HTTP protocol:
  - `initialize` - Server info and capabilities exchange
  - `notifications/initialized` - Acknowledge initialization
  - `ping` - Health check
  - `tools/list` - List available tools
  - `tools/call` - Execute a tool

  ## Why Not Anubis.Server?

  Anubis.Server's `server_info()` and `server_capabilities()` callbacks are
  module-level and cannot be dynamically configured per-route. This plug
  bypasses that limitation by directly proxying to Anubis.Client instances.
  """

  @behaviour Plug

  import Plug.Conn
  require Logger

  @session_header "mcp-session-id"
  @session_table :mcp_hub_proxy_sessions

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, opts) do
    group = Keyword.fetch!(opts, :group)

    ensure_session_table()

    case Exhub.MCP.Hub.ClientManager.get_client_pid(group) do
      {:ok, client_pid} ->
        handle_request(conn, client_pid, group)

      {:error, reason} ->
        Logger.warning("[ProxyPlug] No client for group '#{group}': #{inspect(reason)}")

        conn
        |> put_resp_content_type("application/json")
        |> send_resp(404, Jason.encode!(%{error: "Upstream server '#{group}' not available", reason: inspect(reason)}))
    end
  end

  defp handle_request(conn, _client_pid, group) do
    client_ref = Exhub.MCP.Hub.ServerConfig.client_name(group)
    case conn.method do
      "POST" -> handle_post(conn, client_ref, group)
      "GET" -> handle_get(conn, client_ref, group)
      "DELETE" -> handle_delete(conn, group)
      _ -> send_method_not_allowed(conn)
    end
  end

  defp handle_post(conn, client_ref, group) do
    message = conn.body_params

    if message == %{} or is_nil(message) do
      send_jsonrpc_error(conn, -32700, "Parse error: empty or invalid body", nil)
    else
      session_id = get_or_create_session_id(conn)
      store_session(session_id, client_ref, group)

      case message do
        %{"method" => "initialize"} = req ->
          handle_initialize(conn, req, client_ref, session_id, group)

        %{"method" => "notifications/initialized"} ->
          conn
          |> put_resp_content_type("application/json")
          |> put_resp_header(@session_header, session_id)
          |> send_resp(202, "{}")

        %{"method" => "ping"} = req ->
          handle_ping(conn, req, client_ref, session_id)

        %{"method" => "tools/list"} = req ->
          handle_tools_list(conn, req, client_ref, session_id)

        %{"method" => "tools/call"} = req ->
          handle_tools_call(conn, req, client_ref, session_id)

        _ ->
          send_jsonrpc_error(conn, -32601, "Method not found", message["id"])
      end
    end
  end

  defp handle_initialize(conn, req, client_ref, session_id, group) do
    server_info = fetch_server_info(client_ref, group)
    capabilities = fetch_capabilities(client_ref)

    result = %{
      "protocolVersion" => "2024-11-05",
      "serverInfo" => server_info,
      "capabilities" => Map.merge(capabilities, %{"tools" => %{}})
    }

    response = %{
      "jsonrpc" => "2.0",
      "id" => req["id"],
      "result" => result
    }

    Logger.info("[ProxyPlug] #{group}: initialize from client, upstream: #{inspect(server_info)}")

    conn
    |> put_resp_content_type("application/json")
    |> put_resp_header(@session_header, session_id)
    |> send_resp(200, Jason.encode!(response))
  end

  defp handle_ping(conn, req, client_ref, session_id) do
    response = case Anubis.Client.ping(client_ref, timeout: 10_000) do
      :pong ->
        %{"jsonrpc" => "2.0", "id" => req["id"], "result" => %{}}

      {:error, reason} ->
        %{"jsonrpc" => "2.0", "id" => req["id"], "error" => %{"code" => -32603, "message" => inspect(reason)}}
    end

    conn
    |> put_resp_content_type("application/json")
    |> put_resp_header(@session_header, session_id)
    |> send_resp(200, Jason.encode!(response))
  end

  defp handle_tools_list(conn, req, client_ref, session_id) do
    response = case Anubis.Client.list_tools(client_ref, timeout: 30_000) do
      {:ok, %{result: result}} when is_map(result) ->
        tools = Map.get(result, "tools", [])
        %{"jsonrpc" => "2.0", "id" => req["id"], "result" => %{"tools" => tools}}

      {:error, reason} ->
        %{"jsonrpc" => "2.0", "id" => req["id"], "error" => %{"code" => -32603, "message" => inspect(reason)}}
    end

    conn
    |> put_resp_content_type("application/json")
    |> put_resp_header(@session_header, session_id)
    |> send_resp(200, Jason.encode!(response))
  end

  defp handle_tools_call(conn, req, client_ref, session_id) do
    tool_name = get_in(req, ["params", "name"])
    arguments = get_in(req, ["params", "arguments"]) || %{}

    response = case Anubis.Client.call_tool(client_ref, tool_name, arguments, timeout: 120_000) do
      {:ok, %{result: result}} ->
        %{"jsonrpc" => "2.0", "id" => req["id"], "result" => result}

      {:ok, result} when is_map(result) ->
        %{"jsonrpc" => "2.0", "id" => req["id"], "result" => result}

      {:error, reason} ->
        %{"jsonrpc" => "2.0", "id" => req["id"], "error" => %{"code" => -32603, "message" => inspect(reason)}}
    end

    conn
    |> put_resp_content_type("application/json")
    |> put_resp_header(@session_header, session_id)
    |> send_resp(200, Jason.encode!(response))
  end

  defp handle_get(conn, _client_ref, _group) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(405, Jason.encode!(%{error: "SSE streaming not supported on virtual routes"}))
  end

  defp handle_delete(conn, _group) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, "{}")
  end


  defp get_or_create_session_id(conn) do
    case get_req_header(conn, @session_header) do
      [session_id | _] when is_binary(session_id) and session_id != "" -> session_id
      _ -> generate_session_id()
    end
  end

  defp generate_session_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end

  defp ensure_session_table do
    try do
      :ets.new(@session_table, [:named_table, :public, :set, read_concurrency: true])
    rescue
      ArgumentError -> :ok
    end
  end

  defp store_session(session_id, client_ref, group) do
    :ets.insert(@session_table, {session_id, %{client_pid: client_ref, group: group, created_at: DateTime.utc_now()}})
  end

  defp fetch_server_info(client_ref, default_name) do
    try do
      case Anubis.Client.get_server_info(client_ref) do
        nil -> %{"name" => default_name, "version" => "1.0.0"}
        info when is_map(info) -> info
        _ -> %{"name" => default_name, "version" => "1.0.0"}
      end
    rescue
      _ -> %{"name" => default_name, "version" => "1.0.0"}
    catch
      _, _ -> %{"name" => default_name, "version" => "1.0.0"}
    end
  end

  defp fetch_capabilities(client_ref) do
    try do
      case Anubis.Client.get_server_capabilities(client_ref) do
        nil -> %{}
        caps when is_map(caps) -> caps
        _ -> %{}
      end
    rescue
      _ -> %{}
    catch
      _, _ -> %{}
    end
  end

  defp send_jsonrpc_error(conn, code, message, id) do
    response = %{"jsonrpc" => "2.0", "id" => id, "error" => %{"code" => code, "message" => message}}

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(400, Jason.encode!(response))
  end

  defp send_method_not_allowed(conn) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(405, Jason.encode!(%{error: "Method not allowed"}))
  end
end
