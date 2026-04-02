defmodule Exhub.MCP.LazyPlug do
  @moduledoc """
  A lazy wrapper around `Anubis.Server.Transport.StreamableHTTP.Plug` that defers
  `init/1` to request time. This is necessary because `Plug.Router.forward/2` calls
  `init/1` at compile time, but the anubis plug reads from `:persistent_term` which
  is only populated when the server supervisor starts at runtime.

  Additionally, this plug implements auto-session creation for MCP requests.
  When a POST request contains a session ID header but the session doesn't exist,
  it will automatically create and register the session.
  """

  @behaviour Plug

  require Logger

  @session_header "mcp-session-id"

  @impl Plug
  def init(opts), do: opts

  @impl Plug
  def call(conn, opts) do
    plug_opts = Anubis.Server.Transport.StreamableHTTP.Plug.init(opts)

    with {:ok, server} <- Keyword.fetch(opts, :server) do
      Logger.info("[MCP LazyPlug] #{conn.method} #{conn.request_path} - server: #{server}")

      # Auto-create session for any POST request with session ID header
      with {:post, true} <- {:post, conn.method == "POST"},
           {:ok, session_id} <- get_session_id(conn),
           {:missing, true} <- {:missing, session_missing?(server, session_id)} do
        Logger.info("[MCP LazyPlug] Auto-creating session: #{session_id}")
        auto_create_session(server, session_id)
      else
        {:post, false} ->
          :ok

        {:ok, session_id} ->
          Logger.debug("[MCP LazyPlug] Session ID found: #{session_id}")
          :ok

        {:missing, false} ->
          :ok

        _ ->
          :ok
      end
    end

    Anubis.Server.Transport.StreamableHTTP.Plug.call(conn, plug_opts)
  end

  defp get_session_id(conn) do
    case Plug.Conn.get_req_header(conn, @session_header) do
      [session_id | _] when is_binary(session_id) and session_id != "" -> {:ok, session_id}
      _ -> :error
    end
  end

  defp session_missing?(server, session_id) do
    session_config = Anubis.Server.Supervisor.get_session_config(server)
    registry_mod = session_config.registry_mod
    registry_name = Anubis.Server.Registry.registry_name(server)

    case registry_mod.lookup_session(registry_name, session_id) do
      {:ok, _pid} -> false
      {:error, :not_found} -> true
    end
  rescue
    _ -> true
  end

  defp auto_create_session(server, session_id) do
    supervisor_mod = Anubis.Server.Supervisor
    session_config = supervisor_mod.get_session_config(server)
    registry_mod = session_config.registry_mod
    registry_name = Anubis.Server.Registry.registry_name(server)
    session_name = Anubis.Server.Registry.session_name(server, session_id)

    session_opts = [
      session_id: session_id,
      server_module: server,
      name: session_name,
      transport: session_config.transport,
      session_idle_timeout: session_config.session_idle_timeout,
      timeout: session_config.timeout,
      task_supervisor: session_config.task_supervisor
    ]

    case supervisor_mod.start_session(server, session_opts) do
      {:ok, pid} ->
        case registry_mod.register_session(registry_name, session_id, pid) do
          :ok ->
            Logger.info("[MCP LazyPlug] Auto-created MCP session #{session_id} for server #{server}")
            # Mark session as initialized so it can accept any request
            mark_session_initialized(pid, session_id)

          {:error, reason} ->
            Logger.error("[MCP LazyPlug] Failed to register session #{session_id}: #{inspect(reason)}")
        end

      {:error, {:already_started, pid}} ->
        Logger.warning("[MCP LazyPlug] Session #{session_id} already started, registering existing pid")
        registry_mod.register_session(registry_name, session_id, pid)

      {:error, reason} ->
        Logger.error("[MCP LazyPlug] Failed to start session #{session_id}: #{inspect(reason)}")
    end
  rescue
    error ->
      Logger.error("[MCP LazyPlug] Exception in auto_create_session for #{session_id}: #{inspect(error)}")
  end

  defp mark_session_initialized(session_pid, session_id) do
    # Send the notifications/initialized notification to mark session as initialized
    # This allows the session to accept any request without the full handshake
    init_notification = %{
      "jsonrpc" => "2.0",
      "method" => "notifications/initialized"
    }

    GenServer.cast(session_pid, {:mcp_notification, init_notification, %{}})
    Logger.debug("[MCP LazyPlug] Marked session #{session_id} as initialized")
  rescue
    error ->
      Logger.warning("[MCP LazyPlug] Failed to mark session #{session_id} as initialized: #{inspect(error)}")
  end
end
