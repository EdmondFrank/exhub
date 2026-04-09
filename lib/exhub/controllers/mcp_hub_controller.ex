defmodule Exhub.Controllers.MCPHubController do
  @moduledoc """
  HTTP API controller for managing MCP Hub upstream servers.

  Provides REST endpoints for:
  - Listing servers and their status
  - Adding new upstream servers
  - Updating server configuration
  - Removing servers
  - Toggling server enable/disable state
  - Listing aggregated tools
  """

  import Plug.Conn
  require Logger

  alias Exhub.MCP.Hub.ClientManager

  @doc """
  Lists all configured servers with their current status.

  GET /mcp-hub/servers
  """
  def list_servers(conn) do
    servers = ClientManager.list_servers()

    conn
    |> put_resp_content_type("application/json")
    |> send_resp(200, Jason.encode!(%{servers: servers}))
  end

  @doc """
  Adds a new upstream server.

  POST /mcp-hub/servers

  Request body:
      {
        "name": "my-server",
        "transport": "stdio",
        "command": "npx",
        "args": ["-y", "package-name"],
        "enabled": true
      }
  """
  def add_server(conn) do
    config_data = conn.body_params

    case ClientManager.add_server(config_data) do
      {:ok, config} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(201, Jason.encode!(%{
          success: true,
          message: "Server added successfully",
          server: server_to_json(config)
        }))

      {:error, :name_required} ->
        send_error(conn, 400, "Server name is required")

      {:error, :server_already_exists} ->
        send_error(conn, 409, "Server with this name already exists")

      {:error, :command_required_for_stdio} ->
        send_error(conn, 400, "Command is required for stdio transport")

      {:error, :url_required} ->
        send_error(conn, 400, "URL is required for this transport type")

      {:error, reason} ->
        send_error(conn, 500, "Failed to add server: #{inspect(reason)}")
    end
  end

  @doc """
  Gets a specific server's status.

  GET /mcp-hub/servers/:name
  """
  def get_server(conn, name) do
    case ClientManager.get_server_status(name) do
      nil ->
        send_error(conn, 404, "Server not found")

      server ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(%{server: server}))
    end
  end

  @doc """
  Updates a server's configuration.

  PUT /mcp-hub/servers/:name
  """
  def update_server(conn, name) do
    config_data = conn.body_params

    case ClientManager.update_server(name, config_data) do
      {:ok, config} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(%{
          success: true,
          message: "Server updated successfully",
          server: server_to_json(config)
        }))

      {:error, :server_not_found} ->
        send_error(conn, 404, "Server not found")

      {:error, :name_required} ->
        send_error(conn, 400, "Server name is required")

      {:error, :command_required_for_stdio} ->
        send_error(conn, 400, "Command is required for stdio transport")

      {:error, :url_required} ->
        send_error(conn, 400, "URL is required for this transport type")

      {:error, reason} ->
        send_error(conn, 500, "Failed to update server: #{inspect(reason)}")
    end
  end

  @doc """
  Removes a server.

  DELETE /mcp-hub/servers/:name
  """
  def remove_server(conn, name) do
    case ClientManager.remove_server(name) do
      :ok ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(%{
          success: true,
          message: "Server removed successfully"
        }))

      {:error, reason} ->
        send_error(conn, 500, "Failed to remove server: #{inspect(reason)}")
    end
  end

  @doc """
  Toggles a server's enabled state.

  POST /mcp-hub/servers/:name/toggle

  Request body:
      {"enabled": false}
  """
  def toggle_server(conn, name) do
    enabled = conn.body_params["enabled"]

    if is_nil(enabled) do
      send_error(conn, 400, "Missing 'enabled' field in request body")
    else
      case ClientManager.toggle_server(name, enabled) do
        {:ok, config} ->
          conn
          |> put_resp_content_type("application/json")
          |> send_resp(200, Jason.encode!(%{
            success: true,
            message: "Server #{if enabled, do: "enabled", else: "disabled"} successfully",
            server: server_to_json(config)
          }))

        {:error, :server_not_found} ->
          send_error(conn, 404, "Server not found")

        {:error, reason} ->
          send_error(conn, 500, "Failed to toggle server: #{inspect(reason)}")
      end
    end
  end

  @doc """
  Lists all available tools from all connected servers.

  GET /mcp-hub/tools
  """
  def list_tools(conn) do
    case ClientManager.list_all_tools() do
      {:ok, tools} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(%{tools: tools}))

      {:error, reason} ->
        send_error(conn, 500, "Failed to list tools: #{inspect(reason)}")
    end
  end

  # Private functions

  defp send_error(conn, status, message) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Jason.encode!(%{success: false, error: message}))
  end

  defp server_to_json(config) do
    %{
      name: config.name,
      transport: config.transport,
      enabled: config.enabled,
      command: config.command,
      args: config.args,
      env: config.env,
      url: config.url,
      headers: config.headers,
      expose_route: config.expose_route,
      created_at: DateTime.to_iso8601(config.created_at),
      updated_at: DateTime.to_iso8601(config.updated_at)
    }
  end
end
