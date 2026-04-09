defmodule Exhub.MCP.Hub.Server do
  @moduledoc """
  MCP Hub Server that provides a unified endpoint aggregating all upstream MCP servers.

  This server uses Anubis.Server and exposes tools from all configured upstream
  MCP servers with the naming convention `{server}__{tool}`.

  ## Tool Naming

  - Original tool name: `read_file`
  - Server name: `desktop-commander`
  - Hub exposed name: `desktop-commander__read_file`
  - Description prefix: `[desktop-commander] Read file contents`

  ## Usage

  The server is accessible via HTTP Streamable transport at `/mcp-hub/mcp`.
  """

  use Anubis.Server,
    name: "exhub-mcp-hub",
    version: "1.0.0",
    capabilities: [:tools]

  require Logger

  @impl Anubis.Server
  def init(client_info, frame) do
    Logger.info("MCP Hub client connected: #{inspect(client_info)}")
    {:ok, frame}
  end

  def tools(_frame) do
    case Exhub.MCP.Hub.ClientManager.list_all_tools() do
      {:ok, tools} ->
        Enum.map(tools, fn tool ->
          server = Map.get(tool, "server", "unknown")
          name = Map.get(tool, "name", "")
          description = Map.get(tool, "description", "")
          input_schema = Map.get(tool, "inputSchema", %{})

          %{
            name: "#{server}__#{name}",
            description: "[#{server}] #{description}",
            inputSchema: input_schema
          }
        end)

      {:error, reason} ->
        Logger.error("Failed to list tools: #{inspect(reason)}")
        []
    end
  end

  @impl Anubis.Server
  def handle_tool_call(tool_name, arguments, frame) do
    case String.split(tool_name, "__", parts: 2) do
      [server_name, actual_tool_name] ->
        Logger.info("Routing tool call: #{tool_name} -> #{server_name}:#{actual_tool_name}")

        case Exhub.MCP.Hub.ClientManager.call_tool(
          server_name,
          actual_tool_name,
          arguments
        ) do
          {:ok, result} ->
            {:ok, result, frame}

          {:error, reason} ->
            error_msg = "Tool call failed on #{server_name}: #{inspect(reason)}"
            Logger.error(error_msg)
            {:error, %{message: error_msg}, frame}
        end

      _ ->
        error_msg = "Invalid tool name format: #{tool_name}. Expected: server__tool"
        Logger.error(error_msg)
        {:error, %{message: error_msg}, frame}
    end
  end
end
