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

  # Register the retrieve_tools component
  component(Exhub.MCP.Tools.Hub.RetrieveTools)
  # Register the call_tools component
  component(Exhub.MCP.Tools.Hub.CallTools)

  require Logger

  @impl Anubis.Server
  def init(client_info, frame) do
    Logger.info("MCP Hub client connected: #{inspect(client_info)}")

    # Build search index for retrieve_tools asynchronously so we don't block init
    # if ClientManager is busy connecting to upstream servers
    Task.start(fn ->
      try do
        case Exhub.MCP.Hub.ClientManager.list_all_tools() do
          {:ok, tools} ->
            index = Exhub.MCP.Hub.ToolSearch.build_index(tools)
            Exhub.MCP.Hub.Store.put_search_index(index)
            Logger.info("MCP Hub search index built with #{length(tools)} tools")

          {:error, reason} ->
            Logger.error("Failed to build search index: #{inspect(reason)}")
        end
      rescue
        e -> Logger.error("Search index build crashed: #{inspect(e)}")
      end
    end)

    {:ok, frame}
  end

  @impl Anubis.Server
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end

  @impl Anubis.Server
  def handle_tool_call(tool_name, arguments, frame) do
    start_time = System.monotonic_time(:millisecond)
    Logger.info("[MCP Hub] Tool call initiated: #{tool_name}")

    result = do_handle_tool_call(tool_name, arguments, frame)

    duration = System.monotonic_time(:millisecond) - start_time
    Logger.info("[MCP Hub] Tool call completed: #{tool_name} in #{duration}ms")

    result
  end

  defp do_handle_tool_call(tool_name, arguments, frame) do
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
