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
        # Build search index for retrieve_tools
        index = Exhub.MCP.Hub.ToolSearch.build_index(tools)
        :ets.insert(:mcp_hub_search_index, {:index, index})

        existing = Enum.map(tools, fn tool ->
          server = Map.get(tool, "server", "unknown")
          name = Map.get(tool, "name", "")
          description = Map.get(tool, "description", "")
          input_schema = Map.get(tool, "inputSchema", %{}) || %{}

          %{
            name: "#{server}__#{name}",
            description: "[#{server}] #{description}",
            inputSchema: input_schema
          }
        end)

        retrieve_tools = %{
          name: "retrieve_tools",
          description: "Search for relevant tools across all connected MCP servers. Use natural language to describe what you want to accomplish.",
          inputSchema: %{
            type: "object",
            properties: %{
              query: %{type: "string", description: "Natural language description of what you want to accomplish"},
              limit: %{type: "integer", description: "Maximum number of tools to return (default: 5)"}
            },
            required: ["query"]
          }
        }

        [retrieve_tools | existing]

      {:error, reason} ->
        Logger.error("Failed to list tools: #{inspect(reason)}")
        []
    end
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

  defp do_handle_tool_call("retrieve_tools", arguments, frame) do
    query = Map.get(arguments, "query", "")
    limit = Map.get(arguments, "limit", 5)

    Logger.info("[MCP Hub] retrieve_tools called with query: #{query}")

    results = case :ets.lookup(:mcp_hub_search_index, :index) do
      [{:index, index}] ->
        Exhub.MCP.Hub.ToolSearch.search(index, query, limit: limit)

      [] ->
        # Fallback: rebuild index
        case Exhub.MCP.Hub.ClientManager.list_all_tools() do
          {:ok, tools} ->
            index = Exhub.MCP.Hub.ToolSearch.build_index(tools)
            :ets.insert(:mcp_hub_search_index, {:index, index})
            Exhub.MCP.Hub.ToolSearch.search(index, query, limit: limit)

          {:error, _} ->
            []
        end
    end

    formatted = Enum.map(results, fn result ->
      %{
        name: result["full_name"],
        description: result["description"],
        server: result["server"],
        inputSchema: result["input_schema"]
      }
    end)

    {:ok, %{tools: formatted, count: length(formatted)}, frame}
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
