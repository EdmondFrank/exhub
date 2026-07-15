defmodule Exhub.MCP.Tools.Hub.RetrieveTools do
  @moduledoc """
  MCP Tool: retrieve_tools

  Search for relevant tools across all connected MCP servers.
  Use natural language to describe what you want to accomplish.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "retrieve_tools"

  @impl true
  def description do
    """
    Search for relevant tools across all connected MCP servers. Use natural language to describe what you want to accomplish.
    """
  end

  schema do
    field(:query, :string,
      description: "Natural language description of what you want to accomplish",
      required: true
    )

    field(:limit, :integer,
      description: "Maximum number of tools to return (default: 5)",
      default: 5
    )
  end

  @impl true
  def execute(params, frame) do
    query = Map.get(params, :query, "")
    limit = Map.get(params, :limit, 10)

    require Logger
    Logger.info("[MCP Hub] retrieve_tools called with query: #{query}")

    results =
      case Exhub.MCP.Hub.Store.get_search_index() do
        [{:index, index}] ->
          Exhub.MCP.Hub.ToolSearch.search(index, query, limit: limit)

        [] ->
          # Fallback: rebuild index
          case Exhub.MCP.Hub.ClientManager.list_all_tools() do
            {:ok, tools} ->
              index = Exhub.MCP.Hub.ToolSearch.build_index(tools)
              Exhub.MCP.Hub.Store.put_search_index(index)
              Exhub.MCP.Hub.ToolSearch.search(index, query, limit: limit)

            {:error, _} ->
              []
          end
      end

    formatted =
      Enum.map(results, fn result ->
        %{
          name: result["full_name"],
          description: result["description"],
          server: result["server"],
          inputSchema: result["input_schema"]
        }
      end)

    resp = Response.tool() |> Response.structured(%{tools: formatted, count: length(formatted)})
    {:reply, resp, frame}
  end
end
