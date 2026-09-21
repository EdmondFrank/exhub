defmodule Exhub.MCP.Tools.Hub.RetrieveTools do
  @moduledoc """
  MCP Tool: retrieve_tools

  Search for relevant tools across all connected MCP servers.
  Use natural language to describe what you want to accomplish.

  Two-stage retrieval:

    1. `Exhub.MCP.Hub.ToolSearch` (TF-IDF) pulls a wide candidate pool.
    2. `Exhub.MCP.Hub.ToolRelevance` (Smart Decide / System One) judges each
       candidate — one tool per request, concurrently — and keeps only the
       relevant ones, so callers receive far fewer irrelevant tool definitions.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Hub.ToolRelevance

  use Anubis.Server.Component, type: :tool

  def name, do: "retrieve_tools"

  @impl true
  def description do
    """
    Search for relevant tools across all connected MCP servers. Use natural language to describe what you want to accomplish.

    Results are filtered for relevance by the Smart Decide model (on by default), so fewer irrelevant tool definitions are returned. Set `filter: false` to skip filtering and get the raw TF-IDF ranking.
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

    field(:filter, :boolean,
      description:
        "Smart Decide relevance filtering (default: true). Set `false` for the raw TF-IDF ranking",
      default: true
    )
  end

  @impl true
  def execute(params, frame) do
    query = Map.get(params, :query, "")
    limit = Map.get(params, :limit, 5)
    filter? = filter?(Map.get(params, :filter))

    require Logger
    Logger.info("[MCP Hub] retrieve_tools called with query: #{query} (filter: #{filter?})")

    candidates =
      search_candidates(query, candidate_limit(limit, filter?))

    {results, stats} =
      if filter? do
        ToolRelevance.filter(query, candidates, ToolRelevance.config())
      else
        total = length(candidates)

        {candidates,
         %{
           candidates: total,
           relevant: total,
           errors: 0,
           excluded: 0,
           filtered: false,
           fallback: false
         }}
      end

    results = Enum.take(results, limit)

    formatted =
      Enum.map(results, fn result ->
        base = %{
          name: result["full_name"],
          server: result["server"],
          description: result["description"]
        }

        case compact_params(result["input_schema"]) do
          nil -> base
          params -> Map.put(base, :params, params)
        end
      end)

    resp =
      Response.tool()
      |> Response.structured(%{
        tools: formatted,
        count: length(formatted),
        filtered: stats.filtered,
        fallback: stats.fallback,
        candidates: stats.candidates
      })

    {:reply, resp, frame}
  end

  # `filter: false` skips the Smart Decide pass; absent (nil) follows the hub config.
  defp filter?(nil), do: ToolRelevance.enabled?()
  defp filter?(value), do: value == true

  # When filtering, widen the candidate pool so the model has enough to choose from.
  defp candidate_limit(limit, true) do
    config = ToolRelevance.config()
    max(limit, Keyword.get(config, :candidate_limit, limit))
  end

  defp candidate_limit(limit, false), do: limit

  defp search_candidates(query, limit) do
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
  end

  @doc """
  Reduces a full input schema to a single compact param summary line,
  e.g. `"query: string (required), limit: integer"`.

  Returns `nil` for tools that take no (or no documented) parameters.
  """
  def compact_params(schema) when is_map(schema) do
    properties = Map.get(schema, "properties", %{})
    required = schema |> Map.get("required", []) |> List.wrap() |> MapSet.new()

    if properties != %{} do
      properties
      |> Enum.sort_by(fn {name, _} -> name end)
      |> Enum.map(fn {name, spec} ->
        base = "#{name}: #{param_type(spec)}"

        if MapSet.member?(required, name) do
          base <> " (required)"
        else
          base
        end
      end)
      |> Enum.join(", ")
    end
  end

  def compact_params(_), do: nil

  defp param_type(spec) when is_map(spec) do
    case spec do
      %{"type" => type} when is_binary(type) -> type
      %{"type" => [type | _]} when is_binary(type) -> type
      _ -> "any"
    end
  end

  defp param_type(_), do: "any"
end
