defmodule Exhub.MCP.Tools.WebSearch do
  @moduledoc """
  MCP Tool for searching the web for information.

  This tool uses webscout to perform web searches and returns results
  with titles, URLs, and snippets.
  """

  alias Hermes.Server.Response

  use Hermes.Server.Component, type: :tool

  def name, do: "web_search"

  def description do
    """
    Search the web for information. Returns search results with titles, URLs, and snippets.
    """
  end

  schema do
    field(:query, {:required, :string}, description: "The search query")
    field(:count, :number, description: "Number of results to return (1-50, default 10)")
    field(:summary, :boolean, description: "Enable AI summary of results")

    field(:freshness, :string,
      description:
        "Filter by freshness - \"noLimit\", \"oneDay\", \"oneWeek\", \"oneMonth\", \"oneYear\""
    )
  end

  @impl true
  def execute(params, frame) do
    query = Map.get(params, :query)
    count = Map.get(params, :count, 10)
    summary = Map.get(params, :summary, false)
    freshness = Map.get(params, :freshness, "noLimit")

    # Validate parameters
    cond do
      is_nil(query) or query == "" ->
        resp = Response.tool() |> Response.error("Query is required")
        {:reply, resp, frame}

      true ->
        perform_search(query, count, summary, freshness, frame)
    end
  end

  # Private functions

  defp perform_search(query, count, summary, freshness, frame) do
    proxy = Application.get_env(:exhub, :proxy, "")
    env = if proxy != "", do: [{"HTTPS_PROXY", proxy}], else: []

    args =
      ["-m", "webscout", "text", "-k", query] ++
        ["-n", to_string(count)] ++
        maybe_add_flag(summary, "--summary") ++
        maybe_add_freshness(freshness)

    case System.cmd("python", args, env: env) do
      {output, 0} ->
        results = parse_results(output)
        resp = Response.tool() |> Response.structured(results)
        {:reply, resp, frame}

      {error, exit_code} ->
        resp = Response.tool() |> Response.error("Search failed (exit #{exit_code}): #{error}")
        {:reply, resp, frame}
    end
  end

  defp maybe_add_flag(true, flag), do: [flag]
  defp maybe_add_flag(false, _flag), do: []
  defp maybe_add_flag(nil, _flag), do: []

  defp maybe_add_freshness("noLimit"), do: []
  defp maybe_add_freshness(freshness), do: ["--freshness", freshness]

  defp parse_results(output) do
    case Jason.decode(output) do
      {:ok, results} when is_list(results) ->
        %{
          "success" => true,
          "results" => Enum.map(results, &format_result/1),
          "count" => length(results)
        }

      {:ok, result} when is_map(result) ->
        %{
          "success" => true,
          "results" => [format_result(result)],
          "count" => 1
        }

      {:error, _} ->
        # If not valid JSON, return raw output
        %{
          "success" => true,
          "results" => [%{"raw" => output}],
          "count" => 1
        }
    end
  end

  defp format_result(result) do
    %{
      "title" => Map.get(result, "title", ""),
      "url" => Map.get(result, "url", Map.get(result, "link", "")),
      "snippet" => Map.get(result, "snippet", Map.get(result, "description", ""))
    }
  end
end
