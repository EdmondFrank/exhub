defmodule Exhub.MCP.Tools.WebSearch do
  @moduledoc """
  MCP Tool for searching the web for information using Gitee AI API.

  This tool uses Gitee AI's web search API to perform searches and returns results
  with titles, URLs, snippets, images, and videos.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.WebTools.Relevance

  use Anubis.Server.Component, type: :tool

  @giteeai_web_search_url "https://api.moark.com/v1/web-search"

  # The API caps `count` at 50.
  @max_search_count 50

  def name, do: "web_search"

  @impl true
  def description do
    """
    Search the web for information using AI-powered search.

    This tool performs a web search and returns relevant results including:
    - Web pages with titles, URLs, and summaries
    - Related images
    - Related videos

    Results are filtered for relevance by the Smart Decide model (on by default),
    so fewer irrelevant pages are returned. Set `filter: false` to skip filtering
    and get the raw search results.

    Use this tool when you need to find current information from the internet.
    """
  end

  schema do
    field(:query, {:required, :string}, description: "The search query string")
    field(:count, :integer, description: "Number of results to return (1-50, default 10)")

    field(:summary, :boolean,
      description: "Enable AI-generated summary of results (default false)"
    )

    field(:freshness, :string,
      description:
        "Filter results by freshness: noLimit (default), oneDay, oneWeek, oneMonth, oneYear"
    )

    field(:filter, :boolean,
      description:
        "Smart Decide relevance filtering (default: true). Set `false` for the raw search results",
      default: true
    )
  end

  @impl true
  def execute(params, frame) do
    query = Map.get(params, :query)
    count = Map.get(params, :count, 10)
    summary = Map.get(params, :summary, false)
    freshness = Map.get(params, :freshness, "noLimit")
    filter? = filter?(Map.get(params, :filter))

    # Validate parameters
    cond do
      is_nil(query) or query == "" ->
        resp = Response.tool() |> Response.error("Query is required")
        {:reply, resp, frame}

      true ->
        perform_search(query, count, summary, freshness, filter?, frame)
    end
  end

  # Private functions

  # `filter: false` skips the Smart Decide pass; absent (nil) follows config.
  defp filter?(nil), do: Relevance.enabled?()
  defp filter?(value), do: value == true

  # When filtering, widen the API pool so the model has enough to choose from,
  # then narrow it back to the caller's `count` after judging.
  defp request_count(count, true) do
    configured = Keyword.get(Relevance.config(), :candidate_limit, count)
    min(max(count, configured), @max_search_count)
  end

  defp request_count(count, false), do: count

  defp perform_search(query, count, summary, freshness, filter?, frame) do
    api_key = Application.get_env(:exhub, :giteeai_api_key, "")

    if api_key == "" do
      resp = Response.tool() |> Response.error("GiteeAI API key not configured")
      {:reply, resp, frame}
    else
      do_api_search(query, count, summary, freshness, filter?, api_key, frame)
    end
  end

  defp do_api_search(query, count, summary, freshness, filter?, api_key, frame) do
    body = %{
      query: query,
      count: request_count(count, filter?),
      summary: summary,
      freshness: freshness
    }

    headers = [
      {"Content-Type", "application/json"},
      {"Authorization", "Bearer #{api_key}"}
    ]

    case HTTPoison.post(
           @giteeai_web_search_url,
           Jason.encode!(body),
           headers,
           [recv_timeout: 25_000] ++ Exhub.TLSCompat.httpoison_opts(@giteeai_web_search_url)
         ) do
      {:ok, %HTTPoison.Response{status_code: 200, body: response_body}} ->
        case Jason.decode(response_body) do
          {:ok, response_data} ->
            formatted = format_search_response(query, response_data, count, filter?)
            resp = Response.tool() |> Response.text(formatted)
            {:reply, resp, frame}

          {:error, decode_error} ->
            resp =
              Response.tool()
              |> Response.error("Failed to decode response: #{inspect(decode_error)}")

            {:reply, resp, frame}
        end

      {:ok, %HTTPoison.Response{status_code: status_code, body: response_body}} ->
        resp =
          Response.tool()
          |> Response.error("API returned status #{status_code}: #{response_body}")

        {:reply, resp, frame}

      {:error, %HTTPoison.Error{reason: reason}} ->
        resp = Response.tool() |> Response.error("Web search request failed: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  @doc false
  def format_search_response(query, response_data, count, filter?) do
    data = Map.get(response_data, "data", %{})

    pages = data |> get_in(["webPages", "value"]) |> ensure_list()
    images = data |> get_in(["images", "value"]) |> ensure_list()
    videos = data |> get_in(["videos", "value"]) |> ensure_list()

    {pages, stats} = maybe_filter_pages(query, pages, count, filter?)

    if pages == [] and images == [] and videos == [] do
      ~s[{"status": "no_results", "query": "#{query}", "message": "No results found for the search query"}]
    else
      result_builder = ["Search results for: #{query}\n"] ++ filter_lines(stats)

      result_builder =
        result_builder ++ section("Web Pages", indexed_lines(pages, &page_lines/2))

      result_builder =
        result_builder ++ section("Images", indexed_lines(images, &media_lines/2))

      result_builder = result_builder ++ section("Videos", indexed_lines(videos, &media_lines/2))

      Enum.join(result_builder, "\n")
    end
  end

  defp ensure_list(list) when is_list(list), do: list
  defp ensure_list(_), do: []

  # ── Smart Decide relevance filter ─────────────────────────────────────────

  defp maybe_filter_pages(_query, pages, _count, false) do
    total = length(pages)
    {pages, %{candidates: total, relevant: total, errors: 0, filtered: false, fallback: false}}
  end

  # Filtering widened the API pool (see `request_count/2`) so the model had
  # enough to choose from; narrow the judged set back to the caller's `count`.
  defp maybe_filter_pages(query, pages, count, true) do
    {relevant, stats} = Relevance.filter(query, pages, Relevance.config())
    kept = narrow(relevant, count)

    {kept, Map.put(stats, :returned, length(kept))}
  end

  # A non-positive or non-integer `count` leaves the judged set untouched.
  defp narrow(pages, count) when is_integer(count) and count > 0, do: Enum.take(pages, count)
  defp narrow(pages, _count), do: pages

  defp filter_lines(%{filtered: true, fallback: true}) do
    ["Smart Decide relevance filter found nothing relevant — showing raw results.\n"]
  end

  defp filter_lines(%{filtered: true} = stats) do
    [
      "Smart Decide relevance filter judged #{stats.relevant}/#{stats.candidates} result(s) relevant" <>
        returned_suffix(stats) <> ".\n"
    ]
  end

  defp filter_lines(_stats), do: []

  # Only mentioned when the caller's `count` cut the relevant set short.
  defp returned_suffix(%{returned: returned, relevant: relevant}) when returned < relevant do
    "; returning #{returned}"
  end

  defp returned_suffix(_stats), do: ""

  # ── section formatting ─────────────────────────────────────────────────────

  defp section(_title, []), do: []

  defp section(title, line_groups) do
    ["\n## #{title}\n\n"] ++ Enum.concat(line_groups)
  end

  defp indexed_lines(items, fun) do
    items
    |> Enum.with_index(1)
    |> Enum.map(fn {item, idx} -> fun.(item, idx) end)
  end

  defp page_lines(page, idx) do
    name = Map.get(page, "name", "")
    url = Map.get(page, "url", "")
    snippet = Map.get(page, "snippet", "")
    summary = Map.get(page, "summary", "")

    lines = ["#{idx}. **#{name}**", "   URL: #{url}"]
    lines = if snippet != "", do: lines ++ ["   Snippet: #{snippet}"], else: lines
    lines = if summary != "", do: lines ++ ["   Summary: #{summary}"], else: lines
    lines ++ [""]
  end

  defp media_lines(item, idx) do
    name = Map.get(item, "name", "")
    host_url = Map.get(item, "hostPageUrl", "")
    thumbnail = Map.get(item, "thumbnailUrl", "")

    lines = ["#{idx}. #{name}", "   URL: #{host_url}"]
    lines = if thumbnail != "", do: lines ++ ["   Thumbnail: #{thumbnail}"], else: lines
    lines ++ [""]
  end
end
