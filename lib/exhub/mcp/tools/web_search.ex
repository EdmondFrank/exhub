defmodule Exhub.MCP.Tools.WebSearch do
  @moduledoc """
  MCP Tool for searching the web for information using Gitee AI API.

  This tool uses Gitee AI's web search API to perform searches and returns results
  with titles, URLs, snippets, images, and videos.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  @giteeai_web_search_url "https://ai.gitee.com/v1/web-search"

  def name, do: "web_search"

  @impl true
  def description do
    """
    Search the web for information using AI-powered search.

    This tool performs a web search and returns relevant results including:
    - Web pages with titles, URLs, and summaries
    - Related images
    - Related videos

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
    api_key = Application.get_env(:exhub, :giteeai_api_key, "")

    if api_key == "" do
      resp = Response.tool() |> Response.error("GiteeAI API key not configured")
      {:reply, resp, frame}
    else
      do_api_search(query, count, summary, freshness, api_key, frame)
    end
  end

  defp do_api_search(query, count, summary, freshness, api_key, frame) do
    body = %{
      query: query,
      count: count,
      summary: summary,
      freshness: freshness
    }

    headers = [
      {"Content-Type", "application/json"},
      {"Authorization", "Bearer #{api_key}"}
    ]

    case HTTPoison.post(@giteeai_web_search_url, Jason.encode!(body), headers,
           recv_timeout: 25_000
         ) do
      {:ok, %HTTPoison.Response{status_code: 200, body: response_body}} ->
        case Jason.decode(response_body) do
          {:ok, response_data} ->
            formatted = format_search_response(query, response_data)
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

  defp format_search_response(query, response_data) do
    data = Map.get(response_data, "data", %{})

    result_builder = ["Search results for: #{query}\n"]

    # Format web pages
    result_builder =
      case get_in(data, ["webPages", "value"]) do
        pages when is_list(pages) and length(pages) > 0 ->
          pages_section =
            pages
            |> Enum.with_index(1)
            |> Enum.map(fn {page, idx} ->
              name = Map.get(page, "name", "")
              url = Map.get(page, "url", "")
              snippet = Map.get(page, "snippet", "")
              summary = Map.get(page, "summary", "")

              lines = ["#{idx}. **#{name}**", "   URL: #{url}"]
              lines = if snippet != "", do: lines ++ ["   Snippet: #{snippet}"], else: lines
              lines = if summary != "", do: lines ++ ["   Summary: #{summary}"], else: lines
              lines ++ [""]
            end)
            |> Enum.concat()

          result_builder ++ ["\n## Web Pages\n\n"] ++ pages_section

        _ ->
          result_builder
      end

    # Format images
    result_builder =
      case get_in(data, ["images", "value"]) do
        images when is_list(images) and length(images) > 0 ->
          images_section =
            images
            |> Enum.with_index(1)
            |> Enum.map(fn {img, idx} ->
              name = Map.get(img, "name", "")
              host_url = Map.get(img, "hostPageUrl", "")
              thumbnail = Map.get(img, "thumbnailUrl", "")

              lines = ["#{idx}. #{name}", "   URL: #{host_url}"]
              lines = if thumbnail != "", do: lines ++ ["   Thumbnail: #{thumbnail}"], else: lines
              lines ++ [""]
            end)
            |> Enum.concat()

          result_builder ++ ["\n## Images\n\n"] ++ images_section

        _ ->
          result_builder
      end

    # Format videos
    result_builder =
      case get_in(data, ["videos", "value"]) do
        videos when is_list(videos) and length(videos) > 0 ->
          videos_section =
            videos
            |> Enum.with_index(1)
            |> Enum.map(fn {video, idx} ->
              name = Map.get(video, "name", "")
              host_url = Map.get(video, "hostPageUrl", "")
              thumbnail = Map.get(video, "thumbnailUrl", "")

              lines = ["#{idx}. #{name}", "   URL: #{host_url}"]
              lines = if thumbnail != "", do: lines ++ ["   Thumbnail: #{thumbnail}"], else: lines
              lines ++ [""]
            end)
            |> Enum.concat()

          result_builder ++ ["\n## Videos\n\n"] ++ videos_section

        _ ->
          result_builder
      end

    result_text = Enum.join(result_builder, "\n")

    if result_text == "Search results for: #{query}\n" do
      ~s[{"status": "no_results", "query": "#{query}", "message": "No results found for the search query"}]
    else
      result_text
    end
  end
end
