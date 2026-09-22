defmodule Exhub.MCP.WebTools.WebSearchFormatTest do
  # Sets the app env to inject a decider, so it must not run concurrently.
  use ExUnit.Case, async: false

  alias Exhub.MCP.Tools.WebSearch
  alias Exhub.MCP.WebTools.Relevance

  setup do
    previous = Application.get_env(:exhub, Relevance)
    on_exit(fn -> restore(previous) end)
    :ok
  end

  defp restore(nil), do: Application.delete_env(:exhub, Relevance)
  defp restore(value), do: Application.put_env(:exhub, Relevance, value)

  defp response_data(titles) do
    %{"data" => %{"webPages" => %{"value" => Enum.map(titles, &page/1)}}}
  end

  defp page(title) do
    %{
      "name" => title,
      "url" => "https://example.com/#{title}",
      "snippet" => "snippet for #{title}"
    }
  end

  defp always_relevant(_state, _questions, _opts) do
    {:ok, %{"answers" => %{"relevant" => %{"type" => "noul", "noul" => 0.99}}}}
  end

  defp count_pages(output) do
    [web_pages] = output |> String.split("## Web Pages") |> Enum.drop(1) |> Enum.take(1)
    length(Regex.scan(~r/^   URL: /m, web_pages))
  end

  test "narrows the judged pool back to the caller's count" do
    Application.put_env(:exhub, Relevance,
      decider: &always_relevant/3,
      candidate_limit: 20,
      fallback: true
    )

    output = WebSearch.format_search_response("query", response_data(~w[a b c d e]), 2, true)

    assert count_pages(output) == 2
    assert output =~ "Smart Decide relevance filter judged 5/5 result(s) relevant; returning 2."
  end

  test "keeps every relevant result when count exceeds the judged pool" do
    Application.put_env(:exhub, Relevance, decider: &always_relevant/3, candidate_limit: 20)

    output = WebSearch.format_search_response("query", response_data(~w[a b]), 10, true)

    assert count_pages(output) == 2
    assert output =~ "Smart Decide relevance filter judged 2/2 result(s) relevant.\n"
    refute output =~ "returning"
  end

  test "filter: false renders the raw pages with no filter summary" do
    output = WebSearch.format_search_response("query", response_data(~w[a b c]), 3, false)

    assert count_pages(output) == 3
    refute output =~ "Smart Decide"
  end

  test "a non-positive count leaves the judged set untouched" do
    Application.put_env(:exhub, Relevance, decider: &always_relevant/3, candidate_limit: 20)

    output = WebSearch.format_search_response("query", response_data(~w[a b c]), 0, true)

    assert count_pages(output) == 3
    refute output =~ "returning"
  end
end
