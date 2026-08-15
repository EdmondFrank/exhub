defmodule Exhub.MCP.Brain.Search.Selector do
  @moduledoc """
  Query heuristics for auto-selecting a Brain search policy.

  Pure and side-effect free so it is trivially testable. Used by
  `Exhub.MCP.Tools.Brain.SearchVault` when the active policy is `auto` (the
  configured `default_policy`), and by the `:auto` semantic mode to decide
  whether a query looks conversational enough to warrant vector search.

  Heuristic order:

    1. `tag:` prefix            → `"keyword"` (semantic is never useful for tags)
    2. recency words            → `"recency"`
    3. ≥ 4 tokens (conversational phrasing) → `"semantic"`
    4. single word              → `"keyword"` (fast path)
    5. otherwise                → `"balanced"`
  """

  @recency_words ~w(recent latest newest new)

  @doc "Select the policy name for `query`."
  @spec select(String.t()) :: String.t()
  def select(query) do
    cond do
      String.starts_with?(query, "tag:") -> "keyword"
      recency_query?(query) -> "recency"
      semantic_query?(query) -> "semantic"
      single_word?(query) -> "keyword"
      true -> "balanced"
    end
  end

  @doc "Whether `query` reads like natural language warranting vector search (≥ 4 tokens)."
  @spec semantic_query?(String.t()) :: boolean()
  def semantic_query?(query) do
    query
    |> token_count()
    |> Kernel.>=(4)
  end

  defp recency_query?(query) do
    words = tokenize(query)
    Enum.any?(words, &(&1 in @recency_words))
  end

  defp single_word?(query) do
    not String.contains?(query, " ") and String.trim(query) != ""
  end

  defp token_count(query), do: length(tokenize(query))

  defp tokenize(query) do
    query
    |> String.downcase()
    |> String.split(~r/\s+/, trim: true)
  end
end