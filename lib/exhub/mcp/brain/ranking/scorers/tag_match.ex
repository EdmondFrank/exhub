defmodule Exhub.MCP.Brain.Ranking.Scorers.TagMatch do
  @moduledoc """
  Tag relevance scorer.

  For explicit `tag:` searches, scores notes whose tags match the query tag
  (exact or hierarchical prefix). For normal searches, scores notes whose tags
  contain any of the query terms. Returns a normalized score in `[0, 1]`.
  """

  @behaviour Exhub.MCP.Brain.Ranking.Scorer

  @impl true
  def name, do: :tag_match

  @impl true
  def weight, do: 0.15

  @impl true
  def score(note, context) do
    tags = Map.get(note, :tags, [])

    cond do
      Map.get(context, :is_tag_search, false) ->
        tag = Map.get(context, :tag_query)
        if tag, do: tag_subscore(tags, tag), else: 0.0

      true ->
        query_terms = Map.get(context, :query_terms, [])
        if query_terms == [], do: 0.0, else: term_subscore(tags, query_terms)
    end
  end

  defp tag_subscore(tags, tag) do
    if Enum.any?(tags, &(tag_matches?(&1, tag) or tag_matches?(tag, &1))), do: 1.0, else: 0.0
  end

  defp term_subscore(tags, query_terms) do
    hits =
      Enum.count(query_terms, fn term ->
        Enum.any?(tags, fn t -> String.contains?(down(t), down(term)) end)
      end)

    hits / max(length(query_terms), 1)
  end

  defp tag_matches?(tag, query) do
    t = down(tag)
    q = down(query)
    t == q or String.starts_with?(t, q <> "/")
  end

  defp down(s), do: String.downcase(s)
end