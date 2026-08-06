defmodule Exhub.MCP.Brain.Ranking.Scorers.TitleMatch do
  @moduledoc """
  Title/heading prominence scorer.

  Ranks notes higher when query terms appear in the filename or in markdown
  headings (`#`, `##`). Filename matches are weighted more heavily than
  heading matches. Returns a normalized score in `[0, 1]`.
  """

  @behaviour Exhub.MCP.Brain.Ranking.Scorer

  @impl true
  def name, do: :title_match

  @impl true
  def weight, do: 0.2

  @impl true
  def score(note, context) do
    query_terms = Map.get(context, :query_terms, [])
    if query_terms == [], do: 0.0, else: do_score(note, query_terms)
  end

  defp do_score(note, query_terms) do
    title = note |> Map.get(:file, "") |> Path.basename() |> Path.rootname() |> String.downcase()
    headings = note |> Map.get(:content, "") |> extract_headings()

    title_hits = Enum.count(query_terms, &String.contains?(title, String.downcase(&1)))
    heading_hits = Enum.count(query_terms, &terms_in?(&1, headings))

    # Weight title 2x headings, normalize by number of query terms (max 2.0)
    raw = (title_hits * 2 + heading_hits) / max(length(query_terms), 1)
    min(1.0, raw / 2.0)
  end

  defp terms_in?(term, headings) do
    Enum.any?(headings, &String.contains?(&1, String.downcase(term)))
  end

  defp extract_headings(content) when is_binary(content) do
    content
    |> String.split("\n")
    |> Enum.flat_map(fn line ->
      case Regex.run(~r/^\#{1,2}\s+(.+)$/, String.trim(line)) do
        [_, text] -> [String.downcase(text)]
        _ -> []
      end
    end)
  end

  defp extract_headings(_), do: []
end