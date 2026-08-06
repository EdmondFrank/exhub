defmodule Exhub.MCP.Brain.Ranking.Scorers.BM25 do
  @moduledoc """
  Lexical relevance scorer using BM25.

  Scores a note by term frequency (number of matching lines) weighted by
  document length and inverse document frequency. The raw BM25 value is
  normalized to `[0, 1]` via `x / (x + 1)` (monotonic transform).
  """

  @behaviour Exhub.MCP.Brain.Ranking.Scorer

  @k1 1.2
  @b 0.75

  @impl true
  def name, do: :bm25

  @impl true
  def weight, do: 0.5

  @impl true
  def score(note, context) do
    avgdl = Map.get(context, :avgdl, 1.0)
    doc_count = max(Map.get(context, :doc_count, 1), 1)
    doc_freq = max(Map.get(context, :doc_freq, 1), 1)
    term_freq = length(Map.get(note, :matches, []))
    doc_length = max(Map.get(note, :length, 1), 1)

    if term_freq == 0 do
      0.0
    else
      idf = :math.log((doc_count - doc_freq + 0.5) / (doc_freq + 0.5) + 1.0)
      tf = term_freq * (@k1 + 1) / (term_freq + @k1 * (1 - @b + @b * doc_length / avgdl))
      normalize(idf * tf)
    end
  end

  defp normalize(raw) when raw <= 0, do: 0.0
  defp normalize(raw), do: raw / (raw + 1.0)
end