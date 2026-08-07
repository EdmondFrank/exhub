defmodule Exhub.MCP.Brain.Ranking.Scorers.Semantic do
  @moduledoc """
  Semantic relevance scorer based on vector embeddings.

  Scores a note by the cosine similarity of its best-matching chunk to the
  query embedding, as returned by `Exhub.MCP.Brain.RAG.VectorIndex`.

  The search context carries the precomputed semantic results under
  `:semantic_results` — a list of `%{file, chunk_index, text, similarity}`
  — so this scorer does not perform any I/O itself. Notes whose file is not
  present in the semantic results score `0.0`.

  Returns a normalized score in `[0, 1]` (the similarity is already in that
  range since it derives from cosine distance).
  """

  @behaviour Exhub.MCP.Brain.Ranking.Scorer

  @impl true
  def name, do: :semantic

  @impl true
  def weight, do: 0.3

  @impl true
  def score(note, context) do
    file = Map.get(note, :file)
    results = Map.get(context, :semantic_results, [])

    case Enum.find(results, &(&1.file == file)) do
      nil -> 0.0
      %{similarity: sim} when is_number(sim) -> clamp(sim)
      _ -> 0.0
    end
  end

  defp clamp(v) when v < 0, do: 0.0
  defp clamp(v) when v > 1, do: 1.0
  defp clamp(v), do: v
end