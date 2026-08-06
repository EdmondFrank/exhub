defmodule Exhub.MCP.Brain.Ranking.Fusion do
  @moduledoc """
  Strategies for combining multiple per-signal scores into a final ranking.

  Mirrors `Mosaic.Ranking.Fusion`. Each strategy attaches a `final_score` to
  every note and returns the notes sorted by `final_score` descending.
  """

  @type scored_note :: %{scores: %{atom() => float()}, final_score: float()}
  @type weights :: %{atom() => float()}

  @doc """
  Weighted linear combination (default). `weights` is a map of
  `scorer_name => weight`; missing scorers contribute `0.0`.
  """
  @spec weighted_sum([scored_note()], weights()) :: [scored_note()]
  def weighted_sum(notes, weights) do
    notes
    |> Enum.map(fn note ->
      final =
        weights
        |> Enum.reduce(0.0, fn {name, weight}, acc ->
          acc + Map.get(note.scores, name, 0.0) * weight
        end)

      Map.put(note, :final_score, final)
    end)
    |> Enum.sort_by(& &1.final_score, :desc)
  end

  @doc """
  Reciprocal Rank Fusion. Robust when score scales differ across signals.
  Ranks notes per signal, then combines `1 / (k + rank)`.

  Notes are keyed by their positional index rather than an `:id` field, so the
  strategy is independent of note identity (no dead fallback, no silent
  collapse for duplicate ids).
  """
  @spec rrf([scored_note()], pos_integer()) :: [scored_note()]
  def rrf(notes, k \\ 60) do
    scorer_names =
      notes
      |> Enum.flat_map(&Map.keys(&1.scores))
      |> Enum.uniq()

    # Stable positional keys (0-based) decouple ranking from note identity.
    indexed = notes |> Enum.with_index()

    rankings =
      Enum.map(scorer_names, fn name ->
        ranked =
          indexed
          |> Enum.sort_by(fn {n, _i} -> Map.get(n.scores, name, 0.0) end, :desc)
          |> Enum.with_index(1)
          |> Map.new(fn {{_n, i}, rank} -> {i, rank} end)

        {name, ranked}
      end)
      |> Map.new()

    notes
    |> Enum.with_index()
    |> Enum.map(fn {note, i} ->
      final =
        Enum.reduce(scorer_names, 0.0, fn name, acc ->
          rank = Map.get(rankings[name], i)
          acc + 1.0 / (k + rank)
        end)

      Map.put(note, :final_score, final)
    end)
    |> Enum.sort_by(& &1.final_score, :desc)
  end

  @doc """
  Maximum score across all signals.
  """
  @spec max_score([scored_note()]) :: [scored_note()]
  def max_score(notes) do
    notes
    |> Enum.map(fn note ->
      final = Enum.max([0.0 | Map.values(note.scores)])
      Map.put(note, :final_score, final)
    end)
    |> Enum.sort_by(& &1.final_score, :desc)
  end
end