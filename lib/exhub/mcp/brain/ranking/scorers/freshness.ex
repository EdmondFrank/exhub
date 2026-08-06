defmodule Exhub.MCP.Brain.Ranking.Scorers.Freshness do
  @moduledoc """
  Recency scorer based on note modification time.

  Applies exponential time-decay: a note's score halves every
  `@half_life_days` (configurable via context `:half_life_days`). Notes
  without a modification time receive a neutral `0.5`.
  """

  @behaviour Exhub.MCP.Brain.Ranking.Scorer

  @half_life_days 30

  @impl true
  def name, do: :freshness

  @impl true
  def weight, do: 0.1

  @impl true
  def score(note, context) do
    half_life = Map.get(context, :half_life_days, @half_life_days)

    case Map.get(note, :mtime) do
      %DateTime{} = dt ->
        # Continuous decay in seconds avoids whole-day truncation/discontinuity
        age_seconds = max(DateTime.diff(DateTime.utc_now(), dt, :second), 0)
        :math.pow(0.5, age_seconds / (half_life * 86_400))

      _ ->
        0.5
    end
  end
end