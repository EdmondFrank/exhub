defmodule Exhub.MCP.Brain.Ranking.Ranker do
  @moduledoc """
  Orchestrates the Brain ranking pipeline.

  Applies a set of scoring modules to each candidate note, combines the
  per-signal scores via a fusion strategy, filters by a minimum score, and
  returns the notes sorted by `final_score` descending.

  Defaults are read from application config (`:exhub -> :brain_ranking`) so
  weights, scorers, and fusion can be tuned without recompiling.

  ## Config

  The config value is a map with string keys (as read by `rank/2`):

      config :exhub, :brain_ranking,
        %{
          "fusion" => "weighted_sum",
          "weights" => %{"bm25" => 0.5, "title_match" => 0.2, "freshness" => 0.1},
          "min_score" => 0.0
        }

  Per-call options merged over defaults are passed to `rank/2`.

  ## Options (`rank/2`)

    * `:fusion` - `"weighted_sum"` (default) | `"rrf"` | `"max"`
    * `:weights` - map of scorer name (string) to weight, merged over defaults
    * `:min_score` - float threshold; notes below are dropped
  """

  alias Exhub.MCP.Brain.Ranking.Fusion
  alias Exhub.MCP.Brain.Ranking.Scorers

  @default_scorers [
    Scorers.BM25,
    Scorers.TitleMatch,
    Scorers.TagMatch,
    Scorers.Freshness,
    Scorers.LinkAuthority
  ]

  @doc "Rank candidate notes using configured/overridden scorers and fusion."
  def rank(notes, opts \\ []) do
    config = Application.get_env(:exhub, :brain_ranking, %{}) |> Map.new()
    scorers = Keyword.get(opts, :scorers, @default_scorers)
    context = Keyword.get(opts, :context, %{})

    weights =
      config
      |> Map.get("weights", %{})
      |> Map.merge(Keyword.get(opts, :weights, %{}))
      |> resolve_weights(scorers)

    fusion = Keyword.get(opts, :fusion, Map.get(config, "fusion", "weighted_sum"))
    min_score = Keyword.get(opts, :min_score, Map.get(config, "min_score", 0.0))

    notes
    |> apply_scorers(scorers, context)
    |> apply_fusion(fusion, weights)
    |> filter_by_min_score(min_score)
  end

  defp apply_scorers(notes, scorers, context) do
    Enum.map(notes, fn note ->
      scores =
        Enum.map(scorers, fn scorer -> {scorer.name(), scorer.score(note, context)} end)
        |> Map.new()

      Map.put(note, :scores, scores)
    end)
  end

  defp apply_fusion(notes, fusion, weights) when is_function(fusion, 2) do
    fusion.(notes, weights)
  end

  defp apply_fusion(notes, fusion, weights) when is_binary(fusion) do
    case Map.fetch(fusion_fns(), fusion) do
      {:ok, fn0} -> fn0.(notes, weights)
      _ -> Fusion.weighted_sum(notes, weights)
    end
  end

  defp apply_fusion(notes, _fusion, weights), do: Fusion.weighted_sum(notes, weights)

  defp fusion_fns do
    %{
      "weighted_sum" => &Fusion.weighted_sum/2,
      "rrf" => fn notes, _w -> Fusion.rrf(notes) end,
      "max" => fn notes, _w -> Fusion.max_score(notes) end
    }
  end

  defp resolve_weights(config_weights, scorers) do
    Enum.reduce(scorers, %{}, fn scorer, acc ->
      weight = Map.get(config_weights, Atom.to_string(scorer.name()), scorer.weight())
      Map.put(acc, scorer.name(), weight)
    end)
  end

  defp filter_by_min_score(notes, min_score) do
    Enum.filter(notes, &(&1.final_score >= min_score))
  end
end