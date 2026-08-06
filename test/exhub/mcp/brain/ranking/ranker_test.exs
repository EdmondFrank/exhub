defmodule Exhub.MCP.Brain.Ranking.RankerTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.Ranking.Ranker

  setup do
    # Deterministic defaults for tests
    Application.put_env(:exhub, :brain_ranking,
      %{"fusion" => "weighted_sum", "weights" => %{"bm25" => 1.0}, "min_score" => 0.0}
    )

    on_exit(fn -> Application.delete_env(:exhub, :brain_ranking) end)
    :ok
  end

  defp note(id, scores) do
    %{id: id, file: id, matches: [], scores: scores}
  end

  test "rank/2 applies scorers, attaches scores and final_score" do
    notes = [note("a", %{}), note("b", %{})]

    ranked =
      Ranker.rank(notes,
        weights: %{"bm25" => 1.0},
        fusion: "weighted_sum",
        context: %{query_terms: [], avgdl: 1.0, doc_count: 1, doc_freq: 1}
      )

    assert length(ranked) == 2
    assert Enum.all?(ranked, &Map.has_key?(&1, :scores))
    assert Enum.all?(ranked, &Map.has_key?(&1, :final_score))
  end

  test "min_score filters results below threshold" do
    # bm25 scorer returns 0 for empty matches, so final_score is 0
    notes = [note("a", %{})]

    ranked =
      Ranker.rank(notes,
        weights: %{"bm25" => 1.0},
        fusion: "weighted_sum",
        min_score: 0.5,
        context: %{query_terms: [], avgdl: 1.0, doc_count: 1, doc_freq: 1}
      )

    assert ranked == []
  end

  test "per-call weights are merged over config weights" do
    notes = [
      note("a", %{}),
      note("b", %{})
    ]

    ranked =
      Ranker.rank(notes,
        weights: %{"title_match" => 0.5},
        fusion: "weighted_sum",
        context: %{query_terms: []}
      )

    # bm25 weight from config (1.0) still applied; title_match override added
    assert Enum.all?(ranked, &Map.has_key?(&1.scores, :bm25))
    assert Enum.all?(ranked, &Map.has_key?(&1.scores, :title_match))
  end

  test "per-call weights override config weights" do
    # Config sets bm25 weight 1.0. A note with a bm25 match should score > 0.
    note = %{
      id: "a",
      file: "a.md",
      matches: [%{line: 1, text: "alpha"}],
      content: "alpha",
      length: 5,
      terms: ["alpha"],
      tags: []
    }

    ctx = %{query_terms: ["alpha"], avgdl: 5.0, doc_count: 1, doc_freq: 1}

    default = Ranker.rank([note], weights: %{}, fusion: "weighted_sum", context: ctx)
    zeroed = Ranker.rank([note], weights: %{"bm25" => 0.0}, fusion: "weighted_sum", context: ctx)

    # Zeroing bm25 per-call must reduce the final score (override actually applied)
    assert hd(zeroed).final_score < hd(default).final_score
  end

  test "unknown fusion falls back to weighted_sum" do
    notes = [note("a", %{}), note("b", %{})]
    ctx = %{query_terms: [], avgdl: 1.0, doc_count: 1, doc_freq: 1}

    bogus = Ranker.rank(notes, fusion: "bogus", context: ctx)
    explicit = Ranker.rank(notes, fusion: "weighted_sum", context: ctx)

    assert Enum.map(bogus, & &1.final_score) == Enum.map(explicit, & &1.final_score)
  end
end