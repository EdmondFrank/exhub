defmodule Exhub.MCP.Brain.Ranking.FusionTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.Ranking.Fusion

  defp scored_notes do
    [
      %{id: "a", scores: %{bm25: 0.9, freshness: 0.1}},
      %{id: "b", scores: %{bm25: 0.5, freshness: 0.9}},
      %{id: "c", scores: %{bm25: 0.2, freshness: 0.2}}
    ]
  end

  test "weighted_sum combines scores by weight and sorts descending" do
    weights = %{bm25: 0.6, freshness: 0.4}
    result = Fusion.weighted_sum(scored_notes(), weights)

    # b: 0.5*0.6 + 0.9*0.4 = 0.66; a: 0.9*0.6 + 0.1*0.4 = 0.58; c: 0.20
    assert Enum.map(result, & &1.id) == ["b", "a", "c"]
    assert_in_delta(hd(result).final_score, 0.66, 0.0001)
  end

  test "weighted_sum treats missing scorer weights as zero" do
    weights = %{tag_match: 1.0}
    result = Fusion.weighted_sum(scored_notes(), weights)
    assert Enum.all?(result, &(&1.final_score == 0.0))
  end

  test "weighted_sum normalizes when weights do not sum to 1.0" do
    # Doubling every weight would double the raw sum; the normalized result
    # must be identical to the weights summing to 1.0.
    base = %{bm25: 0.6, freshness: 0.4}
    doubled = Map.new(base, fn {k, v} -> {k, v * 2.0} end)

    expected = Fusion.weighted_sum(scored_notes(), base)
    result = Fusion.weighted_sum(scored_notes(), doubled)

    assert Enum.map(result, & &1.id) == Enum.map(expected, & &1.id)
    assert_in_delta(hd(result).final_score, hd(expected).final_score, 0.0001)
  end

  test "rrf ranks by reciprocal rank across signals" do
    result = Fusion.rrf(scored_notes())
    # "a" ranks 1st on bm25, 3rd on freshness; "b" ranks 2nd on bm25, 1st on freshness
    assert hd(result).id == "b"
    assert Enum.map(result, & &1.id) == ["b", "a", "c"]
  end

  test "rrf does not collapse notes sharing an id" do
    notes = [
      %{id: "x", scores: %{bm25: 0.9, freshness: 0.1}},
      %{id: "x", scores: %{bm25: 0.1, freshness: 0.9}},
      %{id: "y", scores: %{bm25: 0.5, freshness: 0.5}}
    ]

    result = Fusion.rrf(notes)
    # Both duplicate-id notes survive ranking (not silently collapsed)
    assert Enum.sort(Enum.map(result, & &1.id)) == ["x", "x", "y"]
  end

  test "max_score takes the best signal" do
    # a: 0.9, b: 0.9 tie -> both top; c: 0.2 last
    result = Fusion.max_score(scored_notes())
    assert hd(result).final_score == 0.9
    assert List.last(result).id == "c"
    assert Enum.take(Enum.map(result, & &1.id), 2) |> Enum.sort() == ["a", "b"]
  end
end