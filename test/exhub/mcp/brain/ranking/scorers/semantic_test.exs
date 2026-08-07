defmodule Exhub.MCP.Brain.Ranking.Scorers.SemanticTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.Ranking.Scorers.Semantic

  test "name is :semantic" do
    assert Semantic.name() == :semantic
  end

  test "scores a note by best-matching chunk similarity" do
    note = %{file: "a.md"}
    context = %{semantic_results: [%{file: "a.md", similarity: 0.9}]}

    assert Semantic.score(note, context) == 0.9
  end

  test "returns 0.0 when the note is not in semantic results" do
    note = %{file: "b.md"}
    context = %{semantic_results: [%{file: "a.md", similarity: 0.9}]}

    assert Semantic.score(note, context) == 0.0
  end

  test "returns 0.0 when semantic results are absent" do
    assert Semantic.score(%{file: "a.md"}, %{}) == 0.0
  end

  test "clamps out-of-range similarities" do
    assert Semantic.score(%{file: "a.md"}, %{semantic_results: [%{file: "a.md", similarity: 1.5}]}) == 1.0
    assert Semantic.score(%{file: "a.md"}, %{semantic_results: [%{file: "a.md", similarity: -0.2}]}) == 0.0
  end
end