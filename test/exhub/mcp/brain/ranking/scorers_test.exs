defmodule Exhub.MCP.Brain.Ranking.ScorersTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.Ranking.Scorers.{BM25, Freshness, LinkAuthority, TagMatch, TitleMatch}

  describe "BM25" do
    test "returns nonzero for matching notes and zero for no matches" do
      ctx = %{avgdl: 10.0, doc_count: 10, doc_freq: 2}
      assert BM25.score(%{matches: [%{}, %{}], length: 10}, ctx) > 0.0
      assert BM25.score(%{matches: [], length: 10}, ctx) == 0.0
    end

    test "normalizes to [0, 1]" do
      ctx = %{avgdl: 10.0, doc_count: 10, doc_freq: 1}
      score = BM25.score(%{matches: [%{}, %{}, %{}, %{}, %{}], length: 10}, ctx)
      assert score >= 0.0 and score <= 1.0
    end
  end

  describe "TitleMatch" do
    test "ranks notes with matching filename higher than none" do
      ctx = %{query_terms: ["meeting"]}
      with_title = TitleMatch.score(%{file: "meeting.md", content: ""}, ctx)
      without = TitleMatch.score(%{file: "random.md", content: ""}, ctx)
      assert with_title > without
    end

    test "matches headings in content" do
      ctx = %{query_terms: ["agenda"]}
      score = TitleMatch.score(%{file: "notes.md", content: "# Daily Agenda\nbody"}, ctx)
      assert score > 0.0
    end
  end

  describe "TagMatch" do
    test "explicit tag search scores a matching tag" do
      ctx = %{is_tag_search: true, tag_query: "project/active"}
      note = %{tags: ["project/active", "meeting"]}
      assert TagMatch.score(note, ctx) == 1.0
    end

    test "normal search scores by query terms in tags" do
      ctx = %{is_tag_search: false, query_terms: ["meeting"]}
      note = %{tags: ["meeting", "other"]}
      assert TagMatch.score(note, ctx) == 1.0
    end

    test "normal search matches tags case-insensitively" do
      ctx = %{is_tag_search: false, query_terms: ["meeting"]}
      note = %{tags: ["Meeting"]}
      assert TagMatch.score(note, ctx) == 1.0
    end
  end

  describe "Freshness" do
    test "recent note scores higher than old note" do
      recent = DateTime.utc_now()
      # Use a unix epoch-derived UTC datetime to avoid tzdata dependency
      old = DateTime.from_unix!(0)
      assert Freshness.score(%{mtime: recent}, %{}) > Freshness.score(%{mtime: old}, %{})
    end

    test "neutral 0.5 when no mtime" do
      assert Freshness.score(%{}, %{}) == 0.5
    end
  end

  describe "LinkAuthority" do
    test "scores by backlink count normalized against max" do
      ctx = %{backlinks: %{"popular.md" => 20}}
      assert LinkAuthority.score(%{file: "popular.md"}, ctx) == 1.0
      assert LinkAuthority.score(%{file: "unlinked.md"}, ctx) == 0.0
    end
  end
end