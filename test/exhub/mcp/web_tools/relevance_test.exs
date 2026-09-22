defmodule Exhub.MCP.WebTools.RelevanceTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.WebTools.Relevance

  defp page(title, url \\ "https://example.com", snippet \\ "a snippet") do
    %{"name" => title, "url" => url, "snippet" => snippet, "summary" => ""}
  end

  defp noul(probability) do
    {:ok, %{"answers" => %{"relevant" => %{"type" => "noul", "noul" => probability}}}}
  end

  describe "filter/3" do
    test "keeps only results whose noul probability meets the threshold" do
      candidates = [page("Elixir docs"), page("Cat videos")]

      decider = fn state, _questions, _opts ->
        if String.contains?(state, "Elixir"), do: noul(0.9), else: noul(0.1)
      end

      {relevant, stats} = Relevance.filter("elixir", candidates, decider: decider)

      assert Enum.map(relevant, & &1["name"]) == ["Elixir docs"]

      assert stats ==
               %{candidates: 2, relevant: 1, errors: 0, filtered: true, fallback: false}
    end

    test "preserves the input order" do
      candidates = [page("a"), page("b"), page("c")]
      decider = fn _state, _questions, _opts -> noul(0.9) end

      {relevant, _stats} = Relevance.filter("query", candidates, decider: decider)

      assert Enum.map(relevant, & &1["name"]) == ["a", "b", "c"]
    end

    test "judges one result per request with a single noul question" do
      {:ok, agent} = Agent.start_link(fn -> [] end)

      decider = fn state, questions, _opts ->
        Agent.update(agent, &[{state, questions} | &1])
        noul(0.9)
      end

      candidates = [page("a"), page("b"), page("c")]
      {relevant, _stats} = Relevance.filter("read file", candidates, decider: decider)

      assert length(relevant) == 3

      calls = Agent.get(agent, & &1)
      assert length(calls) == 3
      assert Enum.all?(calls, fn {state, _questions} -> state =~ "Result: " end)

      {state, questions} = hd(calls)
      assert state =~ "URL: https://example.com"
      assert state =~ "Snippet: a snippet"

      assert map_size(questions) == 1
      assert questions["relevant"]["type"] == "noul"
      assert questions["relevant"]["instructions"] =~ "read file"
      assert questions["relevant"]["instructions"] =~ "When unsure"
    end

    test "respects a custom threshold" do
      decider = fn _state, _questions, _opts -> noul(0.6) end

      {kept, _} =
        Relevance.filter("q", [page("a")], decider: decider, threshold: 0.9, fallback: false)

      assert kept == []

      {kept, _} = Relevance.filter("q", [page("a")], decider: decider, threshold: 0.5)
      assert length(kept) == 1
    end

    test "fails open when a decision errors" do
      decider = fn state, _questions, _opts ->
        if String.contains?(state, "good"), do: noul(0.9), else: {:error, "boom"}
      end

      candidates = [page("good"), page("bad")]
      {relevant, stats} = Relevance.filter("q", candidates, decider: decider)

      assert Enum.map(relevant, & &1["name"]) == ["good", "bad"]

      assert stats ==
               %{candidates: 2, relevant: 2, errors: 1, filtered: true, fallback: false}
    end

    test "fails open when the decider raises" do
      decider = fn _state, _questions, _opts -> raise "kaboom" end

      {relevant, stats} = Relevance.filter("q", [page("a")], decider: decider)

      assert length(relevant) == 1
      assert stats.errors == 1
    end

    test "falls back to the search pool when nothing is judged relevant" do
      decider = fn _state, _questions, _opts -> noul(0.01) end

      {relevant, stats} =
        Relevance.filter("q", [page("a"), page("b")], decider: decider)

      assert Enum.map(relevant, & &1["name"]) == ["a", "b"]

      assert stats ==
               %{candidates: 2, relevant: 2, errors: 0, filtered: true, fallback: true}
    end

    test "returns no results when everything is irrelevant and fallback is off" do
      decider = fn _state, _questions, _opts -> noul(0.01) end

      {relevant, stats} =
        Relevance.filter("q", [page("a"), page("b")], decider: decider, fallback: false)

      assert relevant == []

      assert stats ==
               %{candidates: 2, relevant: 0, errors: 0, filtered: true, fallback: false}
    end

    test "skips judging for a blank query" do
      candidates = [page("a")]
      decider = fn _state, _questions, _opts -> flunk("decider must not be called") end

      assert {^candidates, stats} = Relevance.filter("   ", candidates, decider: decider)

      assert stats ==
               %{candidates: 1, relevant: 1, errors: 0, filtered: false, fallback: false}
    end

    test "handles an empty candidate list" do
      decider = fn _state, _questions, _opts -> flunk("decider must not be called") end

      assert {[], stats} = Relevance.filter("q", [], decider: decider)

      assert stats ==
               %{candidates: 0, relevant: 0, errors: 0, filtered: false, fallback: false}
    end

    test "ignores non-map candidates" do
      decider = fn _state, _questions, _opts -> noul(0.9) end

      {relevant, stats} = Relevance.filter("q", [page("a"), "not-a-page", nil], decider: decider)

      assert length(relevant) == 1
      assert stats.candidates == 1
    end

    test "truncates the state to state_char_limit" do
      {:ok, agent} = Agent.start_link(fn -> nil end)

      decider = fn state, _questions, _opts ->
        Agent.update(agent, fn _ -> state end)
        noul(0.9)
      end

      long = String.duplicate("x", 5000)

      Relevance.filter("q", [page("t", "https://e.com", long)],
        decider: decider,
        state_char_limit: 100
      )

      state = Agent.get(agent, & &1)
      assert String.length(state) == 101
      assert String.ends_with?(state, "…")
    end
  end

  describe "relevant?/2" do
    test "reads the noul probability" do
      answers = fn probability ->
        %{"answers" => %{"relevant" => %{"type" => "noul", "noul" => probability}}}
      end

      assert Relevance.relevant?(answers.(0.8), 0.5)
      refute Relevance.relevant?(answers.(0.2), 0.5)
    end

    test "falls back to probabilities.true and choice" do
      assert Relevance.relevant?(
               %{"answers" => %{"relevant" => %{"probabilities" => %{"true" => 0.9}}}},
               0.5
             )

      refute Relevance.relevant?(%{"answers" => %{"relevant" => %{"choice" => "no"}}}, 0.5)
    end

    test "treats unparsable answers as relevant" do
      assert Relevance.relevant?(%{"answers" => %{"relevant" => "raw"}}, 0.5)
      assert Relevance.relevant?(%{"answers" => %{}}, 0.5)
      assert Relevance.relevant?(nil, 0.5)
    end
  end

  describe "config/0" do
    test "exposes the in-code defaults" do
      config = Relevance.config()

      assert Keyword.get(config, :enabled) == true
      assert Keyword.get(config, :candidate_limit) == 20
      assert Keyword.get(config, :max_concurrency) == 8
      assert Keyword.get(config, :threshold) == 0.7
      assert Keyword.get(config, :fallback) == true
      assert Relevance.enabled?() == true
    end
  end
end
