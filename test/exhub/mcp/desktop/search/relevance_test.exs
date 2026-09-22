defmodule Exhub.MCP.Desktop.Search.RelevanceTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Desktop.Search.Relevance

  defp block(file, code \\ "some code") do
    %{"file" => file, "owner_symbol" => "handle", "code" => code}
  end

  defp noul(probability) do
    {:ok, %{"answers" => %{"relevant" => %{"type" => "noul", "noul" => probability}}}}
  end

  describe "filter/3" do
    test "keeps only blocks whose noul probability meets the threshold" do
      candidates = [block("keep.ex", "budget planning"), block("drop.ex", "grocery list")]

      decider = fn state, _questions, _opts ->
        if String.contains?(state, "keep.ex"), do: noul(0.9), else: noul(0.1)
      end

      {relevant, stats} = Relevance.filter("budget", candidates, decider: decider)

      assert Enum.map(relevant, & &1["file"]) == ["keep.ex"]

      assert stats ==
               %{
                 candidates: 2,
                 relevant: 1,
                 errors: 0,
                 skipped: 0,
                 filtered: true,
                 fallback: false
               }
    end

    test "preserves the input order" do
      candidates = [block("a.ex"), block("b.ex"), block("c.ex")]
      decider = fn _state, _questions, _opts -> noul(0.9) end

      {relevant, _stats} = Relevance.filter("query", candidates, decider: decider)

      assert Enum.map(relevant, & &1["file"]) == ["a.ex", "b.ex", "c.ex"]
    end

    test "judges one block per request with a single noul question" do
      {:ok, agent} = Agent.start_link(fn -> [] end)

      decider = fn state, questions, _opts ->
        Agent.update(agent, &[{state, questions} | &1])
        noul(0.9)
      end

      candidates = [block("a.ex"), block("b.ex"), block("c.ex")]
      {relevant, _stats} = Relevance.filter("budget planning", candidates, decider: decider)

      assert length(relevant) == 3

      calls = Agent.get(agent, & &1)
      assert length(calls) == 3
      assert Enum.all?(calls, fn {state, _questions} -> state =~ "File: " end)

      {_state, questions} = hd(calls)
      assert map_size(questions) == 1
      assert questions["relevant"]["type"] == "noul"
      assert questions["relevant"]["instructions"] =~ "budget planning"
      assert questions["relevant"]["instructions"] =~ "When unsure"
    end

    test "respects a custom threshold" do
      decider = fn _state, _questions, _opts -> noul(0.6) end

      {kept, _} =
        Relevance.filter("q", [block("a.ex")],
          decider: decider,
          threshold: 0.9,
          fallback: false
        )

      assert kept == []

      {kept, _} = Relevance.filter("q", [block("a.ex")], decider: decider, threshold: 0.5)
      assert length(kept) == 1
    end

    test "fails open when a decision errors" do
      decider = fn state, _questions, _opts ->
        if String.contains?(state, "good"), do: noul(0.9), else: {:error, "boom"}
      end

      candidates = [block("good.ex"), block("bad.ex")]
      {relevant, stats} = Relevance.filter("q", candidates, decider: decider)

      assert Enum.map(relevant, & &1["file"]) == ["good.ex", "bad.ex"]

      assert stats ==
               %{
                 candidates: 2,
                 relevant: 2,
                 errors: 1,
                 skipped: 0,
                 filtered: true,
                 fallback: false
               }
    end

    test "fails open when the decider raises" do
      decider = fn _state, _questions, _opts -> raise "kaboom" end

      {relevant, stats} = Relevance.filter("q", [block("a.ex")], decider: decider)

      assert length(relevant) == 1
      assert stats.errors == 1
    end

    test "falls back to the ranked pool when nothing is judged relevant" do
      decider = fn _state, _questions, _opts -> noul(0.01) end

      {relevant, stats} =
        Relevance.filter("q", [block("a.ex"), block("b.ex")], decider: decider)

      assert Enum.map(relevant, & &1["file"]) == ["a.ex", "b.ex"]

      assert stats ==
               %{
                 candidates: 2,
                 relevant: 2,
                 errors: 0,
                 skipped: 0,
                 filtered: true,
                 fallback: true
               }
    end

    test "returns no blocks when everything is irrelevant and fallback is off" do
      decider = fn _state, _questions, _opts -> noul(0.01) end

      {relevant, stats} =
        Relevance.filter("q", [block("a.ex"), block("b.ex")],
          decider: decider,
          fallback: false
        )

      assert relevant == []

      assert stats ==
               %{
                 candidates: 2,
                 relevant: 0,
                 errors: 0,
                 skipped: 0,
                 filtered: true,
                 fallback: false
               }
    end

    test "skips judging for a blank purpose" do
      candidates = [block("a.ex")]
      decider = fn _state, _questions, _opts -> flunk("decider must not be called") end

      assert {^candidates, stats} = Relevance.filter("   ", candidates, decider: decider)

      assert stats ==
               %{
                 candidates: 1,
                 relevant: 1,
                 errors: 0,
                 skipped: 0,
                 filtered: false,
                 fallback: false
               }
    end

    test "handles an empty candidate list" do
      decider = fn _state, _questions, _opts -> flunk("decider must not be called") end

      assert {[], stats} = Relevance.filter("q", [], decider: decider)

      assert stats ==
               %{
                 candidates: 0,
                 relevant: 0,
                 errors: 0,
                 skipped: 0,
                 filtered: false,
                 fallback: false
               }
    end

    test "ignores non-map candidates" do
      decider = fn _state, _questions, _opts -> noul(0.9) end

      {relevant, stats} =
        Relevance.filter("q", [block("a.ex"), "not-a-block", nil], decider: decider)

      assert length(relevant) == 1
      assert stats.candidates == 1
    end

    test "keeps an oversized block unjudged instead of judging truncated code" do
      decider = fn _state, _questions, _opts -> flunk("decider must not be called") end

      huge = String.duplicate("x", 7000)

      {relevant, stats} =
        Relevance.filter("q", [block("t.ex", huge)], decider: decider, fallback: false)

      assert Enum.map(relevant, & &1["file"]) == ["t.ex"]

      assert stats ==
               %{
                 candidates: 1,
                 relevant: 1,
                 errors: 0,
                 skipped: 1,
                 filtered: true,
                 fallback: false
               }
    end

    test "judges a block below max_judgeable_chars but truncates its state" do
      {:ok, agent} = Agent.start_link(fn -> nil end)

      decider = fn state, _questions, _opts ->
        Agent.update(agent, fn _ -> state end)
        noul(0.9)
      end

      code = String.duplicate("y", 5000)

      {relevant, stats} =
        Relevance.filter("q", [block("t.ex", code)],
          decider: decider,
          state_char_limit: 4000,
          max_judgeable_chars: 6000
        )

      assert length(relevant) == 1
      assert stats.skipped == 0

      state = Agent.get(agent, & &1)
      assert String.length(state) == 4001
      assert String.ends_with?(state, "…")
    end

    test "judges a block that fits and sends its full state" do
      {:ok, agent} = Agent.start_link(fn -> nil end)

      decider = fn state, _questions, _opts ->
        Agent.update(agent, fn _ -> state end)
        noul(0.9)
      end

      code = String.duplicate("y", 1500)

      {relevant, stats} =
        Relevance.filter("q", [block("t.ex", code)],
          decider: decider,
          max_judgeable_chars: 2000
        )

      assert length(relevant) == 1
      assert stats.skipped == 0

      state = Agent.get(agent, & &1)
      assert String.length(state) > 1500
      assert state =~ code
      refute String.ends_with?(state, "…")
    end

    test "the state carries the file, symbol and code" do
      {:ok, agent} = Agent.start_link(fn -> nil end)

      decider = fn state, _questions, _opts ->
        Agent.update(agent, fn _ -> state end)
        noul(0.9)
      end

      Relevance.filter("q", [block("t.ex", "def handle do\n  :ok\nend")], decider: decider)

      state = Agent.get(agent, & &1)
      assert state =~ "File: t.ex"
      assert state =~ "Symbol: handle"
      assert state =~ "def handle do"
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

      assert Keyword.get(config, :candidate_limit) == 20
      assert Keyword.get(config, :max_concurrency) == 8
      assert Keyword.get(config, :threshold) == 0.5
      assert Keyword.get(config, :state_char_limit) == 4000
      assert Keyword.get(config, :max_judgeable_chars) == 6000
      assert Keyword.get(config, :query_char_limit) == 800
      assert Keyword.get(config, :fallback) == true
      assert is_boolean(Keyword.get(config, :enabled))
    end
  end
end
