defmodule Exhub.MCP.Hub.ToolRelevanceTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Hub.ToolRelevance

  defp tool(name, description, server \\ "srv") do
    %{
      "name" => name,
      "full_name" => "#{server}__#{name}",
      "server" => server,
      "description" => description
    }
  end

  defp noul(probability) do
    {:ok, %{"answers" => %{"relevant" => %{"type" => "noul", "noul" => probability}}}}
  end

  describe "filter/3" do
    test "keeps only tools whose noul probability meets the threshold" do
      candidates = [tool("read_file", "Read a file"), tool("play_music", "Play a song")]

      decider = fn state, _questions, _opts ->
        if String.contains?(state, "read_file"), do: noul(0.9), else: noul(0.1)
      end

      {relevant, stats} = ToolRelevance.filter("read file", candidates, decider: decider)

      assert Enum.map(relevant, & &1["name"]) == ["read_file"]

      assert stats ==
               %{
                 candidates: 2,
                 relevant: 1,
                 errors: 0,
                 excluded: 0,
                 filtered: true,
                 fallback: false
               }
    end

    test "preserves the input order" do
      candidates = [tool("a", "A"), tool("b", "B"), tool("c", "C")]
      decider = fn _state, _questions, _opts -> noul(0.9) end

      {relevant, _stats} = ToolRelevance.filter("query", candidates, decider: decider)

      assert Enum.map(relevant, & &1["name"]) == ["a", "b", "c"]
    end

    test "judges one tool per request with a single noul question" do
      {:ok, agent} = Agent.start_link(fn -> [] end)

      decider = fn state, questions, _opts ->
        Agent.update(agent, &[{state, questions} | &1])
        noul(0.9)
      end

      candidates = [tool("a", "A"), tool("b", "B"), tool("c", "C")]
      {relevant, _stats} = ToolRelevance.filter("read file", candidates, decider: decider)

      assert length(relevant) == 3

      calls = Agent.get(agent, & &1)
      assert length(calls) == 3
      assert Enum.all?(calls, fn {state, _questions} -> state =~ "Tool: srv__" end)

      {_state, questions} = hd(calls)
      assert map_size(questions) == 1
      assert questions["relevant"]["type"] == "noul"
      assert questions["relevant"]["instructions"] =~ "read file"
      assert questions["relevant"]["instructions"] =~ "When unsure"
    end

    test "respects a custom threshold" do
      decider = fn _state, _questions, _opts -> noul(0.6) end

      {kept, _} =
        ToolRelevance.filter("q", [tool("a", "A")],
          decider: decider,
          threshold: 0.9,
          fallback: false
        )

      assert kept == []

      {kept, _} = ToolRelevance.filter("q", [tool("a", "A")], decider: decider, threshold: 0.5)
      assert length(kept) == 1
    end

    test "fails open when a decision errors" do
      decider = fn state, _questions, _opts ->
        if String.contains?(state, "good"), do: noul(0.9), else: {:error, "boom"}
      end

      candidates = [tool("good", "G"), tool("bad", "B")]
      {relevant, stats} = ToolRelevance.filter("q", candidates, decider: decider)

      assert Enum.map(relevant, & &1["name"]) == ["good", "bad"]

      assert stats ==
               %{
                 candidates: 2,
                 relevant: 2,
                 errors: 1,
                 excluded: 0,
                 filtered: true,
                 fallback: false
               }
    end

    test "fails open when the decider raises" do
      decider = fn _state, _questions, _opts -> raise "kaboom" end

      {relevant, stats} = ToolRelevance.filter("q", [tool("a", "A")], decider: decider)

      assert length(relevant) == 1
      assert stats.errors == 1
    end

    test "falls back to the TF-IDF pool when nothing is judged relevant" do
      decider = fn _state, _questions, _opts -> noul(0.01) end

      {relevant, stats} =
        ToolRelevance.filter("q", [tool("a", "A"), tool("b", "B")], decider: decider)

      assert Enum.map(relevant, & &1["name"]) == ["a", "b"]

      assert stats ==
               %{
                 candidates: 2,
                 relevant: 2,
                 errors: 0,
                 excluded: 0,
                 filtered: true,
                 fallback: true
               }
    end

    test "returns no tools when everything is irrelevant and fallback is off" do
      decider = fn _state, _questions, _opts -> noul(0.01) end

      {relevant, stats} =
        ToolRelevance.filter("q", [tool("a", "A"), tool("b", "B")],
          decider: decider,
          fallback: false
        )

      assert relevant == []

      assert stats ==
               %{
                 candidates: 2,
                 relevant: 0,
                 errors: 0,
                 excluded: 0,
                 filtered: true,
                 fallback: false
               }
    end

    test "drops excluded servers before judging" do
      candidates = [
        tool("read_file", "Read a file"),
        tool("smart_decide", "Decide", "smart-decide"),
        tool("retrieve_tools", "Search", "mcp-hub")
      ]

      decider = fn _state, _questions, _opts -> noul(0.9) end

      {relevant, stats} = ToolRelevance.filter("q", candidates, decider: decider)

      # `mcp-hub` is dropped; `smart-decide` stays in the pool so it remains
      # discoverable as a tool.
      assert Enum.map(relevant, & &1["server"]) == ["srv", "smart-decide"]
      assert stats.candidates == 3
      assert stats.excluded == 1
      assert stats.relevant == 2
    end

    test "exclude_servers is configurable" do
      decider = fn _state, _questions, _opts -> noul(0.9) end

      {relevant, _stats} =
        ToolRelevance.filter("q", [tool("a", "A")], decider: decider, exclude_servers: ["srv"])

      assert relevant == []
    end

    test "skips judging for a blank query" do
      candidates = [tool("a", "A")]
      decider = fn _state, _questions, _opts -> flunk("decider must not be called") end

      assert {^candidates, stats} = ToolRelevance.filter("   ", candidates, decider: decider)

      assert stats ==
               %{
                 candidates: 1,
                 relevant: 1,
                 errors: 0,
                 excluded: 0,
                 filtered: false,
                 fallback: false
               }
    end

    test "handles an empty candidate list" do
      decider = fn _state, _questions, _opts -> flunk("decider must not be called") end

      assert {[], stats} = ToolRelevance.filter("q", [], decider: decider)

      assert stats ==
               %{
                 candidates: 0,
                 relevant: 0,
                 errors: 0,
                 excluded: 0,
                 filtered: false,
                 fallback: false
               }
    end

    test "ignores non-map candidates" do
      decider = fn _state, _questions, _opts -> noul(0.9) end

      {relevant, stats} =
        ToolRelevance.filter("q", [tool("a", "A"), "not-a-tool", nil], decider: decider)

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
      ToolRelevance.filter("q", [tool("t", long)], decider: decider, state_char_limit: 100)

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

      assert ToolRelevance.relevant?(answers.(0.8), 0.5)
      refute ToolRelevance.relevant?(answers.(0.2), 0.5)
    end

    test "falls back to probabilities.true and choice" do
      assert ToolRelevance.relevant?(
               %{"answers" => %{"relevant" => %{"probabilities" => %{"true" => 0.9}}}},
               0.5
             )

      refute ToolRelevance.relevant?(%{"answers" => %{"relevant" => %{"choice" => "no"}}}, 0.5)
    end

    test "treats unparsable answers as relevant" do
      assert ToolRelevance.relevant?(%{"answers" => %{"relevant" => "raw"}}, 0.5)
      assert ToolRelevance.relevant?(%{"answers" => %{}}, 0.5)
      assert ToolRelevance.relevant?(nil, 0.5)
    end
  end

  describe "config/0" do
    test "exposes the in-code defaults" do
      config = ToolRelevance.config()

      assert Keyword.get(config, :enabled) == true
      assert Keyword.get(config, :candidate_limit) == 30
      assert Keyword.get(config, :max_concurrency) == 8
      assert Keyword.get(config, :threshold) == 0.5
      assert Keyword.get(config, :exclude_servers) == ["mcp-hub"]
      assert Keyword.get(config, :fallback) == true
      assert ToolRelevance.enabled?() == true
    end
  end
end
