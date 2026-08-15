defmodule Exhub.MCP.Brain.Search.PolicyTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.Search.Policy

  test "new/1 returns a struct with defaults" do
    policy = Policy.new(%{"name" => "custom"})

    assert %Policy{name: "custom"} = policy
    assert policy.retrieval == [:content]
    assert policy.semantic == :auto
    assert policy.semantic_limit == 10
    assert is_nil(policy.fusion)
    assert is_nil(policy.weights)
    assert is_nil(policy.min_score)
    assert is_nil(policy.scorers)
    assert is_nil(policy.top_n)
  end

  test "new/1 defaults name to balanced when absent" do
    assert %Policy{name: "balanced"} = Policy.new(%{})
  end

  test "new/1 accepts both string and atom keys" do
    str = Policy.new(%{"name" => "x", "semantic" => "on", "semantic_limit" => 5})
    atm = Policy.new(%{name: "x", semantic: :on, semantic_limit: 5})

    assert str.semantic == :on
    assert str.semantic_limit == 5
    assert str == atm
  end

  test "new/1 normalizes semantic string values to atoms" do
    assert Policy.new(%{"semantic" => "on"}).semantic == :on
    assert Policy.new(%{"semantic" => "off"}).semantic == :off
    assert Policy.new(%{"semantic" => "auto"}).semantic == :auto
  end

  test "new/1 ignores unknown keys" do
    policy = Policy.new(%{"name" => "x", "bogus" => 1, "nope" => "y"})
    refute Map.has_key?(policy, :bogus)
    refute Map.has_key?(policy, :nope)
  end

  test "merge/2 overrides only provided keys" do
    base = Policy.new(%{"name" => "x", "semantic" => "off", "semantic_limit" => 3})
    merged = Policy.merge(base, %{semantic: :on})

    assert merged.semantic == :on
    assert merged.semantic_limit == 3
    assert merged.name == "x"
  end

  test "merge/2 deep-merges weights maps" do
    base = Policy.new(%{"name" => "x", "weights" => %{"bm25" => 0.4, "freshness" => 0.1}})
    merged = Policy.merge(base, %{weights: %{"freshness" => 0.6}})

    assert merged.weights == %{"bm25" => 0.4, "freshness" => 0.6}
  end
end