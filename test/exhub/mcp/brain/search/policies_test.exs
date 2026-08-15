defmodule Exhub.MCP.Brain.Search.PoliciesTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Brain.Search.Policies
  alias Exhub.MCP.Brain.Search.Policy

  setup do
    Application.put_env(:exhub, :brain_search, %{})

    on_exit(fn -> Application.delete_env(:exhub, :brain_search) end)
    :ok
  end

  test "get/1 returns the balanced built-in with its defaults" do
    policy = Policies.get("balanced")

    assert %Policy{name: "balanced"} = policy
    assert policy.retrieval == [:content]
    assert policy.semantic == :auto
  end

  test "all/0 includes the five built-in policies" do
    names = Policies.all() |> Map.keys() |> Enum.sort()

    assert names == ["balanced", "filename", "keyword", "recency", "semantic"]
  end

  test "config policies are deep-merged over built-ins of the same name" do
    Application.put_env(:exhub, :brain_search, %{
      "policies" => %{"keyword" => %{"weights" => %{"bm25" => 0.6}}}
    })

    keyword = Policies.get("keyword")

    # Deep merge: provided weight bumps, other built-in weights survive.
    assert keyword.weights["bm25"] == 0.6
    assert Map.has_key?(keyword.weights, "title_match")
    # Semantic stays off for the keyword policy.
    assert keyword.semantic == :off
  end

  test "custom policies are added alongside built-ins" do
    Application.put_env(:exhub, :brain_search, %{
      "policies" => %{"my_policy" => %{"semantic" => "on", "top_n" => 5}}
    })

    assert %Policy{name: "my_policy", semantic: :on, top_n: 5} = Policies.get("my_policy")
  end

  test "unknown names fall back to the default policy" do
    assert Policies.get("does-not-exist").name == "balanced"
  end

  test "default/0 respects the configured default_policy" do
    Application.put_env(:exhub, :brain_search, %{"default_policy" => "semantic"})
    assert Policies.default().name == "semantic"
  end

  test "resolve/1 handles nil, names, maps and structs" do
    assert Policies.resolve(nil).name == "balanced"
    assert Policies.resolve("semantic").name == "semantic"
    assert Policies.resolve(%{"name" => "inline", "semantic" => "off"}).name == "inline"
    assert Policies.resolve(%Policy{name: "given"}).name == "given"
  end
end