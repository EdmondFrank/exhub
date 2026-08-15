defmodule Exhub.MCP.Brain.Search.Policies do
  @moduledoc """
  Registry of Brain search policies.

  Built-in policies (`balanced`, `keyword`, `semantic`, `recency`, `filename`)
  are deep-merged with user-defined policies from config:

      config :exhub, :brain_search,
        %{
          "default_policy" => "auto",
          "semantic_autodetect" => true,
          "policies" => %{
            "keyword" => %{"weights" => %{"bm25" => 0.6}},
            "my_policy" => %{"semantic" => "on", "top_n" => 5}
          }
        }

  Since `weights` is a nested map, config entries are deep-merged over the
  built-in weights (per-signal overrides keep the other signals' defaults).
  """

  alias Exhub.MCP.Brain.Search.Policy

  @builtin_weights %{
    "bm25" => 0.4,
    "title_match" => 0.25,
    "tag_match" => 0.15,
    "freshness" => 0.1,
    "link_authority" => 0.1,
    "semantic" => 0.3
  }

  @doc "All policies: built-ins deep-merged over config-defined policies."
  @spec all() :: %{optional(String.t()) => Policy.t()}
  def all do
    config_policies = config() |> Map.get("policies", %{}) |> stringify_keys()

    Enum.reduce(config_policies, builtins(), fn {name, overrides}, acc ->
      Map.put(acc, name, Policy.merge(Map.get(acc, name) || Policy.new(%{name: name}), overrides))
    end)
  end

  @doc "Fetch a named policy, falling back to the default policy when unknown."
  @spec get(String.t() | atom()) :: Policy.t()
  def get(name) do
    key = to_string(name)
    Map.get(all(), key) || default()
  end

  @doc "The default policy per `default_policy` config (falls back to `balanced`)."
  @spec default() :: Policy.t()
  def default do
    name = config() |> Map.get("default_policy", "balanced") |> to_string()

    # `auto` is not a concrete policy; it is resolved per-query by the
    # Selector in SearchVault. Guard against infinite fallback recursion.
    if name == "auto", do: get("balanced"), else: get(name)
  end

  @doc """
  Whether auto (heuristic) policy selection applies for the given per-call
  `policy` value: an explicit `"auto"`, or no policy at all with the
  configured `default_policy` set to `"auto"`.
  """
  @spec auto?(nil | String.t() | atom() | map()) :: boolean()
  def auto?(nil) do
    config() |> Map.get("default_policy", "balanced") |> to_string() == "auto"
  end

  def auto?(%Policy{}), do: false
  def auto?(value) when is_map(value), do: false
  def auto?(value), do: to_string(value) == "auto"

  @doc "Whether conversational queries may auto-enable semantic search."
  @spec semantic_autodetect?() :: boolean()
  def semantic_autodetect? do
    config() |> Map.get("semantic_autodetect", false) |> to_boolean()
  end

  @doc """
  Resolve the active policy from a per-call `policy` value.

  Accepts `nil` (default policy), a policy name (string/atom), an inline
  policy map (`Policy.new/1`), or an already-built `Policy` struct.
  """
  @spec resolve(nil | String.t() | atom() | map() | Policy.t()) :: Policy.t()
  def resolve(nil), do: default()

  def resolve(%Policy{} = policy), do: policy
  def resolve(map) when is_map(map), do: Policy.new(map)
  def resolve(name), do: get(name)

  # ── built-ins ────────────────────────────────────────────────────────

  @doc false
  def builtins do
    weights = default_weights()

    %{
      "balanced" =>
        Policy.new(%{
          name: "balanced",
          description: "Content search with auto semantic mode and default weights.",
          retrieval: [:content],
          semantic: :auto
        }),
      "keyword" =>
        Policy.new(%{
          name: "keyword",
          description: "Fast keyword-only search; BM25 boosted, semantic off.",
          retrieval: [:content],
          semantic: :off,
          weights: Map.put(weights, "bm25", 0.6)
        }),
      "semantic" =>
        Policy.new(%{
          name: "semantic",
          description: "Semantic-first search; semantic weight boosted to 0.6.",
          retrieval: [:content],
          semantic: :on,
          weights: Map.put(weights, "semantic", 0.6)
        }),
      "recency" =>
        Policy.new(%{
          name: "recency",
          description: "Recency-first search; freshness weight boosted to 0.6.",
          retrieval: [:content],
          semantic: :auto,
          weights: Map.put(weights, "freshness", 0.6)
        }),
      "filename" =>
        Policy.new(%{
          name: "filename",
          description: "Filename-only channel; semantic off.",
          retrieval: [:filename],
          semantic: :off
        })
    }
  end

  # ── config helpers ───────────────────────────────────────────────────

  defp config do
    Application.get_env(:exhub, :brain_search, %{}) || %{}
  end

  defp default_weights do
    Application.get_env(:exhub, :brain_ranking, %{})["weights"] || @builtin_weights
  end

  defp stringify_keys(map) do
    Map.new(map, fn {k, v} -> {to_string(k), v} end)
  end

  defp to_boolean(v) when is_boolean(v), do: v
  defp to_boolean(v) when is_binary(v), do: v in ["true", "1", "yes"]
  defp to_boolean(_), do: false
end