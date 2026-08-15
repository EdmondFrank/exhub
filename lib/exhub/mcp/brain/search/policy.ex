defmodule Exhub.MCP.Brain.Search.Policy do
  @moduledoc """
  A named bundle of search hyper-parameters for the Brain vault.

  A policy decides how a `brain_search_vault` search runs:

    * `retrieval`      — candidate channels: `[:content]`, `[:filename]` or `[:both]`
    * `semantic`       — vector (RAG) mode: `:off`, `:auto` or `:on`
    * `semantic_limit` — max notes surfaced by the vector search
    * `fusion`/`weights`/`min_score`/`scorers` — ranking hyper-parameters;
      `nil` means "fall back to the `:brain_ranking` defaults"
    * `top_n`          — optional cap on the number of ranked files returned

  Precedence at search time (lowest to highest): built-in policy defaults,
  named policy config (`:exhub -> :brain_search -> "policies"`), per-call
  explicit params, then an inline policy map passed as `policy`.
  """

  @enforce_keys [:name]
  defstruct name: "balanced",
            description: "",
            retrieval: [:content],
            semantic: :auto,
            semantic_limit: 10,
            fusion: nil,
            weights: nil,
            min_score: nil,
            scorers: nil,
            top_n: nil

  @type t :: %__MODULE__{
          name: String.t(),
          description: String.t(),
          retrieval: [:content | :filename | :both],
          semantic: :off | :auto | :on,
          semantic_limit: pos_integer(),
          fusion: nil | String.t(),
          weights: nil | map(),
          min_score: nil | float(),
          scorers: nil | list(module()),
          top_n: nil | pos_integer()
        }

  @doc "Build a policy from a map with string or atom keys."
  @spec new(map()) :: t()
  def new(map) do
    name = Map.get(map, :name) || Map.get(map, "name") || "balanced"

    %__MODULE__{
      name: to_string(name),
      description: Map.get(map, :description) || Map.get(map, "description") || "",
      retrieval: normalize_retrieval(Map.get(map, :retrieval) || Map.get(map, "retrieval")),
      semantic: normalize_semantic(Map.get(map, :semantic) || Map.get(map, "semantic")),
      semantic_limit:
        Map.get(map, :semantic_limit) || Map.get(map, "semantic_limit") || 10,
      fusion: Map.get(map, :fusion) || Map.get(map, "fusion"),
      weights: Map.get(map, :weights) || Map.get(map, "weights"),
      min_score: Map.get(map, :min_score) || Map.get(map, "min_score"),
      scorers: Map.get(map, :scorers) || Map.get(map, "scorers"),
      top_n: Map.get(map, :top_n) || Map.get(map, "top_n")
    }
  end

  @doc "Merge `overrides` (string or atom keys) over `policy`; weights are deep-merged."
  @spec merge(t(), map()) :: t()
  def merge(policy, overrides) do
    overrides =
      Map.new(overrides, fn {k, v} ->
        key =
          case k do
            k when is_atom(k) -> k
            k when is_binary(k) -> String.to_existing_atom(k)
          end

        {key, normalize_value(key, v)}
      end)

    %{policy | weights: deep_merge_weights(policy.weights, overrides[:weights])}
    |> then(fn p ->
      Enum.reduce(overrides, p, fn
        {:weights, _}, acc -> acc
        {key, value}, acc -> Map.put(acc, key, value)
      end)
    end)
  end

  defp deep_merge_weights(nil, nil), do: nil
  defp deep_merge_weights(existing, nil), do: existing
  defp deep_merge_weights(nil, override), do: override
  defp deep_merge_weights(existing, override), do: Map.merge(existing, override)

  defp normalize_value(:semantic, v), do: normalize_semantic(v)
  defp normalize_value(:retrieval, v), do: normalize_retrieval(v)
  defp normalize_value(_key, v), do: v

  defp normalize_retrieval(list) when is_list(list), do: Enum.map(list, &normalize_channel/1)
  defp normalize_retrieval(nil), do: [:content]
  defp normalize_retrieval(channel), do: [normalize_channel(channel)]

  defp normalize_channel(c) when c in [:content, :filename, :both], do: c
  defp normalize_channel("content"), do: :content
  defp normalize_channel("filename"), do: :filename
  defp normalize_channel("both"), do: :both
  defp normalize_channel(c), do: c

  defp normalize_semantic(v) when v in [:off, :auto, :on], do: v
  defp normalize_semantic("off"), do: :off
  defp normalize_semantic("auto"), do: :auto
  defp normalize_semantic("on"), do: :on
  defp normalize_semantic(v) when is_boolean(v), do: if(v, do: :on, else: :off)
  defp normalize_semantic(_), do: :auto
end