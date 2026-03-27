defmodule Exhub.TokenUsage.TokenUsageStats do
  @moduledoc """
  Statistics and aggregation functions for token usage data.

  Provides utilities for:
  - Aggregating token usage by model, provider, or day
  - Calculating costs based on configurable pricing
  - Generating summary statistics and time-series data
  """

  alias Exhub.TokenUsage.TokenUsageStore

  # Default pricing per 1M tokens (in USD).
  # Keys are lowercase, without provider prefix, date suffix, or "-latest" suffix.
  @default_pricing %{
    # OpenAI GPT-4 family
    "gpt-4" => %{input: 30.0, output: 60.0},
    "gpt-4-32k" => %{input: 60.0, output: 120.0},
    "gpt-4-turbo" => %{input: 10.0, output: 30.0},
    "gpt-4o" => %{input: 2.5, output: 10.0},
    "gpt-4o-mini" => %{input: 0.15, output: 0.6},
    "gpt-4.1" => %{input: 2.0, output: 8.0},
    "gpt-4.1-mini" => %{input: 0.4, output: 1.6},
    "gpt-4.1-nano" => %{input: 0.1, output: 0.4},
    "gpt-4.5-preview" => %{input: 75.0, output: 150.0},
    # OpenAI reasoning models
    "o1" => %{input: 15.0, output: 60.0},
    "o1-mini" => %{input: 3.0, output: 12.0},
    "o3-mini" => %{input: 1.1, output: 4.4},
    "o3" => %{input: 10.0, output: 40.0},
    # Legacy
    "gpt-3.5-turbo" => %{input: 0.5, output: 1.5},
    # Anthropic Claude 3
    "claude-3-opus" => %{input: 15.0, output: 75.0},
    "claude-3-sonnet" => %{input: 3.0, output: 15.0},
    "claude-3-haiku" => %{input: 0.25, output: 1.25},
    # Anthropic Claude 3.5
    "claude-3-5-sonnet" => %{input: 3.0, output: 15.0},
    "claude-3-5-haiku" => %{input: 0.8, output: 4.0},
    # Anthropic Claude 3.7
    "claude-3-7-sonnet" => %{input: 3.0, output: 15.0},
    # Anthropic Claude 4
    "claude-opus-4" => %{input: 15.0, output: 75.0},
    "claude-sonnet-4" => %{input: 3.0, output: 15.0},
    "claude-haiku-4" => %{input: 0.8, output: 4.0},
    # Google Gemini
    "gemini-2.5-pro" => %{input: 1.25, output: 10.0},
    "gemini-2.5-flash" => %{input: 0.15, output: 0.6},
    "gemini-2.0-flash" => %{input: 0.1, output: 0.4},
    "gemini-1.5-pro" => %{input: 1.25, output: 5.0},
    "gemini-1.5-flash" => %{input: 0.075, output: 0.3},
    # DeepSeek
    "deepseek-chat" => %{input: 0.27, output: 1.1},
    "deepseek-v3" => %{input: 0.27, output: 1.1},
    "deepseek-v3.2" => %{input: 0.27, output: 1.1},
    "deepseek-reasoner" => %{input: 0.55, output: 2.19},
    "deepseek-r1" => %{input: 0.55, output: 2.19},
    # Qwen
    "qwen2.5-72b-instruct" => %{input: 0.4, output: 1.2},
    "qwen3-235b-a22b" => %{input: 0.6, output: 2.4},
    "qwen3-coder-480b-a35b-instruct" => %{input: 0.6, output: 2.4},
    # Kimi / Moonshot
    "kimi-k2.5" => %{input: 0.6, output: 2.5},
    "kimi-k2-instruct" => %{input: 0.6, output: 2.5},
    # Mistral
    "mistral-small" => %{input: 0.2, output: 0.6},
    "mistral-large" => %{input: 2.0, output: 6.0},
    "codestral" => %{input: 0.3, output: 0.9},
    # Cohere
    "command-r-plus" => %{input: 2.5, output: 10.0},
    "command-a-03-2025" => %{input: 2.5, output: 10.0},
    # Default fallback
    "default" => %{input: 1.0, output: 2.0}
  }

  # Provider-level fallback pricing (keyed by provider name pattern).
  @provider_pricing %{
    "openai" => %{input: 2.0, output: 8.0},
    "anthropic" => %{input: 3.0, output: 15.0},
    "google" => %{input: 0.15, output: 0.6},
    "gemini" => %{input: 0.15, output: 0.6},
    "groq" => %{input: 0.1, output: 0.1},
    "mistral" => %{input: 0.2, output: 0.6},
    "cohere" => %{input: 2.5, output: 10.0},
    "deepseek" => %{input: 0.27, output: 1.1},
    "qwen" => %{input: 0.4, output: 1.2},
    "kimi" => %{input: 0.6, output: 2.5},
    "moonshot" => %{input: 0.6, output: 2.5},
    "siliconflow" => %{input: 0.4, output: 1.2},
    "gitee" => %{input: 0.4, output: 1.2}
  }

  @doc """
  Get aggregated statistics grouped by model.

  ## Parameters
    - filters: Map with optional date range filters

  ## Examples
      iex> TokenUsageStats.aggregate_by_model(%{start_date: "2026-03-01"})
      {:ok, [%{model: "gpt-4", total_input: 100000, total_output: 50000, total_cost: 6.0}]}
  """
  @spec aggregate_by_model(map()) :: {:ok, list(map())}
  def aggregate_by_model(filters \\ %{}) do
    TokenUsageStore.get_stats(:model, filters)
  end

  @doc """
  Get aggregated statistics grouped by provider.

  ## Parameters
    - filters: Map with optional date range filters

  ## Examples
      iex> TokenUsageStats.aggregate_by_provider(%{start_date: "2026-03-01"})
      {:ok, [%{provider: "openai", total_input: 100000, total_output: 50000}]}
  """
  @spec aggregate_by_provider(map()) :: {:ok, list(map())}
  def aggregate_by_provider(filters \\ %{}) do
    TokenUsageStore.get_stats(:provider, filters)
  end

  @doc """
  Get aggregated statistics grouped by day.

  ## Parameters
    - filters: Map with optional date range filters

  ## Examples
      iex> TokenUsageStats.aggregate_by_day(%{start_date: "2026-03-01", end_date: "2026-03-11"})
      {:ok, [%{date: "2026-03-11", total_input: 50000, total_output: 20000}]}
  """
  @spec aggregate_by_day(map()) :: {:ok, list(map())}
  def aggregate_by_day(filters \\ %{}) do
    TokenUsageStore.get_stats(:day, filters)
  end

  @doc """
  Get overall summary statistics.

  ## Parameters
    - filters: Map with optional date range filters

  ## Returns
    - total_requests: Total number of requests
    - total_input_tokens: Total input/prompt tokens
    - total_output_tokens: Total output/completion tokens
    - total_tokens: Combined total tokens
    - total_cost: Estimated total cost in USD
    - unique_models_count: Number of unique models used
    - unique_providers_count: Number of unique providers used

  ## Examples
      iex> TokenUsageStats.get_summary(%{start_date: "2026-03-01"})
      {:ok, %{total_requests: 1000, total_input_tokens: 500000, total_cost: 15.50}}
  """
  @spec get_summary(map()) :: {:ok, map()}
  def get_summary(filters \\ %{}) do
    TokenUsageStore.get_summary(filters)
  end

  @doc """
  Calculate the cost for a given model and token counts.

  ## Parameters
    - model: The model name
    - input_tokens: Number of input tokens
    - output_tokens: Number of output tokens
    - pricing: Optional custom pricing map (defaults to @default_pricing)

  ## Examples
      iex> TokenUsageStats.calculate_cost("gpt-4", 1000, 500)
      0.06
  """
  @spec calculate_cost(String.t(), non_neg_integer(), non_neg_integer(), map()) :: float()
  def calculate_cost(model, input_tokens, output_tokens, pricing \\ @default_pricing) do
    normalized = normalize_model_name(model)

    model_pricing =
      Map.get(pricing, normalized) ||
        Map.get(pricing, model) ||
        infer_pricing_from_pattern(normalized, pricing) ||
        pricing["default"]

    input_cost = input_tokens * model_pricing.input / 1_000_000
    output_cost = output_tokens * model_pricing.output / 1_000_000

    input_cost + output_cost
  end

  # ---------------------------------------------------------------------------
  # Model name normalization helpers (mirrors token_usage_store.ex)
  # ---------------------------------------------------------------------------

  defp normalize_model_name(model) do
    model
    |> strip_provider_prefix()
    |> strip_date_suffix()
    |> strip_latest_suffix()
    |> String.downcase()
  end

  defp strip_provider_prefix(model) do
    case String.split(model, "/", parts: 2) do
      [_provider, rest] -> rest
      _ -> model
    end
  end

  defp strip_date_suffix(model) do
    case Regex.run(~r/^(.+)-(\d{8})$/, model) do
      [_, base, _date] -> base
      _ -> model
    end
  end

  defp strip_latest_suffix(model) do
    if String.ends_with?(model, "-latest"),
      do: String.slice(model, 0, String.length(model) - 7),
      else: model
  end

  defp infer_pricing_from_pattern(normalized, pricing) do
    cond do
      String.starts_with?(normalized, "claude-") ->
        Map.get(pricing, "claude-3-5-sonnet")

      String.starts_with?(normalized, "gpt-") or
          String.starts_with?(normalized, "o1") or
          String.starts_with?(normalized, "o3") ->
        Map.get(pricing, "gpt-4o")

      String.starts_with?(normalized, "gemini-") ->
        Map.get(pricing, "gemini-2.0-flash")

      String.starts_with?(normalized, "deepseek-") ->
        Map.get(pricing, "deepseek-chat")

      String.starts_with?(normalized, "qwen") ->
        Map.get(pricing, "qwen2.5-72b-instruct")

      String.starts_with?(normalized, "kimi-") or
          String.starts_with?(normalized, "moonshot-") ->
        Map.get(pricing, "kimi-k2.5")

      String.starts_with?(normalized, "mistral-") or
          String.starts_with?(normalized, "codestral") ->
        Map.get(pricing, "mistral-small")

      true ->
        Enum.find_value(@provider_pricing, nil, fn {provider, p} ->
          if String.starts_with?(normalized, provider), do: p
        end)
    end
  end

  @doc """
  Get usage trends over time.

  Returns daily statistics for the specified number of days.

  ## Parameters
    - days: Number of days to look back (default: 30)

  ## Examples
      iex> TokenUsageStats.get_trends(7)
      {:ok, [%{date: "2026-03-11", total_tokens: 100000, cost: 1.50}, ...]}
  """
  @spec get_trends(non_neg_integer()) :: {:ok, list(map())}
  def get_trends(days \\ 30) do
    end_date = Date.utc_today()
    start_date = Date.add(end_date, -days)

    filters = %{
      start_date: Date.to_iso8601(start_date),
      end_date: Date.to_iso8601(end_date)
    }

    TokenUsageStore.get_stats(:day, filters)
  end

  @doc """
  Get top models by usage.

  ## Parameters
    - limit: Number of top models to return (default: 10)
    - filters: Map with optional date range filters

  ## Examples
      iex> TokenUsageStats.top_models(5)
      {:ok, [%{model: "gpt-4", total_tokens: 1000000, percentage: 60.5}, ...]}
  """
  @spec top_models(non_neg_integer(), map()) :: {:ok, list(map())}
  def top_models(limit \\ 10, filters \\ %{}) do
    with {:ok, model_stats} <- aggregate_by_model(filters),
         {:ok, summary} <- get_summary(filters) do
      total_tokens = summary.total_tokens

      top_models =
        model_stats
        |> Enum.sort_by(& &1.total_tokens, :desc)
        |> Enum.take(limit)
        |> Enum.map(fn stat ->
          percentage = if total_tokens > 0, do: stat.total_tokens / total_tokens * 100, else: 0

          Map.merge(stat, %{
            percentage: Float.round(percentage, 2)
          })
        end)

      {:ok, top_models}
    end
  end

  @doc """
  Get top providers by usage.

  ## Parameters
    - limit: Number of top providers to return (default: 10)
    - filters: Map with optional date range filters

  ## Examples
      iex> TokenUsageStats.top_providers(5)
      {:ok, [%{provider: "openai", total_tokens: 1000000, percentage: 75.0}, ...]}
  """
  @spec top_providers(non_neg_integer(), map()) :: {:ok, list(map())}
  def top_providers(limit \\ 10, filters \\ %{}) do
    with {:ok, provider_stats} <- aggregate_by_provider(filters),
         {:ok, summary} <- get_summary(filters) do
      total_tokens = summary.total_tokens

      top_providers =
        provider_stats
        |> Enum.sort_by(& &1.total_tokens, :desc)
        |> Enum.take(limit)
        |> Enum.map(fn stat ->
          percentage = if total_tokens > 0, do: stat.total_tokens / total_tokens * 100, else: 0

          Map.merge(stat, %{
            percentage: Float.round(percentage, 2)
          })
        end)

      {:ok, top_providers}
    end
  end

  @doc """
  Get recent usage records.

  ## Parameters
    - limit: Number of records to return (default: 100)

  ## Examples
      iex> TokenUsageStats.recent_usage(10)
      {:ok, [%{model: "gpt-4", input_tokens: 1000, timestamp: ~U[2026-03-11...]}, ...]}
  """
  @spec recent_usage(non_neg_integer()) :: {:ok, list(map())}
  def recent_usage(limit \\ 100) do
    TokenUsageStore.get_usage(%{limit: limit})
  end

  @doc """
  Get dashboard data - comprehensive statistics for the dashboard.

  Returns a map with all key metrics for the dashboard view.

  ## Parameters
    - days: Number of days for trend data (default: 30)

  ## Examples
      iex> TokenUsageStats.get_dashboard_data()
      {:ok, %{summary: %{...}, trends: [...], top_models: [...], recent: [...]}}
  """
  @spec get_dashboard_data(non_neg_integer()) :: {:ok, map()}
  def get_dashboard_data(days \\ 30) do
    with {:ok, summary} <- get_summary(),
         {:ok, trends} <- get_trends(days),
         {:ok, top_models} <- top_models(5),
         {:ok, top_providers} <- top_providers(5),
         {:ok, recent} <- recent_usage(50) do
      data = %{
        summary: summary,
        trends: trends,
        top_models: top_models,
        top_providers: top_providers,
        recent_usage: recent
      }

      {:ok, data}
    end
  end
end
