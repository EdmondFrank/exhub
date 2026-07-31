defmodule Exhub.Metrics.PerformanceStats do
  @moduledoc """
  Statistics and aggregation API for performance metrics.

  Provides utilities for:
  - Aggregating performance data by model, tool, provider, or day
  - Generating summary statistics with percentiles
  - Generating dashboard data
  - Querying recent raw metrics

  ## Examples

      iex> PerformanceStats.aggregate_by_model(%{start_date: "2026-07-01"})
      {:ok, [%{model: "gpt-4", avg_ms: 350, p95: 800, ...}]}

      iex> PerformanceStats.get_summary()
      {:ok, %{total_requests: 1000, avg_duration_ms: 320, p95: 750, ...}}
  """

  alias Exhub.Metrics.PerformanceStore

  @doc "Get aggregated statistics grouped by model (LLM proxy metrics)."
  @spec aggregate_by_model(map()) :: {:ok, list(map())}
  def aggregate_by_model(filters \\ %{}) do
    PerformanceStore.get_stats(:model, filters)
  end

  @doc "Get aggregated statistics grouped by tool name (MCP tool call metrics)."
  @spec aggregate_by_tool(map()) :: {:ok, list(map())}
  def aggregate_by_tool(filters \\ %{}) do
    PerformanceStore.get_stats(:tool, filters)
  end

  @doc "Get aggregated statistics grouped by provider."
  @spec aggregate_by_provider(map()) :: {:ok, list(map())}
  def aggregate_by_provider(filters \\ %{}) do
    PerformanceStore.get_stats(:provider, filters)
  end

  @doc "Get aggregated statistics grouped by day."
  @spec aggregate_by_day(map()) :: {:ok, list(map())}
  def aggregate_by_day(filters \\ %{}) do
    PerformanceStore.get_stats(:day, filters)
  end

  @doc "Get aggregated statistics grouped by metric type."
  @spec aggregate_by_type(map()) :: {:ok, list(map())}
  def aggregate_by_type(filters \\ %{}) do
    PerformanceStore.get_stats(:type, filters)
  end

  @doc "Get overall summary statistics."
  @spec get_summary(map()) :: {:ok, map()}
  def get_summary(filters \\ %{}) do
    PerformanceStore.get_summary(filters)
  end

  @doc "Get percentile stats (p50, p95, p99) for a specific type/entity."
  @spec get_percentiles(atom(), String.t() | nil, map()) :: {:ok, map()}
  def get_percentiles(metric_type, entity \\ nil, filters \\ %{}) do
    PerformanceStore.get_percentiles(metric_type, entity, filters)
  end

  @doc "Get recent raw metric records."
  @spec recent_metrics(atom(), non_neg_integer()) :: {:ok, list(map())}
  def recent_metrics(metric_type \\ :all, limit \\ 100) do
    PerformanceStore.get_recent(metric_type, limit: limit)
  end

  @doc """
  Get usage trends over time.

  Returns daily statistics for the specified number of days.

  ## Parameters
    - days: Number of days to look back (default: 30)
  """
  @spec get_trends(non_neg_integer()) :: {:ok, list(map())}
  def get_trends(days \\ 30) do
    end_date = Date.utc_today()
    start_date = Date.add(end_date, -days)

    filters = %{
      start_date: Date.to_iso8601(start_date),
      end_date: Date.to_iso8601(end_date)
    }

    PerformanceStore.get_stats(:day, filters)
  end

  @doc """
  Get top models by total duration.

  ## Parameters
    - limit: Number of top models to return (default: 10)
    - filters: Map with optional date range filters
  """
  @spec top_models(non_neg_integer(), map()) :: {:ok, list(map())}
  def top_models(limit \\ 10, filters \\ %{}) do
    with {:ok, model_stats} <- aggregate_by_model(filters),
         {:ok, summary} <- get_summary(filters) do
      total_ms = summary.total_duration_ms

      top =
        model_stats
        |> Enum.sort_by(& &1.total_ms, :desc)
        |> Enum.take(limit)
        |> Enum.map(fn stat ->
          percentage = if total_ms > 0, do: stat.total_ms / total_ms * 100, else: 0
          Map.merge(stat, %{percentage: Float.round(percentage, 2)})
        end)

      {:ok, top}
    end
  end

  @doc """
  Get top tools by total duration.

  ## Parameters
    - limit: Number of top tools to return (default: 10)
    - filters: Map with optional date range filters
  """
  @spec top_tools(non_neg_integer(), map()) :: {:ok, list(map())}
  def top_tools(limit \\ 10, filters \\ %{}) do
    with {:ok, tool_stats} <- aggregate_by_tool(filters),
         {:ok, summary} <- get_summary(filters) do
      total_ms = summary.total_duration_ms

      top =
        tool_stats
        |> Enum.sort_by(& &1.total_ms, :desc)
        |> Enum.take(limit)
        |> Enum.map(fn stat ->
          percentage = if total_ms > 0, do: stat.total_ms / total_ms * 100, else: 0
          Map.merge(stat, %{percentage: Float.round(percentage, 2)})
        end)

      {:ok, top}
    end
  end

  @doc """
  Get comprehensive dashboard data.

  ## Parameters
    - filters: Map with optional filters:
      - :days — Number of days for trend data (default: 30)
      - :start_date — ISO8601 date string
      - :end_date — ISO8601 date string
      - :metric_type — Filter by metric type
  """
  @spec get_dashboard_data(map() | non_neg_integer()) :: {:ok, map()}
  def get_dashboard_data(filters \\ %{})

  def get_dashboard_data(days) when is_integer(days) do
    get_dashboard_data(%{days: days})
  end

  def get_dashboard_data(filters) when is_map(filters) do
    date_filters = build_date_filters(filters)

    with {:ok, summary} <- get_summary(date_filters),
         {:ok, trends} <- PerformanceStore.get_stats(:day, date_filters),
         {:ok, top_models} <- top_models(5, date_filters),
         {:ok, top_tools} <- top_tools(5, date_filters),
         {:ok, recent} <- PerformanceStore.get_recent(:all, limit: 50),
         {:ok, by_type} <- aggregate_by_type(date_filters) do
      data = %{
        summary: summary,
        trends: trends,
        top_models: top_models,
        top_tools: top_tools,
        by_type: by_type,
        recent_metrics: recent
      }

      {:ok, data}
    end
  end

  # ─── Private ─────────────────────────────────────────────────────────────

  defp build_date_filters(filters) do
    base =
      %{}
      |> maybe_put_filter(:start_date, Map.get(filters, :start_date))
      |> maybe_put_filter(:end_date, Map.get(filters, :end_date))
      |> maybe_put_filter(:metric_type, Map.get(filters, :metric_type))
      |> maybe_put_filter(:entity, Map.get(filters, :entity))

    if map_size(base) == 0 and Map.has_key?(filters, :days) do
      days = Map.get(filters, :days)
      end_date = Date.utc_today()
      start_date = Date.add(end_date, -days)

      base
      |> Map.put(:start_date, Date.to_iso8601(start_date))
      |> Map.put(:end_date, Date.to_iso8601(end_date))
    else
      base
    end
  end

  defp maybe_put_filter(map, _key, nil), do: map
  defp maybe_put_filter(map, key, value), do: Map.put(map, key, value)
end
