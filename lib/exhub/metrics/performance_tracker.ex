defmodule Exhub.Metrics.PerformanceTracker do
  @moduledoc """
  Convenience API for recording performance metrics from call sites.

  Provides simple functions that wrap PerformanceStore.record/4 with
  sensible defaults and error handling. All functions are fire-and-forget
  (cast-based) and will never raise.

  ## Usage

      # LLM proxy request
      start = System.monotonic_time(:millisecond)
      conn = ProxyPlug.forward_upstream(conn, url, opts)
      duration = System.monotonic_time(:millisecond) - start
      PerformanceTracker.record_llm_proxy("gpt-4", duration, provider: "openai")

      # MCP tool call
      start = System.monotonic_time(:millisecond)
      result = do_handle_tool_call(tool_name, arguments, frame)
      duration = System.monotonic_time(:millisecond) - start
      PerformanceTracker.record_mcp_tool_call(tool_name, duration)

      # Hercules run
      PerformanceTracker.record_hercules_run(run_id, duration_ms, status: :passed)
  """

  alias Exhub.Metrics.PerformanceStore

  require Logger

  @doc """
  Record an LLM proxy request metric.

  ## Options
    - :provider — provider name (e.g., "openai", "anthropic")
    - :ttft_ms — time to first token (for streaming)
    - :status — :success | :error | :timeout (default: :success)
    - :error_message — error details
    - :input_tokens, :output_tokens — token counts
    - :request_id — request correlation ID
  """
  @spec record_llm_proxy(String.t(), non_neg_integer(), keyword()) :: :ok
  def record_llm_proxy(model, duration_ms, opts \\ []) do
    safe_record(:llm_proxy, model, duration_ms, opts)
  end

  @doc """
  Record an MCP tool call metric.

  ## Options
    - :status — :success | :error | :timeout (default: :success)
    - :error_message — error details
    - :request_id — request correlation ID
  """
  @spec record_mcp_tool_call(String.t(), non_neg_integer(), keyword()) :: :ok
  def record_mcp_tool_call(tool_name, duration_ms, opts \\ []) do
    safe_record(:mcp_tool_call, tool_name, duration_ms, opts)
  end

  @doc """
  Record a Hercules run metric.

  ## Options
    - :status — :success | :error | :timeout (default: :success)
    - :error_message — error details
  """
  @spec record_hercules_run(String.t(), non_neg_integer(), keyword()) :: :ok
  def record_hercules_run(run_id, duration_ms, opts \\ []) do
    safe_record(:hercules_run, run_id, duration_ms, opts)
  end

  @doc """
  Record a generic metric with explicit type.

  See PerformanceStore.record/4 for details.
  """
  @spec record(atom(), String.t(), non_neg_integer(), keyword()) :: :ok
  def record(metric_type, entity, duration_ms, opts \\ []) do
    safe_record(metric_type, entity, duration_ms, opts)
  end

  # ─── Private ─────────────────────────────────────────────────────────────

  defp safe_record(metric_type, entity, duration_ms, opts) do
    if Process.whereis(PerformanceStore) do
      PerformanceStore.record(metric_type, entity, duration_ms, opts)
    else
      Logger.warning(
        "[PerfMetrics] PerformanceStore not available, skipping metric: #{metric_type}/#{entity}"
      )
    end

    :ok
  rescue
    e ->
      Logger.error("[PerfMetrics] Failed to record metric: #{inspect(e)}")
      :ok
  end
end
