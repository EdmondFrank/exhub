defmodule Exhub.Metrics.PerformanceStore do
  @moduledoc """
  Store for performance metrics tracking and analytics.

  This module manages time-series performance data with support for:
  - Recording individual metrics (duration, latency, status) per request
  - Two-tier storage: raw ring buffer + aggregated stats by {type, entity, date}
  - Percentile calculations (p50, p95, p99) from raw samples
  - Persistent storage via ETS and JSON file

  Metric types:
  - :llm_proxy      — LLM API proxy request latency
  - :mcp_tool_call  — MCP tool call latency
  - :hercules_run    — Hercules test run duration

  Uses two ETS tables:
  - :perf_raw       — individual records (ring buffer, max @max_raw_records)
  - :perf_aggregate — aggregated stats keyed by {metric_type, entity, date}
  """

  use GenServer

  require Logger

  @raw_table :perf_raw
  @agg_table :perf_aggregate
  @data_dir Path.join([System.user_home(), ".config", "exhub"])
  @data_file "performance_metrics.ndjson"
  @max_raw_records 10_000

  # ─── Client API ──────────────────────────────────────────────────────────

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Record a performance metric.

  ## Parameters
    - metric_type: :llm_proxy | :mcp_tool_call | :hercules_run
    - entity: model name (LLM), tool name (MCP), or run_id (Hercules)
    - duration_ms: total duration in milliseconds
    - opts: keyword list with optional fields:
      - :provider, :ttft_ms, :status, :error_message
      - :input_tokens, :output_tokens, :request_id, :timestamp

  ## Examples
      iex> PerformanceStore.record(:llm_proxy, "gpt-4", 350, provider: "openai")
      :ok
  """
  @spec record(atom(), String.t(), non_neg_integer(), keyword()) :: :ok
  def record(metric_type, entity, duration_ms, opts \\ []) do
    GenServer.cast(__MODULE__, {:record, metric_type, entity, duration_ms, opts})
  end

  @doc "Get recent raw metric records with optional filters."
  @spec get_recent(atom(), keyword()) :: {:ok, list(map())}
  def get_recent(metric_type \\ :all, opts \\ []) do
    GenServer.call(__MODULE__, {:get_recent, metric_type, opts})
  end

  @doc "Get aggregated stats grouped by model, tool, or day."
  @spec get_stats(atom(), map()) :: {:ok, list(map())}
  def get_stats(group_by, filters \\ %{}) do
    GenServer.call(__MODULE__, {:get_stats, group_by, filters})
  end

  @doc "Get overall summary statistics."
  @spec get_summary(map()) :: {:ok, map()}
  def get_summary(filters \\ %{}) do
    GenServer.call(__MODULE__, {:get_summary, filters})
  end

  @doc "Get percentile stats (p50, p95, p99) for a specific type/entity."
  @spec get_percentiles(atom(), String.t() | nil, map()) :: {:ok, map()}
  def get_percentiles(metric_type, entity \\ nil, filters \\ %{}) do
    GenServer.call(__MODULE__, {:get_percentiles, metric_type, entity, filters})
  end

  @doc "Wait for the store to finish loading from disk."
  def await_loaded(server \\ __MODULE__, timeout_ms \\ 5000) do
    GenServer.call(server, :await_loaded, timeout_ms)
  end

  # ─── Server Callbacks ────────────────────────────────────────────────────

  @impl true
  def init(_opts) do
    raw_table =
      :ets.new(@raw_table, [:bag, :public, :named_table, read_concurrency: true])

    agg_table =
      :ets.new(@agg_table, [:set, :protected, :named_table, read_concurrency: true])

    data_path = data_file_path()

    state = %{
      raw_table: raw_table,
      agg_table: agg_table,
      data_path: data_path,
      dirty: false,
      timer: nil,
      loaded: false,
      last_persist_at: nil,
      waiting_calls: [],
      raw_seq: 0
    }

    state =
      case load_from_file(data_path) do
        {:ok, {raw_records, agg_records}} ->
          Enum.each(raw_records, fn record ->
            :ets.insert(raw_table, {record.seq, record})
          end)

          Enum.each(agg_records, fn {key, agg} ->
            :ets.insert(agg_table, {key, agg})
          end)

          max_seq =
            raw_records
            |> Enum.map(& &1.seq)
            |> Enum.max(fn -> 0 end)

          Logger.info(
            "PerformanceStore: Loaded #{length(raw_records)} raw, #{length(agg_records)} aggregated records"
          )

          %{state | loaded: true, raw_seq: max_seq}

        {:error, :file_not_found} ->
          Logger.info("PerformanceStore: No existing data file found at #{data_path}")
          %{state | loaded: true}

        {:error, reason} ->
          Logger.warning("PerformanceStore: Failed to load data: #{inspect(reason)}")
          %{state | loaded: true}
      end

    timer = schedule_persist(5000)
    {:ok, %{state | timer: timer}}
  end

  @impl true
  def handle_cast({:record, metric_type, entity, duration_ms, opts}, state) do
    timestamp = Keyword.get(opts, :timestamp, DateTime.utc_now())
    date = DateTime.to_date(timestamp)
    date_key = Date.to_iso8601(date)

    status = Keyword.get(opts, :status, :success)
    provider = Keyword.get(opts, :provider)
    ttft_ms = Keyword.get(opts, :ttft_ms)
    error_message = Keyword.get(opts, :error_message)
    input_tokens = Keyword.get(opts, :input_tokens)
    output_tokens = Keyword.get(opts, :output_tokens)
    request_id = Keyword.get(opts, :request_id)

    seq = state.raw_seq + 1

    record = %{
      seq: seq,
      metric_type: metric_type,
      entity: entity,
      provider: provider,
      duration_ms: duration_ms,
      ttft_ms: ttft_ms,
      status: status,
      error_message: error_message,
      input_tokens: input_tokens,
      output_tokens: output_tokens,
      timestamp: timestamp,
      date: date,
      date_key: date_key,
      request_id: request_id
    }

    # Insert raw record
    :ets.insert(state.raw_table, {seq, record})

    # Trim old records if exceeding max
    if seq > @max_raw_records do
      trim_count = seq - @max_raw_records
      :ets.delete(state.raw_table, trim_count)
    end

    # Update aggregated stats
    update_aggregate(state.agg_table, metric_type, entity, date_key, duration_ms, status, state)

    Logger.debug(
      "[PerfMetrics] Recorded #{metric_type}/#{entity}: #{duration_ms}ms (#{status})"
    )

    {:noreply, %{state | dirty: true, raw_seq: seq}}
  end

  @impl true
  def handle_call({:get_recent, metric_type, opts}, _from, state) do
    limit = Keyword.get(opts, :limit, 100)
    offset = Keyword.get(opts, :offset, 0)

    records =
      state.raw_table
      |> :ets.tab2list()
      |> Enum.map(fn {_seq, record} -> record end)
      |> filter_by_type(metric_type)
      |> filter_raw_records(opts)
      |> Enum.sort_by(& &1.seq, :desc)
      |> Enum.drop(offset)
      |> Enum.take(limit)

    {:reply, {:ok, records}, state}
  end

  @impl true
  def handle_call({:get_stats, group_by, filters}, _from, state) do
    records =
      state.agg_table
      |> :ets.tab2list()
      |> Enum.map(fn {_key, agg} -> agg end)
      |> filter_agg_records(filters)

    grouped =
      case group_by do
        :model ->
          records
          |> Enum.group_by(& &1.entity)
          |> Enum.map(fn {model, aggs} -> aggregate_groups(model, aggs, :model) end)
          |> Enum.sort_by(& &1.total_ms, :desc)

        :tool ->
          records
          |> Enum.filter(&(&1.metric_type == :mcp_tool_call))
          |> Enum.group_by(& &1.entity)
          |> Enum.map(fn {tool, aggs} -> aggregate_groups(tool, aggs, :tool) end)
          |> Enum.sort_by(& &1.total_ms, :desc)

        :provider ->
          records
          |> Enum.group_by(& &1.provider)
          |> Enum.map(fn {provider, aggs} -> aggregate_groups(provider, aggs, :provider) end)
          |> Enum.sort_by(& &1.total_ms, :desc)

        :day ->
          records
          |> Enum.group_by(& &1.date_key)
          |> Enum.map(fn {date, aggs} -> aggregate_groups(date, aggs, :day) end)
          |> Enum.sort_by(& &1.date, :desc)

        :type ->
          records
          |> Enum.group_by(& &1.metric_type)
          |> Enum.map(fn {type, aggs} -> aggregate_groups(type, aggs, :type) end)
          |> Enum.sort_by(& &1.total_ms, :desc)

        _ ->
          []
      end

    {:reply, {:ok, grouped}, state}
  end

  @impl true
  def handle_call({:get_summary, filters}, _from, state) do
    records =
      state.raw_table
      |> :ets.tab2list()
      |> Enum.map(fn {_seq, record} -> record end)
      |> filter_raw_records(filters)

    {total_ms, count, error_count, timeout_count} =
      Enum.reduce(records, {0, 0, 0, 0}, fn r, {ms, c, ec, tc} ->
        {
          ms + r.duration_ms,
          c + 1,
          ec + if(r.status == :error, do: 1, else: 0),
          tc + if(r.status == :timeout, do: 1, else: 0)
        }
      end)

    durations = Enum.map(records, & &1.duration_ms)

    result = %{
      total_requests: count,
      total_duration_ms: total_ms,
      avg_duration_ms: if(count > 0, do: div(total_ms, count), else: 0),
      min_duration_ms: Enum.min(durations, fn -> 0 end),
      max_duration_ms: Enum.max(durations, fn -> 0 end),
      p50: percentile(durations, 50),
      p95: percentile(durations, 95),
      p99: percentile(durations, 99),
      error_count: error_count,
      timeout_count: timeout_count,
      error_rate: if(count > 0, do: Float.round(error_count / count * 100, 2), else: 0.0),
      unique_entities: records |> Enum.map(& &1.entity) |> Enum.uniq() |> length(),
      unique_types: records |> Enum.map(& &1.metric_type) |> Enum.uniq() |> length()
    }

    {:reply, {:ok, result}, state}
  end

  @impl true
  def handle_call({:get_percentiles, metric_type, entity, filters}, _from, state) do
    records =
      state.raw_table
      |> :ets.tab2list()
      |> Enum.map(fn {_seq, record} -> record end)
      |> filter_by_type(metric_type)
      |> filter_raw_records(filters)
      |> then(fn recs ->
        if entity do
          Enum.filter(recs, &(&1.entity == entity))
        else
          recs
        end
      end)

    durations = Enum.map(records, & &1.duration_ms)

    ttfts =
      records
      |> Enum.filter(&(&1.ttft_ms != nil))
      |> Enum.map(& &1.ttft_ms)

    result = %{
      metric_type: metric_type,
      entity: entity,
      count: length(durations),
      p50: percentile(durations, 50),
      p95: percentile(durations, 95),
      p99: percentile(durations, 99),
      min: Enum.min(durations, fn -> 0 end),
      max: Enum.max(durations, fn -> 0 end),
      avg: if(length(durations) > 0, do: div(Enum.sum(durations), length(durations)), else: 0),
      ttft_p50: percentile(ttfts, 50),
      ttft_p95: percentile(ttfts, 95),
      ttft_p99: percentile(ttfts, 99)
    }

    {:reply, {:ok, result}, state}
  end

  @impl true
  def handle_call(:await_loaded, from, state) do
    if state.loaded do
      {:reply, :ok, state}
    else
      {:noreply, %{state | waiting_calls: [from | state.waiting_calls]}}
    end
  end

  @impl true
  def handle_info(:persist, %{dirty: false} = state) do
    timer = schedule_persist()
    {:noreply, %{state | timer: timer}}
  end

  @impl true
  def handle_info(:persist, state) do
    persist_to_file_async(state.raw_table, state.agg_table, state.data_path)
    timer = schedule_persist()
    {:noreply, %{state | dirty: false, timer: timer, last_persist_at: DateTime.utc_now()}}
  end

  @impl true
  def terminate(_reason, state) do
    if state.dirty do
      persist_to_file(state.raw_table, state.agg_table, state.data_path)
    end

    if state.timer, do: Process.cancel_timer(state.timer)
    :ok
  end

  # ─── Private: Aggregate Updates ──────────────────────────────────────────

  defp update_aggregate(agg_table, metric_type, entity, date_key, duration_ms, status, _state) do
    key = {metric_type, entity, date_key}

    new_agg =
      case :ets.lookup(agg_table, key) do
        [{^key, existing}] ->
          durations = [duration_ms | existing.durations]
          # Keep last 200 samples for percentile calculation
          durations = Enum.take(durations, 200)

          %{
            existing
            | count: existing.count + 1,
              total_ms: existing.total_ms + duration_ms,
              min_ms: min(existing.min_ms, duration_ms),
              max_ms: max(existing.max_ms, duration_ms),
              error_count: existing.error_count + if(status == :error, do: 1, else: 0),
              timeout_count: existing.timeout_count + if(status == :timeout, do: 1, else: 0),
              durations: durations,
              last_updated: DateTime.utc_now()
          }

        [] ->
          %{
            metric_type: metric_type,
            entity: entity,
            date_key: date_key,
            count: 1,
            total_ms: duration_ms,
            min_ms: duration_ms,
            max_ms: duration_ms,
            error_count: if(status == :error, do: 1, else: 0),
            timeout_count: if(status == :timeout, do: 1, else: 0),
            durations: [duration_ms],
            last_updated: DateTime.utc_now()
          }
      end

    :ets.insert(agg_table, {key, new_agg})
  end

  # ─── Private: Group Aggregation ──────────────────────────────────────────

  defp aggregate_groups(key, aggs, type) do
    {total_ms, count, errors, timeouts, all_durations} =
      Enum.reduce(aggs, {0, 0, 0, 0, []}, fn agg, {ms, c, ec, tc, ds} ->
        {ms + agg.total_ms, c + agg.count, ec + agg.error_count,
         tc + agg.timeout_count, ds ++ agg.durations}
      end)

    base = %{
      total_ms: total_ms,
      request_count: count,
      avg_ms: if(count > 0, do: div(total_ms, count), else: 0),
      min_ms: Enum.min(all_durations, fn -> 0 end),
      max_ms: Enum.max(all_durations, fn -> 0 end),
      p50: percentile(all_durations, 50),
      p95: percentile(all_durations, 95),
      p99: percentile(all_durations, 99),
      error_count: errors,
      timeout_count: timeouts,
      error_rate: if(count > 0, do: Float.round(errors / count * 100, 2), else: 0.0)
    }

    case type do
      :model -> Map.put(base, :model, key)
      :tool -> Map.put(base, :tool, key)
      :provider -> Map.put(base, :provider, key)
      :day -> Map.put(base, :date, key)
      :type -> Map.put(base, :metric_type, key)
      _ -> base
    end
  end

  # ─── Private: Percentile Calculation ────────────────────────────────────

  defp percentile([], _p), do: 0
  defp percentile(durations, _p) when is_list(durations) and length(durations) == 1,
    do: hd(durations)

  defp percentile(durations, p) when is_list(durations) do
    sorted = Enum.sort(durations)
    n = length(sorted)
    # Use nearest-rank method
    rank = max(1, ceil(p / 100 * n))
    index = min(rank - 1, n - 1)
    Enum.at(sorted, index)
  end

  # ─── Private: Filtering ─────────────────────────────────────────────────

  defp filter_by_type(records, :all), do: records
  defp filter_by_type(records, type), do: Enum.filter(records, &(&1.metric_type == type))

  defp filter_raw_records(records, opts) when is_list(opts) do
    filter_raw_records(records, Enum.into(opts, %{}))
  end

  defp filter_raw_records(records, filters) when is_map(filters) do
    records
    |> filter_by_entity(filters[:entity] || filters["entity"])
    |> filter_by_provider(filters[:provider] || filters["provider"])
    |> filter_by_date_range(
      filters[:start_date] || filters["start_date"],
      filters[:end_date] || filters["end_date"]
    )
  end

  defp filter_by_entity(records, nil), do: records
  defp filter_by_entity(records, entity), do: Enum.filter(records, &(&1.entity == entity))

  defp filter_by_provider(records, nil), do: records
  defp filter_by_provider(records, provider),
    do: Enum.filter(records, &(&1.provider == provider))

  defp filter_by_date_range(records, nil, nil), do: records

  defp filter_by_date_range(records, start_date, nil) do
    start_str = normalize_date(start_date)
    Enum.filter(records, &(normalize_date(&1.date) >= start_str))
  end

  defp filter_by_date_range(records, nil, end_date) do
    end_str = normalize_date(end_date)
    Enum.filter(records, &(normalize_date(&1.date) <= end_str))
  end

  defp filter_by_date_range(records, start_date, end_date) do
    start_str = normalize_date(start_date)
    end_str = normalize_date(end_date)
    Enum.filter(records, fn r -> date = normalize_date(r.date); date >= start_str and date <= end_str end)
  end

  defp filter_agg_records(records, filters) when is_map(filters) do
    records
    |> filter_agg_by_type(filters[:metric_type] || filters["metric_type"])
    |> filter_agg_by_entity(filters[:entity] || filters["entity"])
    |> filter_agg_by_provider(filters[:provider] || filters["provider"])
    |> filter_agg_by_date_range(
      filters[:start_date] || filters["start_date"],
      filters[:end_date] || filters["end_date"]
    )
  end

  defp filter_agg_by_type(records, nil), do: records
  defp filter_agg_by_type(records, type), do: Enum.filter(records, &(&1.metric_type == type))

  defp filter_agg_by_entity(records, nil), do: records
  defp filter_agg_by_entity(records, entity), do: Enum.filter(records, &(&1.entity == entity))

  defp filter_agg_by_provider(records, nil), do: records
  defp filter_agg_by_provider(records, provider),
    do: Enum.filter(records, &(&1.provider == provider))

  defp filter_agg_by_date_range(records, nil, nil), do: records

  defp filter_agg_by_date_range(records, start_date, nil) do
    start_str = normalize_date(start_date)
    Enum.filter(records, &(&1.date_key >= start_str))
  end

  defp filter_agg_by_date_range(records, nil, end_date) do
    end_str = normalize_date(end_date)
    Enum.filter(records, &(&1.date_key <= end_str))
  end

  defp filter_agg_by_date_range(records, start_date, end_date) do
    start_str = normalize_date(start_date)
    end_str = normalize_date(end_date)
    Enum.filter(records, fn r -> r.date_key >= start_str and r.date_key <= end_str end)
  end

  # ─── Private: Utilities ──────────────────────────────────────────────────

  defp normalize_date(%Date{} = date), do: Date.to_iso8601(date)
  defp normalize_date(%DateTime{} = dt), do: Date.to_iso8601(DateTime.to_date(dt))
  defp normalize_date(date_str) when is_binary(date_str), do: date_str

  defp schedule_persist(delay \\ 60_000) do
    Process.send_after(self(), :persist, delay)
  end

  defp data_file_path do
    Path.join(@data_dir, @data_file)
  end

  # ─── Private: Persistence ────────────────────────────────────────────────

  defp load_from_file(path) do
    # Try NDJSON first, fall back to old JSON format
    ndjson_path = path

    if File.exists?(ndjson_path) do
      load_from_ndjson(ndjson_path)
    else
      old_json = Path.rootname(path) <> ".json"

      if File.exists?(old_json) do
        case load_from_old_json(old_json) do
          {:ok, result} ->
            # Migrate: write NDJSON and remove old file
            Logger.info("PerformanceStore: Migrating from .json to .ndjson format")
            {:ok, result}

          error ->
            error
        end
      else
        {:error, :file_not_found}
      end
    end
  end

  defp load_from_ndjson(path) do
    case File.read(path) do
      {:ok, content} ->
        lines = String.split(content, "\n", trim: true)

        {raw_records, agg_records, errors} =
          Enum.reduce(lines, {[], [], 0}, fn line, {raw, agg, errs} ->
            case Jason.decode(line) do
              {:ok, data} when is_map(data) ->
                case Map.get(data, "t") do
                  "r" -> {[parse_raw_record(data) | raw], agg, errs}
                  "a" -> {raw, [parse_agg_record(data) | agg], errs}
                  _ -> {raw, agg, errs + 1}
                end

              _ ->
                {raw, agg, errs + 1}
            end
          end)

        if errors > 0 do
          Logger.warning("PerformanceStore: #{errors} unparseable lines in #{path}")
        end

        {:ok, {Enum.reverse(raw_records), Enum.reverse(agg_records)}}

      {:error, :enoent} ->
        {:error, :file_not_found}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp load_from_old_json(path) do
    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} when is_map(data) ->
            raw_records =
              Map.get(data, "raw", [])
              |> Enum.map(&parse_raw_record/1)

            agg_records =
              Map.get(data, "aggregated", [])
              |> Enum.map(&parse_agg_record/1)

            {:ok, {raw_records, agg_records}}

          _ ->
            {:error, :invalid_format}
        end

      {:error, :enoent} ->
        {:error, :file_not_found}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_raw_record(record) when is_map(record) do
    %{
      seq: Map.get(record, "seq", 0),
      metric_type: String.to_atom(Map.get(record, "metric_type", "unknown")),
      entity: Map.get(record, "entity", ""),
      provider: Map.get(record, "provider"),
      duration_ms: Map.get(record, "duration_ms", 0),
      ttft_ms: Map.get(record, "ttft_ms"),
      status: String.to_atom(Map.get(record, "status", "success")),
      error_message: Map.get(record, "error_message"),
      input_tokens: Map.get(record, "input_tokens"),
      output_tokens: Map.get(record, "output_tokens"),
      timestamp: parse_datetime(Map.get(record, "timestamp")),
      date: parse_date(Map.get(record, "date")),
      date_key: Map.get(record, "date_key", ""),
      request_id: Map.get(record, "request_id")
    }
  end

  defp parse_agg_record(record) when is_map(record) do
    key = {
      String.to_atom(Map.get(record, "metric_type", "unknown")),
      Map.get(record, "entity", ""),
      Map.get(record, "date_key", "")
    }

    agg = %{
      metric_type: elem(key, 0),
      entity: elem(key, 1),
      date_key: elem(key, 2),
      count: Map.get(record, "count", 0),
      total_ms: Map.get(record, "total_ms", 0),
      min_ms: Map.get(record, "min_ms", 0),
      max_ms: Map.get(record, "max_ms", 0),
      error_count: Map.get(record, "error_count", 0),
      timeout_count: Map.get(record, "timeout_count", 0),
      durations: Map.get(record, "durations", []),
      last_updated: parse_datetime(Map.get(record, "last_updated"))
    }

    {key, agg}
  end

  defp parse_datetime(nil), do: DateTime.utc_now()

  defp parse_datetime(iso_string) when is_binary(iso_string) do
    case DateTime.from_iso8601(iso_string) do
      {:ok, dt, _} -> dt
      _ -> DateTime.utc_now()
    end
  end

  defp parse_datetime(%DateTime{} = dt), do: dt
  defp parse_datetime(_), do: DateTime.utc_now()

  defp parse_date(nil), do: Date.utc_today()

  defp parse_date(iso_string) when is_binary(iso_string) do
    case Date.from_iso8601(iso_string) do
      {:ok, date} -> date
      _ -> Date.utc_today()
    end
  end

  defp parse_date(%Date{} = date), do: date
  defp parse_date(_), do: Date.utc_today()

  defp persist_to_file(raw_table, agg_table, path) do
    raw_lines =
      :ets.tab2list(raw_table)
      |> Enum.map(fn {_seq, record} -> encode_raw_line(record) end)

    agg_lines =
      :ets.tab2list(agg_table)
      |> Enum.map(fn {_key, agg} -> encode_agg_line(agg) end)

    content = Enum.join(raw_lines ++ agg_lines, "\n") <> "\n"

    File.mkdir_p!(Path.dirname(path))

    tmp_path = path <> ".tmp"
    File.write!(tmp_path, content)
    File.rename!(tmp_path, path)

    # Clean up old .json format if it exists
    old_json = Path.rootname(path) <> ".json"
    if File.exists?(old_json), do: File.rm!(old_json)

    :ok
  rescue
    e ->
      Logger.error("Failed to persist performance metrics: #{inspect(e)}")
      {:error, :persist_failed}
  end

  # ─── Private: NDJSON Encoding ────────────────────────────────────────────

  defp encode_raw_line(record) do
    record
    |> Enum.filter(fn {k, v} -> k != :__struct__ and v != nil end)
    |> Enum.into(%{"t" => "r"}, fn
      {:metric_type, v} -> {"metric_type", Atom.to_string(v)}
      {:status, v} -> {"status", Atom.to_string(v)}
      {:timestamp, %DateTime{} = v} -> {"timestamp", DateTime.to_iso8601(v)}
      {:date, %Date{} = v} -> {"date", Date.to_iso8601(v)}
      {k, v} -> {Atom.to_string(k), v}
    end)
    |> Jason.encode!()
  end

  defp encode_agg_line(agg) do
    agg
    |> Enum.filter(fn {k, v} -> k != :__struct__ and v != nil end)
    |> Enum.into(%{"t" => "a"}, fn
      {:metric_type, v} -> {"metric_type", Atom.to_string(v)}
      {:last_updated, %DateTime{} = v} -> {"last_updated", DateTime.to_iso8601(v)}
      {k, v} -> {Atom.to_string(k), v}
    end)
    |> Jason.encode!()
  end

  defp persist_to_file_async(raw_table, agg_table, path) do
    Task.start(fn ->
      persist_to_file(raw_table, agg_table, path)
    end)
  end
end
