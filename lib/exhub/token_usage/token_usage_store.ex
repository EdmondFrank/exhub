defmodule Exhub.TokenUsage.TokenUsageStore do
  @moduledoc """
  Store for token usage tracking and analytics.

  This module manages time-series token usage data with support for:
  - Recording token usage per request with model, provider, and timestamp
  - Aggregated queries by date range, model, or provider
  - Persistent storage via ETS and JSON file
  - Cost estimation based on configurable pricing

  Uses composite ETS keys {model, date} for efficient time-series queries.
  """

  use GenServer

  require Logger

  @table :token_usage_store
  @data_dir Path.join([System.user_home(), ".config", "exhub"])
  @data_file "token_usage.json"

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

  # Client API

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Record token usage for a request.

  ## Parameters
    - model: The model name (e.g., "gpt-4", "claude-3-sonnet")
    - provider: The provider name (e.g., "openai", "anthropic", "gitee")
    - input_tokens: Number of input/prompt tokens
    - output_tokens: Number of output/completion tokens
    - metadata: Optional map with additional fields (request_id, user_id, etc.)

  ## Examples
      iex> TokenUsageStore.record_usage("gpt-4", "openai", 1000, 500)
      :ok
  """
  @spec record_usage(String.t(), String.t(), non_neg_integer(), non_neg_integer(), map()) :: :ok
  def record_usage(model, provider, input_tokens, output_tokens, metadata \\ %{}) do
    GenServer.cast(__MODULE__, {:record, model, provider, input_tokens, output_tokens, metadata})
  end

  @doc """
  Get token usage records with optional filters.

  ## Parameters
    - filters: Map with optional filters:
      - :start_date - DateTime struct or ISO8601 string
      - :end_date - DateTime struct or ISO8601 string
      - :model - Filter by model name
      - :provider - Filter by provider name
      - :limit - Maximum records to return (default: 100)
      - :offset - Pagination offset (default: 0)

  ## Examples
      iex> TokenUsageStore.get_usage(%{model: "gpt-4", limit: 10})
      {:ok, [%{model: "gpt-4", input_tokens: 1000, ...}]}
  """
  @spec get_usage(map()) :: {:ok, list(map())}
  def get_usage(filters \\ %{}) do
    GenServer.call(__MODULE__, {:get_usage, filters})
  end

  @doc """
  Get daily aggregated token totals.

  ## Parameters
    - date: Date struct or ISO8601 date string (YYYY-MM-DD)

  ## Examples
      iex> TokenUsageStore.get_daily_totals(~D[2026-03-11])
      {:ok, %{input_tokens: 50000, output_tokens: 20000, request_count: 100}}
  """
  @spec get_daily_totals(Date.t() | String.t()) :: {:ok, map()}
  def get_daily_totals(date) do
    GenServer.call(__MODULE__, {:daily_totals, date})
  end

  @doc """
  Get usage statistics aggregated by model, provider, or day.

  ## Parameters
    - group_by: Atom - :model, :provider, or :day
    - filters: Map with date range filters

  ## Examples
      iex> TokenUsageStore.get_stats(:model, %{start_date: "2026-03-01", end_date: "2026-03-11"})
      {:ok, [%{model: "gpt-4", total_input: 100000, total_output: 50000, ...}]}
  """
  @spec get_stats(atom(), map()) :: {:ok, list(map())}
  def get_stats(group_by, filters \\ %{}) do
    GenServer.call(__MODULE__, {:get_stats, group_by, filters})
  end

  @doc """
  Get overall summary statistics.

  ## Parameters
    - filters: Map with date range filters

  ## Examples
      iex> TokenUsageStore.get_summary(%{start_date: "2026-03-01"})
      {:ok, %{total_requests: 1000, total_input_tokens: 500000, total_cost: 15.50}}
  """
  @spec get_summary(map()) :: {:ok, map()}
  def get_summary(filters \\ %{}) do
    GenServer.call(__MODULE__, {:get_summary, filters})
  end

  @doc """
  Wait for the store to finish loading from disk.
  """
  def await_loaded(server \\ __MODULE__, timeout_ms \\ 5000) do
    GenServer.call(server, :await_loaded, timeout_ms)
  end

  # Server Callbacks

  @impl true
  def init(_opts) do
    table = :ets.new(@table, [:set, :protected, :named_table, read_concurrency: true])

    data_path = data_file_path()

    state = %{
      table: table,
      data_path: data_path,
      dirty: false,
      timer: nil,
      loaded: false,
      last_persist_at: nil,
      waiting_calls: [],
      pricing: load_pricing()
    }

    state =
      case load_from_file(state.data_path) do
        {:ok, records} ->
          Enum.each(records, fn record ->
            insert_record(table, record)
          end)

          Logger.info("TokenUsageStore: Loaded #{length(records)} records from #{data_path}")
          %{state | loaded: true}

        {:error, :file_not_found} ->
          Logger.info("TokenUsageStore: No existing data file found at #{data_path}")
          %{state | loaded: true}

        {:error, reason} ->
          Logger.warning("TokenUsageStore: Failed to load data: #{inspect(reason)}")
          %{state | loaded: true}
      end

    timer = schedule_persist(5000)

    {:ok, %{state | timer: timer}}
  end

  @impl true
  def handle_cast({:record, model, provider, input_tokens, output_tokens, metadata}, state) do
    timestamp = Map.get(metadata, :timestamp, DateTime.utc_now())
    date = DateTime.to_date(timestamp)
    date_key = normalize_date(date)
    key = {model, date_key}

    case :ets.lookup(state.table, key) do
      [{^key, existing_record}] ->
        existing_count = existing_record.request_count || 1

        updated_record = %{
          existing_record
          | input_tokens: existing_record.input_tokens + input_tokens,
            output_tokens: existing_record.output_tokens + output_tokens,
            total_tokens: existing_record.total_tokens + input_tokens + output_tokens,
            estimated_cost:
              existing_record.estimated_cost +
                calculate_cost(model, input_tokens, output_tokens, state.pricing),
            request_count: existing_count + 1,
            timestamp: timestamp
        }

        :ets.insert(state.table, {key, updated_record})

        Logger.debug(
          "[TokenUsage] Updated aggregate for #{model} on #{date_key}: +#{input_tokens} input, +#{output_tokens} output, +1 request"
        )

      [] ->
        record = %{
          id: generate_id(),
          model: model,
          provider: provider,
          input_tokens: input_tokens,
          output_tokens: output_tokens,
          total_tokens: input_tokens + output_tokens,
          estimated_cost: calculate_cost(model, input_tokens, output_tokens, state.pricing),
          request_count: 1,
          timestamp: timestamp,
          date: date,
          request_id: Map.get(metadata, :request_id),
          user_id: Map.get(metadata, :user_id)
        }

        :ets.insert(state.table, {key, record})

        Logger.debug(
          "[TokenUsage] Created new aggregate for #{model} on #{date_key}: #{input_tokens} input, #{output_tokens} output, 1 request"
        )
    end

    persist_to_file_async(state.table, state.data_path)

    {:noreply, %{state | dirty: true}}
  end

  @impl true
  def handle_call({:get_usage, filters}, _from, state) do
    records =
      state.table
      |> :ets.tab2list()
      |> Enum.map(fn {_key, record} -> record end)
      |> apply_filters(filters)
      |> apply_pagination(filters)

    {:reply, {:ok, records}, state}
  end

  @impl true
  def handle_call({:daily_totals, date}, _from, state) do
    date_str = normalize_date(date)

    {input_total, output_total, count} =
      state.table
      |> :ets.tab2list()
      |> Enum.filter(fn {_key, record} ->
        record_date = normalize_date(record.date)
        record_date == date_str
      end)
      |> Enum.reduce({0, 0, 0}, fn {_key, record}, {input_acc, output_acc, count_acc} ->
        {input_acc + record.input_tokens, output_acc + record.output_tokens, count_acc + 1}
      end)

    result = %{
      date: date_str,
      input_tokens: input_total,
      output_tokens: output_total,
      total_tokens: input_total + output_total,
      request_count: count
    }

    {:reply, {:ok, result}, state}
  end

  @impl true
  def handle_call({:get_stats, group_by, filters}, _from, state) do
    records =
      state.table
      |> :ets.tab2list()
      |> Enum.map(fn {_key, record} -> record end)
      |> apply_date_filters(filters)

    grouped =
      case group_by do
        :model ->
          records
          |> Enum.group_by(& &1.model)
          |> Enum.map(fn {model, model_records} ->
            aggregate_records(model, model_records, :model)
          end)

        :provider ->
          records
          |> Enum.group_by(& &1.provider)
          |> Enum.map(fn {provider, provider_records} ->
            aggregate_records(provider, provider_records, :provider)
          end)

        :day ->
          records
          |> Enum.group_by(&normalize_date(&1.date))
          |> Enum.map(fn {date, date_records} ->
            aggregate_records(date, date_records, :day)
          end)
          |> Enum.sort_by(& &1.date, :desc)

        _ ->
          []
      end

    {:reply, {:ok, grouped}, state}
  end

  @impl true
  def handle_call({:get_summary, filters}, _from, state) do
    records =
      state.table
      |> :ets.tab2list()
      |> Enum.map(fn {_key, record} -> record end)
      |> apply_date_filters(filters)

    summary =
      Enum.reduce(
        records,
        %{
          total_requests: 0,
          total_input_tokens: 0,
          total_output_tokens: 0,
          total_tokens: 0,
          total_cost: 0.0,
          unique_models: MapSet.new(),
          unique_providers: MapSet.new()
        },
        fn record, acc ->
          %{
            total_requests: acc.total_requests + 1,
            total_input_tokens: acc.total_input_tokens + record.input_tokens,
            total_output_tokens: acc.total_output_tokens + record.output_tokens,
            total_tokens: acc.total_tokens + record.total_tokens,
            total_cost: acc.total_cost + record.estimated_cost,
            unique_models: MapSet.put(acc.unique_models, record.model),
            unique_providers: MapSet.put(acc.unique_providers, record.provider)
          }
        end
      )

    result = %{
      total_requests: summary.total_requests,
      total_input_tokens: summary.total_input_tokens,
      total_output_tokens: summary.total_output_tokens,
      total_tokens: summary.total_tokens,
      total_cost: Float.round(summary.total_cost, 4),
      unique_models_count: MapSet.size(summary.unique_models),
      unique_providers_count: MapSet.size(summary.unique_providers),
      unique_models: MapSet.to_list(summary.unique_models),
      unique_providers: MapSet.to_list(summary.unique_providers)
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
    persist_to_file_async(state.table, state.data_path)
    timer = schedule_persist()
    {:noreply, %{state | dirty: false, timer: timer, last_persist_at: DateTime.utc_now()}}
  end

  @impl true
  def terminate(_reason, state) do
    if state.dirty do
      persist_to_file(state.table, state.data_path)
    end

    if state.timer, do: Process.cancel_timer(state.timer)
    :ok
  end

  # Private Functions

  defp insert_record(table, record) do
    date_key = normalize_date(record.date)
    # Use composite key: {{model, date}, record} for efficient queries
    :ets.insert(table, {{record.model, date_key}, record})
  end

  defp generate_id do
    "#{System.system_time(:millisecond)}_#{:erlang.unique_integer([:positive])}"
  end

  defp calculate_cost(model, input_tokens, output_tokens, pricing) do
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

  defp load_pricing do
    # Could be extended to load from config
    @default_pricing
  end

  # Normalize a model name for pricing lookup:
  #   1. Strip provider prefix  (e.g. "openai/gpt-4.1"  → "gpt-4.1")
  #   2. Strip 8-digit date suffix (e.g. "claude-sonnet-4.6-20260219" → "claude-sonnet-4.6")
  #   3. Strip "-latest" suffix
  #   4. Lowercase
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

  # Pattern-based provider fallback when no exact model match is found.
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
        provider_pricing_for(normalized)
    end
  end

  # Look up provider-level pricing by matching the start of the model name
  # against known provider keys.
  defp provider_pricing_for(normalized) do
    Enum.find_value(@provider_pricing, nil, fn {provider, pricing} ->
      if String.starts_with?(normalized, provider), do: pricing
    end)
  end

  defp normalize_date(%Date{} = date), do: Date.to_iso8601(date)
  defp normalize_date(%DateTime{} = dt), do: Date.to_iso8601(DateTime.to_date(dt))
  defp normalize_date(date_str) when is_binary(date_str), do: date_str

  defp apply_filters(records, filters) do
    records
    |> filter_by_model(filters[:model])
    |> filter_by_provider(filters[:provider])
    |> filter_by_date_range(filters[:start_date], filters[:end_date])
    |> sort_by_timestamp(:desc)
  end

  defp apply_date_filters(records, filters) do
    records
    |> filter_by_date_range(filters[:start_date], filters[:end_date])
  end

  defp apply_pagination(records, filters) do
    limit = filters[:limit] || 100
    offset = filters[:offset] || 0

    records
    |> Enum.drop(offset)
    |> Enum.take(limit)
  end

  defp filter_by_model(records, nil), do: records
  defp filter_by_model(records, model), do: Enum.filter(records, &(&1.model == model))

  defp filter_by_provider(records, nil), do: records
  defp filter_by_provider(records, provider), do: Enum.filter(records, &(&1.provider == provider))

  defp filter_by_date_range(records, nil, nil), do: records

  defp filter_by_date_range(records, start_date, nil) do
    start_str = normalize_date(start_date)

    Enum.filter(records, fn record ->
      normalize_date(record.date) >= start_str
    end)
  end

  defp filter_by_date_range(records, nil, end_date) do
    end_str = normalize_date(end_date)

    Enum.filter(records, fn record ->
      normalize_date(record.date) <= end_str
    end)
  end

  defp filter_by_date_range(records, start_date, end_date) do
    start_str = normalize_date(start_date)
    end_str = normalize_date(end_date)

    Enum.filter(records, fn record ->
      date_str = normalize_date(record.date)
      date_str >= start_str and date_str <= end_str
    end)
  end

  defp sort_by_timestamp(records, :desc) do
    Enum.sort_by(records, & &1.timestamp, {:desc, DateTime})
  end

  defp aggregate_records(key, records, type) do
    {input_total, output_total, cost_total, count} =
      Enum.reduce(records, {0, 0, 0.0, 0}, fn record,
                                              {input_acc, output_acc, cost_acc, count_acc} ->
        {
          input_acc + record.input_tokens,
          output_acc + record.output_tokens,
          cost_acc + record.estimated_cost,
          count_acc + 1
        }
      end)

    base = %{
      total_input: input_total,
      total_output: output_total,
      total_tokens: input_total + output_total,
      total_cost: Float.round(cost_total, 4),
      request_count: count
    }

    case type do
      :model -> Map.put(base, :model, key)
      :provider -> Map.put(base, :provider, key)
      :day -> Map.put(base, :date, key)
      _ -> base
    end
  end

  defp schedule_persist(delay \\ 60000) do
    Process.send_after(self(), :persist, delay)
  end

  defp data_file_path do
    Path.join(@data_dir, @data_file)
  end

  defp load_from_file(path) do
    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content, keys: :atoms!) do
          {:ok, records} when is_list(records) ->
            # Convert string timestamps back to DateTime
            records =
              Enum.map(records, fn record ->
                %{
                  record
                  | timestamp: parse_datetime(record.timestamp),
                    date: parse_date(record.date)
                }
              end)

            {:ok, records}

          _ ->
            {:error, :invalid_format}
        end

      {:error, :enoent} ->
        {:error, :file_not_found}

      {:error, reason} ->
        {:error, reason}
    end
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

  defp persist_to_file(table, path) do
    entries =
      :ets.tab2list(table)
      |> Enum.map(fn {_key, record} ->
        %{
          id: record.id,
          model: record.model,
          provider: record.provider,
          input_tokens: record.input_tokens,
          output_tokens: record.output_tokens,
          total_tokens: record.total_tokens,
          estimated_cost: record.estimated_cost,
          request_count: record.request_count,
          timestamp: DateTime.to_iso8601(record.timestamp),
          date: Date.to_iso8601(record.date),
          request_id: record.request_id,
          user_id: record.user_id
        }
      end)

    File.mkdir_p!(Path.dirname(path))

    tmp_path = path <> ".tmp"
    File.write!(tmp_path, Jason.encode!(entries))
    File.rename!(tmp_path, path)

    :ok
  rescue
    e ->
      Logger.error("Failed to persist token usage data: #{inspect(e)}")
      {:error, :persist_failed}
  end

  defp persist_to_file_async(table, path) do
    Task.start(fn ->
      persist_to_file(table, path)
    end)
  end
end
