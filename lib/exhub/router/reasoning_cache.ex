defmodule Exhub.Router.ReasoningCache do
  @moduledoc """
  ETS-backed cache for Kimi/MiMo `reasoning_content`, keyed by tool_call ID.

  Moonshot (kimi-k2.5/k2.6) and Xiaomi MiMo (mimo-v2.5/mimo-v2.5-pro) require that every assistant message containing
  `tool_calls` also carries the original `reasoning_content` from when the model
  produced those calls. Clients (Claude Code, etc.) typically strip this field
  when replaying conversation history, so the proxy must re-inject it.

  This cache stores the actual reasoning text keyed by each tool_call ID
  (a stable UUID the client echoes back verbatim). Entries expire after 2 hours.
  A periodic cleanup timer removes expired entries every 10 minutes.
  """

  use GenServer
  require Logger

  @default_name __MODULE__
  @ttl_ms 2 * 60 * 60 * 1000
  @cleanup_interval_ms 10 * 60 * 1000

  # ── Public API ──────────────────────────────────────────────────────────────

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, @default_name)
    GenServer.start_link(__MODULE__, name, name: name)
  end

  @doc "Store reasoning content for a single tool_call ID."
  @spec put(GenServer.server(), String.t(), String.t()) :: :ok
  def put(server \\ @default_name, tool_call_id, reasoning_content)
      when is_binary(tool_call_id) and is_binary(reasoning_content) do
    GenServer.cast(server, {:put, tool_call_id, reasoning_content})
  end

  @doc "Retrieve reasoning content for a tool_call ID. Returns nil if not found or expired."
  @spec get(GenServer.server(), String.t()) :: String.t() | nil
  def get(server \\ @default_name, tool_call_id) when is_binary(tool_call_id) do
    GenServer.call(server, {:get, tool_call_id})
  end

  @doc """
  Given a list of tool_call maps (each with an `"id"` key), return the first
  cached `reasoning_content` found, or nil if none are cached.
  """
  @spec get_for_tool_calls(GenServer.server(), list()) :: String.t() | nil
  def get_for_tool_calls(server \\ @default_name, tool_calls) when is_list(tool_calls) do
    Enum.find_value(tool_calls, fn
      %{"id" => id} when is_binary(id) -> get(server, id)
      _ -> nil
    end)
  end

  @doc """
  Parse a response body (JSON or SSE stream) and store any `reasoning_content`
  found, keyed by the tool_call IDs present in the same message.

  Safe to call with any binary — invalid JSON and missing fields are silently ignored.
  """
  @spec put_from_response(GenServer.server(), String.t()) :: :ok
  def put_from_response(server \\ @default_name, response_body) when is_binary(response_body) do
    try do
      do_put_from_response(server, response_body)
    rescue
      _ -> :ok
    end
  end

  # ── GenServer callbacks ──────────────────────────────────────────────────────

  @impl true
  def init(name) do
    table = :ets.new(name, [:set, :private])
    schedule_cleanup()
    {:ok, table}
  end

  @impl true
  def handle_cast({:put, tool_call_id, reasoning_content}, table) do
    expires_at = System.monotonic_time(:millisecond) + @ttl_ms
    :ets.insert(table, {tool_call_id, reasoning_content, expires_at})
    {:noreply, table}
  end

  @impl true
  def handle_call({:get, tool_call_id}, _from, table) do
    now = System.monotonic_time(:millisecond)

    result =
      case :ets.lookup(table, tool_call_id) do
        [{^tool_call_id, content, expires_at}] when expires_at > now -> content
        _ -> nil
      end

    {:reply, result, table}
  end

  @impl true
  def handle_info(:cleanup, table) do
    now = System.monotonic_time(:millisecond)
    :ets.select_delete(table, [{{:_, :_, :"$1"}, [{:<, :"$1", now}], [true]}])
    schedule_cleanup()
    {:noreply, table}
  end

  # ── Private helpers ──────────────────────────────────────────────────────────

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup, @cleanup_interval_ms)
  end

  # Try non-streaming JSON first; fall back to SSE line-by-line scan.
  defp do_put_from_response(server, body) do
    case Jason.decode(body) do
      {:ok, %{"choices" => choices}} when is_list(choices) ->
        Enum.each(choices, fn choice ->
          message = Map.get(choice, "message") || Map.get(choice, "delta") || %{}
          store_from_message(server, message)
        end)

      _ ->
        put_from_sse(server, body)
    end
  end

  # Accumulate reasoning_content and tool_call IDs across SSE delta chunks.
  defp put_from_sse(server, body) do
    lines = String.split(body, "\n")

    {reasoning, ids} =
      Enum.reduce(lines, {"", []}, fn line, {acc_reasoning, acc_ids} ->
        trimmed = String.trim_leading(line, "data: ")

        case trimmed do
          "[DONE]" ->
            {acc_reasoning, acc_ids}

          json_str ->
            case Jason.decode(json_str) do
              {:ok, %{"choices" => choices}} when is_list(choices) ->
                Enum.reduce(choices, {acc_reasoning, acc_ids}, fn choice, {r, ids} ->
                  delta = Map.get(choice, "delta", %{})

                  r =
                    case Map.get(delta, "reasoning_content") do
                      rc when is_binary(rc) -> r <> rc
                      _ -> r
                    end

                  new_ids =
                    case Map.get(delta, "tool_calls") do
                      tcs when is_list(tcs) ->
                        Enum.reduce(tcs, ids, fn
                          %{"id" => id}, acc when is_binary(id) and id != "" -> [id | acc]
                          _, acc -> acc
                        end)

                      _ ->
                        ids
                    end

                  {r, new_ids}
                end)

              _ ->
                {acc_reasoning, acc_ids}
            end
        end
      end)

    if reasoning != "" and ids != [] do
      Enum.each(ids, fn id -> put(server, id, reasoning) end)
    end

    :ok
  end

  defp store_from_message(server, message) do
    reasoning = Map.get(message, "reasoning_content")
    tool_calls = Map.get(message, "tool_calls")

    if is_binary(reasoning) and reasoning != "" and
         is_list(tool_calls) and tool_calls != [] do
      Enum.each(tool_calls, fn
        %{"id" => id} when is_binary(id) -> put(server, id, reasoning)
        _ -> :ok
      end)
    end
  end
end
