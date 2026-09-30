# Kimi Reasoning Content Cache Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace the `"."` placeholder for `reasoning_content` with a stateful ETS cache that stores actual reasoning content keyed by tool_call IDs, and also fix `kimi-k2.6` temperature in `proxy_plug.ex`.

**Architecture:** A new `Exhub.Router.ReasoningCache` GenServer owns an ETS table. When the proxy receives a response from Moonshot, it parses the body to extract `reasoning_content` keyed by each tool_call ID. On the next request, `transform_kimi_reasoning_body/1` looks up each assistant message's tool_call IDs in the cache and injects the real content (falling back to `"."` if not found). The cache uses a short TTL (2 hours) cleaned up by a periodic timer.

**Tech Stack:** Elixir, GenServer, ETS, Jason, ExUnit

---

### Task 1: Create `Exhub.Router.ReasoningCache`

**Files:**
- Create: `lib/exhub/router/reasoning_cache.ex`
- Create: `test/exhub/router/reasoning_cache_test.exs`

**Step 1: Write the failing tests**

```elixir
# test/exhub/router/reasoning_cache_test.exs
defmodule Exhub.Router.ReasoningCacheTest do
  use ExUnit.Case, async: false

  alias Exhub.Router.ReasoningCache

  setup do
    # Start a fresh named cache for each test using a unique name
    name = :"cache_#{System.unique_integer([:positive])}"
    {:ok, pid} = ReasoningCache.start_link(name: name)
    %{cache: name, pid: pid}
  end

  test "stores and retrieves reasoning content by tool_call_id", %{cache: cache} do
    ReasoningCache.put(cache, "call_abc123", "I need to think about this carefully.")
    assert ReasoningCache.get(cache, "call_abc123") == "I need to think about this carefully."
  end

  test "returns nil for unknown tool_call_id", %{cache: cache} do
    assert ReasoningCache.get(cache, "call_unknown") == nil
  end

  test "stores multiple tool_call_ids independently", %{cache: cache} do
    ReasoningCache.put(cache, "call_1", "reasoning one")
    ReasoningCache.put(cache, "call_2", "reasoning two")
    assert ReasoningCache.get(cache, "call_1") == "reasoning one"
    assert ReasoningCache.get(cache, "call_2") == "reasoning two"
  end

  test "put_from_response/2 extracts tool_call ids from a non-streaming response body", %{cache: cache} do
    response_body = Jason.encode!(%{
      "choices" => [%{
        "message" => %{
          "role" => "assistant",
          "reasoning_content" => "Let me think...",
          "tool_calls" => [
            %{"id" => "call_xyz", "type" => "function", "function" => %{"name" => "foo", "arguments" => "{}"}}
          ]
        }
      }]
    })

    ReasoningCache.put_from_response(cache, response_body)
    assert ReasoningCache.get(cache, "call_xyz") == "Let me think..."
  end

  test "put_from_response/2 is a no-op when no tool_calls in response", %{cache: cache} do
    response_body = Jason.encode!(%{
      "choices" => [%{
        "message" => %{
          "role" => "assistant",
          "content" => "Hello!"
        }
      }]
    })

    ReasoningCache.put_from_response(cache, response_body)
    # No crash, nothing stored
    assert ReasoningCache.get(cache, "any_id") == nil
  end

  test "put_from_response/2 handles streaming SSE body by scanning for data lines", %{cache: cache} do
    sse_body = """
    data: {"choices":[{"delta":{"role":"assistant","reasoning_content":"thinking..."}}]}
    data: {"choices":[{"delta":{"tool_calls":[{"index":0,"id":"call_sse1","type":"function","function":{"name":"bar","arguments":""}}]}}]}
    data: [DONE]
    """

    ReasoningCache.put_from_response(cache, sse_body)
    assert ReasoningCache.get(cache, "call_sse1") == "thinking..."
  end

  test "put_from_response/2 is safe with invalid JSON", %{cache: cache} do
    ReasoningCache.put_from_response(cache, "not json at all")
    assert ReasoningCache.get(cache, "any") == nil
  end

  test "get_for_tool_calls/2 returns reasoning for a list of tool_call maps", %{cache: cache} do
    ReasoningCache.put(cache, "call_a", "reason A")
    tool_calls = [%{"id" => "call_a"}, %{"id" => "call_b"}]
    assert ReasoningCache.get_for_tool_calls(cache, tool_calls) == "reason A"
  end

  test "get_for_tool_calls/2 returns nil when none found", %{cache: cache} do
    tool_calls = [%{"id" => "call_missing"}]
    assert ReasoningCache.get_for_tool_calls(cache, tool_calls) == nil
  end
end
```

**Step 2: Run tests to verify they fail**

```bash
cd /Users/edmondfrank/.emacs.d/site-lisp/exhub
mix test test/exhub/router/reasoning_cache_test.exs 2>&1
```
Expected: compile error or test failures (module doesn't exist yet).

**Step 3: Implement `ReasoningCache`**

```elixir
# lib/exhub/router/reasoning_cache.ex
defmodule Exhub.Router.ReasoningCache do
  @moduledoc """
  ETS-backed cache for Kimi reasoning_content, keyed by tool_call ID.

  Moonshot (kimi-k2.5/k2.6) requires that every assistant message containing
  tool_calls also carries the original `reasoning_content` from when the model
  produced those calls. Clients (Claude Code, etc.) typically strip this field
  when replaying conversation history, so the proxy must re-inject it.

  This cache stores the actual reasoning text keyed by each tool_call ID
  (a stable UUID the client echoes back verbatim). Entries expire after 2 hours.
  """

  use GenServer
  require Logger

  @default_name __MODULE__
  @ttl_ms 2 * 60 * 60 * 1000  # 2 hours
  @cleanup_interval_ms 10 * 60 * 1000  # 10 minutes

  # ── Public API ──────────────────────────────────────────────────────────────

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, @default_name)
    GenServer.start_link(__MODULE__, name, name: name)
  end

  @doc "Store reasoning content for a single tool_call ID."
  def put(server \\ @default_name, tool_call_id, reasoning_content)
      when is_binary(tool_call_id) and is_binary(reasoning_content) do
    GenServer.cast(server, {:put, tool_call_id, reasoning_content})
  end

  @doc "Retrieve reasoning content for a tool_call ID. Returns nil if not found."
  def get(server \\ @default_name, tool_call_id) when is_binary(tool_call_id) do
    GenServer.call(server, {:get, tool_call_id})
  end

  @doc """
  Given a list of tool_call maps (each with an `"id"` key), return the first
  cached reasoning_content found, or nil.
  """
  def get_for_tool_calls(server \\ @default_name, tool_calls) when is_list(tool_calls) do
    Enum.find_value(tool_calls, fn
      %{"id" => id} when is_binary(id) -> get(server, id)
      _ -> nil
    end)
  end

  @doc """
  Parse a response body (JSON or SSE) and store any reasoning_content found,
  keyed by the tool_call IDs present in the same message.

  Safe to call with any binary — invalid JSON / missing fields are silently ignored.
  """
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

  # Non-streaming: single JSON object
  defp do_put_from_response(server, body) do
    case Jason.decode(body) do
      {:ok, %{"choices" => choices}} when is_list(choices) ->
        Enum.each(choices, fn choice ->
          message = Map.get(choice, "message") || Map.get(choice, "delta") || %{}
          store_from_message(server, message)
        end)

      _ ->
        # Might be SSE — try line-by-line
        put_from_sse(server, body)
    end
  end

  defp put_from_sse(server, body) do
    # Accumulate reasoning_content and tool_call IDs across delta chunks
    lines = String.split(body, "\n")

    {reasoning, ids} =
      Enum.reduce(lines, {"", []}, fn line, {acc_reasoning, acc_ids} ->
        case String.trim_leading(line, "data: ") do
          "[DONE]" ->
            {acc_reasoning, acc_ids}

          json_str ->
            case Jason.decode(json_str) do
              {:ok, %{"choices" => choices}} ->
                Enum.reduce(choices, {acc_reasoning, acc_ids}, fn choice, {r, ids} ->
                  delta = Map.get(choice, "delta", %{})
                  r = if rc = Map.get(delta, "reasoning_content"), do: r <> rc, else: r

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
  end

  defp store_from_message(server, message) do
    reasoning = Map.get(message, "reasoning_content")
    tool_calls = Map.get(message, "tool_calls")

    if is_binary(reasoning) and reasoning != "" and is_list(tool_calls) and tool_calls != [] do
      Enum.each(tool_calls, fn
        %{"id" => id} when is_binary(id) -> put(server, id, reasoning)
        _ -> :ok
      end)
    end
  end
end
```

**Step 4: Run tests to verify they pass**

```bash
mix test test/exhub/router/reasoning_cache_test.exs 2>&1
```
Expected: all tests pass.

**Step 5: Commit**

```bash
git add lib/exhub/router/reasoning_cache.ex test/exhub/router/reasoning_cache_test.exs
git commit -m "feat: add ReasoningCache for kimi reasoning_content round-trip"
```

---

### Task 2: Register `ReasoningCache` in the supervision tree

**Files:**
- Modify: `lib/exhub/application.ex`

**Step 1: Find the children list in `application.ex`**

Look for `children = [` in `lib/exhub/application.ex`.

**Step 2: Add `ReasoningCache` as a child**

Add `Exhub.Router.ReasoningCache` to the children list (before the endpoint):

```elixir
{Exhub.Router.ReasoningCache, []},
```

**Step 3: Compile to verify no errors**

```bash
mix compile 2>&1
```
Expected: `mix compile: ok`

**Step 4: Commit**

```bash
git add lib/exhub/application.ex
git commit -m "feat: supervise ReasoningCache"
```

---

### Task 3: Wire cache into `proxy_plug.ex` — response side (store)

**Files:**
- Modify: `lib/exhub/proxy_plug.ex`

**Context:** The `stream_accumulator/3` function already collects the full response body for token tracking. We extend it to also call `ReasoningCache.put_from_response/2` for kimi reasoning models.

**Step 1: Add a helper to detect kimi reasoning models**

In `proxy_plug.ex`, add a private function near the top of the private section:

```elixir
@kimi_reasoning_models ["kimi-k2.5", "kimi-k2.6", "inf-kimi-k2.5"]

defp kimi_reasoning_model?(model) when is_binary(model), do: model in @kimi_reasoning_models
defp kimi_reasoning_model?(_), do: false
```

**Step 2: Extend `stream_accumulator/3` to cache reasoning**

In the `{:process, ^ref, model_name, provider}` branch of `stream_accumulator/3`, after the existing token tracking spawn, add:

```elixir
if kimi_reasoning_model?(model_name) do
  Exhub.Router.ReasoningCache.put_from_response(response_body)
end
```

**Step 3: Extend `track_token_usage/4` (non-streaming path) similarly**

In `track_token_usage/4`, after the existing spawn, add the same cache call for non-streaming responses:

```elixir
defp track_token_usage({:ok, %{body: resp_body}}, model_name, provider, req_body)
     when is_binary(resp_body) and resp_body != "" do
  if kimi_reasoning_model?(model_name) do
    Exhub.Router.ReasoningCache.put_from_response(resp_body)
  end

  spawn(fn ->
    # ... existing token tracking ...
  end)
end
```

**Step 4: Compile**

```bash
mix compile 2>&1
```
Expected: `mix compile: ok`

**Step 5: Commit**

```bash
git add lib/exhub/proxy_plug.ex
git commit -m "feat: store actual reasoning_content in cache from kimi responses"
```

---

### Task 4: Wire cache into `config.ex` — request side (inject)

**Files:**
- Modify: `lib/exhub/router/config.ex`

**Context:** `transform_kimi_reasoning_body/1` currently injects `"."` unconditionally. Change it to look up the cache first and only fall back to `"."` if not found.

**Step 1: Update `transform_kimi_reasoning_body/1`**

Replace the inner `Map.put(msg, "reasoning_content", ".")` with a cache lookup:

```elixir
defp transform_kimi_reasoning_body(body) do
  messages = Map.get(body, "messages")

  if is_list(messages) do
    transformed_messages =
      Enum.map(messages, fn msg ->
        if is_map(msg) and
             Map.get(msg, "role") == "assistant" and
             is_list(Map.get(msg, "tool_calls")) and
             length(Map.get(msg, "tool_calls")) > 0 and
             is_nil(Map.get(msg, "reasoning_content")) do
          tool_calls = Map.get(msg, "tool_calls")
          cached = Exhub.Router.ReasoningCache.get_for_tool_calls(tool_calls)
          Map.put(msg, "reasoning_content", cached || ".")
        else
          msg
        end
      end)

    Map.put(body, "messages", transformed_messages)
  else
    body
  end
end
```

**Step 2: Compile**

```bash
mix compile 2>&1
```
Expected: `mix compile: ok`

**Step 3: Commit**

```bash
git add lib/exhub/router/config.ex
git commit -m "feat: inject real reasoning_content from cache, fallback to placeholder"
```

---

### Task 5: Fix `kimi-k2.6` temperature + transform in `proxy_plug.ex`

**Files:**
- Modify: `lib/exhub/proxy_plug.ex`

**Context:** `encode_body_with_model_transforms/1` has a specific case for `"kimi-k2.5"` that sets `temperature: 1` and calls `transform_request_body`. `"kimi-k2.6"` is missing — it falls through to the default `Jason.encode!` without temperature fix or reasoning transform.

**Step 1: Merge kimi-k2.5 and kimi-k2.6 into one clause**

Replace:

```elixir
%{"model" => "kimi-k2.5"} ->
  body_params
  |> Map.put("temperature", 1)
  |> Exhub.Router.Config.transform_request_body("kimi-k2.5")
  |> Jason.encode!()
```

With:

```elixir
%{"model" => model} when model in ["kimi-k2.5", "kimi-k2.6", "inf-kimi-k2.5"] ->
  body_params
  |> Map.put("temperature", 1)
  |> Exhub.Router.Config.transform_request_body(model)
  |> Jason.encode!()
```

**Step 2: Compile**

```bash
mix compile 2>&1
```
Expected: `mix compile: ok`

**Step 3: Commit**

```bash
git add lib/exhub/proxy_plug.ex
git commit -m "fix: apply temperature=1 and reasoning transform to kimi-k2.6 and inf-kimi-k2.5"
```

---

### Task 6: Run full test suite

```bash
mix test 2>&1
```
Expected: all existing tests pass, new `ReasoningCacheTest` passes.

---

### Summary of changed files

| File | Change |
|---|---|
| `lib/exhub/router/reasoning_cache.ex` | **New** — ETS cache GenServer |
| `test/exhub/router/reasoning_cache_test.exs` | **New** — unit tests |
| `lib/exhub/application.ex` | Add `ReasoningCache` to supervision tree |
| `lib/exhub/proxy_plug.ex` | Store on response; fix kimi-k2.6 temperature/transform |
| `lib/exhub/router/config.ex` | Inject real content from cache, fallback to `"."` |
