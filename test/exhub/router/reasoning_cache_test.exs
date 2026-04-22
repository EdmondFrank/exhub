defmodule Exhub.Router.ReasoningCacheTest do
  use ExUnit.Case, async: false

  alias Exhub.Router.ReasoningCache

  setup do
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

  test "put_from_response/2 extracts tool_call ids from a non-streaming response body",
       %{cache: cache} do
    response_body =
      Jason.encode!(%{
        "choices" => [
          %{
            "message" => %{
              "role" => "assistant",
              "reasoning_content" => "Let me think...",
              "tool_calls" => [
                %{
                  "id" => "call_xyz",
                  "type" => "function",
                  "function" => %{"name" => "foo", "arguments" => "{}"}
                }
              ]
            }
          }
        ]
      })

    ReasoningCache.put_from_response(cache, response_body)
    assert ReasoningCache.get(cache, "call_xyz") == "Let me think..."
  end

  test "put_from_response/2 is a no-op when no tool_calls in response", %{cache: cache} do
    response_body =
      Jason.encode!(%{
        "choices" => [
          %{
            "message" => %{
              "role" => "assistant",
              "content" => "Hello!"
            }
          }
        ]
      })

    ReasoningCache.put_from_response(cache, response_body)
    assert ReasoningCache.get(cache, "any_id") == nil
  end

  test "put_from_response/2 handles streaming SSE body by scanning for data lines",
       %{cache: cache} do
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

  test "get_for_tool_calls/2 returns nil for empty list", %{cache: cache} do
    assert ReasoningCache.get_for_tool_calls(cache, []) == nil
  end
end
