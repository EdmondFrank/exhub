defmodule Exhub.MCP.Agent.EventsTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Agent.Events

  defp update(update_map, extra \\ %{}) do
    Map.merge(%{type: "update", update: update_map}, extra)
  end

  defp message(text, id \\ "m1") do
    update(%{
      "sessionUpdate" => "agent_message_chunk",
      "messageId" => id,
      "content" => %{"text" => text}
    })
  end

  defp thought(text) do
    update(%{
      "sessionUpdate" => "agent_thought_chunk",
      "messageId" => "m1",
      "content" => %{"text" => text}
    })
  end

  defp tool_call(id, title, kind, path) do
    locations = if path, do: [%{"path" => path}], else: []

    update(%{
      "sessionUpdate" => "tool_call",
      "toolCallId" => id,
      "title" => title,
      "kind" => kind,
      "status" => "pending",
      "locations" => locations
    })
  end

  defp tool_result(id, text, status \\ "completed") do
    update(%{
      "sessionUpdate" => "tool_call_update",
      "toolCallId" => id,
      "status" => status,
      "content" => [%{"type" => "content", "content" => %{"type" => "text", "text" => text}}]
    })
  end

  defp usage(used, size, cost) do
    update(%{
      "sessionUpdate" => "usage_update",
      "used" => used,
      "size" => size,
      "cost" => %{"amount" => cost, "currency" => "USD"}
    })
  end

  describe "digest/2" do
    test "summarizes the turn: answer, usage, tools, changed files" do
      events = [
        update(%{"sessionUpdate" => "config_option_update"}),
        thought("let me look"),
        tool_call("c1", "Read mathops.py", "read", "/tmp/mathops.py"),
        tool_result("c1", "def add(a,b):\n    return a-b\n"),
        message("Bug: `add` returns `a-b`. "),
        tool_call("c2", "Fix add", "edit", "/tmp/mathops.py"),
        tool_result("c2", "edited"),
        message("Fixed and the test passes."),
        usage(41_657, 256_000, 0.0075),
        %{type: "complete", stop_reason: "end_turn"}
      ]

      digest = Events.digest(events, agent_id: "pi", session_id: "s1", stop_reason: "end_turn")

      assert digest["agent_id"] == "pi"
      assert digest["session_id"] == "s1"
      assert digest["status"] == "complete"
      assert digest["stop_reason"] == "end_turn"
      assert digest["answer"] == "Bug: `add` returns `a-b`. Fixed and the test passes."

      assert digest["usage"] == %{
               "used" => 41_657,
               "size" => 256_000,
               "cost" => %{"amount" => 0.0075, "currency" => "USD"}
             }

      assert digest["tool_count"] == 2
      assert digest["changed_files"] == ["/tmp/mathops.py"]

      assert [
               %{"id" => "c1", "status" => "completed", "kind" => "read"},
               %{"id" => "c2", "status" => "completed", "kind" => "edit"}
             ] =
               Enum.map(digest["tools"], &Map.take(&1, ["id", "status", "kind"]))
    end

    test "keeps only the last message id, so earlier turns do not leak in" do
      events = [message("first", "m1"), message("second", "m2")]
      assert Events.digest(events)["answer"] == "second"
    end

    test "caps the answer and flags it" do
      long = String.duplicate("a", 100)
      digest = Events.digest([message(long)], max_answer_bytes: 10)

      assert digest["answer_truncated"] == true
      assert byte_size(digest["answer"]) <= 13
    end

    test "reports errors and permission requests" do
      events = [
        %{type: "permission_request", tool_call: %{"title" => "Run rm"}, options: [1, 2, 3]},
        %{type: "error", message: "agent exited"}
      ]

      digest = Events.digest(events)

      assert digest["permission_requests"] == [%{"title" => "Run rm", "options" => 3}]
      assert digest["errors"] == [%{"message" => "agent exited"}]
    end

    test "omits nil fields instead of emitting nulls" do
      digest = Events.digest([], agent_id: "pi", session_id: "s1")

      refute Map.has_key?(digest, "answer")
      refute Map.has_key?(digest, "stop_reason")
      assert digest["tool_count"] == 0
    end
  end

  describe "slim/2" do
    test "drops boilerplate and thoughts, keeps the interesting events" do
      events = [
        update(%{"sessionUpdate" => "available_commands_update"}),
        update(%{"sessionUpdate" => "session_info_update", "updatedAt" => "now"}),
        thought("thinking"),
        message("answer"),
        %{type: "complete", stop_reason: "end_turn"}
      ]

      slim = Events.slim(events)

      types =
        Enum.map(slim.events, fn event ->
          get_in(event, [:update, "sessionUpdate"]) || event.type
        end)

      assert types == ["agent_message_chunk", "complete"]
      assert slim.dropped == 3
      refute slim.truncated
    end

    test "keeps thoughts when asked" do
      slim = Events.slim([thought("thinking"), message("answer")], include_thoughts: true)
      assert length(slim.events) == 2
    end

    test "caps tool output and drops raw payloads" do
      big = String.duplicate("x", 5_000)
      event = tool_result("c1", big)
      slim = Events.slim([event], max_output_bytes: 100)

      [kept] = slim.events
      assert kept.update["output_bytes"] == 5_000
      assert kept.update["output_truncated"] == true
      assert byte_size(kept.update["output"]) <= 100 + 4
      refute Map.has_key?(kept.update, "content")
      refute Map.has_key?(kept.update, "rawOutput")
    end

    test "respects the response byte cap" do
      events = Enum.map(1..50, fn i -> message(String.duplicate("y", 1_000), "m#{i}") end)
      slim = Events.slim(events, max_bytes: 5_000)

      assert slim.truncated
      assert slim.dropped > 0
      assert length(slim.events) < 50
    end
  end

  describe "usage/1 and answer/1" do
    test "usage returns the last update" do
      assert Events.usage([usage(1, 10, 0.1), usage(2, 10, 0.2)]) == %{
               "used" => 2,
               "size" => 10,
               "cost" => %{"amount" => 0.2, "currency" => "USD"}
             }
    end

    test "usage is nil without usage events" do
      assert Events.usage([message("hi")]) == nil
    end
  end
end
