defmodule Exhub.Sagents.EventStreamTest do
  use ExUnit.Case, async: true

  alias Exhub.Sagents.Hub

  @tool_info %{call_id: "call_1", name: "read_file", display_text: nil}

  describe "present_event/2 for a turn stream" do
    test "maps deltas and the final answer" do
      assert Hub.present_event(:turn, {:delta, "hel"}) == %{type: :delta, text: "hel"}
      assert Hub.present_event(:turn, {:complete, "hello"}) == %{type: :complete, text: "hello"}
    end

    test "maps tool execution lifecycle" do
      assert Hub.present_event(:turn, {:tool, :executing, @tool_info}) ==
               %{type: :tool_call, tool: "read_file", call_id: "call_1"}

      assert Hub.present_event(:turn, {:tool, :completed, @tool_info}) ==
               %{type: :tool_result, tool: "read_file", call_id: "call_1", status: :completed}
    end

    test "maps every terminal state, including cancel" do
      assert Hub.present_event(:turn, {:cancelled, nil}) == %{type: :cancelled}
      assert Hub.present_event(:turn, {:error, "boom"}) == %{type: :error, error: "boom"}

      assert Hub.present_event(:turn, {:paused, :needs_input}) == %{
               type: :paused,
               reason: ":needs_input"
             }

      assert Hub.present_event(:turn, {:interrupted, %{action_requests: []}}) ==
               %{type: :interrupted, interrupt: %{action_requests: []}}
    end

    test "drops transcript-only events" do
      assert Hub.present_event(:turn, {:display_message, %{id: 1}}) == nil
      assert Hub.present_event(:turn, {:status, :idle, nil}) == nil
    end
  end

  describe "present_event/2 for a follow stream" do
    test "maps the snapshot and display rows" do
      assert Hub.present_event(:follow, {:snapshot, %{messages: []}}) ==
               %{event: "snapshot", data: %{messages: []}}

      assert Hub.present_event(:follow, {:display_message, %{id: 7}}) ==
               %{event: "display_message", data: %{id: 7}}
    end

    test "folds the tool status into the payload" do
      assert Hub.present_event(:follow, {:tool, :failed, @tool_info}) ==
               %{event: "tool_status", data: Map.put(@tool_info, :status, :failed)}
    end

    test "maps status with running and alive flags" do
      assert Hub.present_event(:follow, {:status, :running, nil}) ==
               %{
                 event: "status",
                 data: %{status: :running, running: true, alive: true, payload: nil}
               }

      assert Hub.present_event(:follow, {:status, :interrupted, %{action_requests: []}}) ==
               %{
                 event: "status",
                 data: %{
                   status: :interrupted,
                   running: false,
                   alive: true,
                   payload: %{action_requests: []}
                 }
               }

      assert Hub.present_event(:follow, {:status, :idle, nil}) ==
               %{
                 event: "status",
                 data: %{status: :idle, running: false, alive: true, payload: nil}
               }

      assert Hub.present_event(:follow, {:status, :not_running, nil}) ==
               %{
                 event: "status",
                 data: %{status: :not_running, running: false, alive: false, payload: nil}
               }
    end

    test "pre-renders Markdown on text rows" do
      row = %{
        id: 1,
        content_type: "text",
        message_type: "assistant",
        content: %{"text" => "**hi**"}
      }

      assert %{event: "display_message", data: data} =
               Hub.present_event(:follow, {:display_message, row})

      assert data.html == "<p><strong>hi</strong></p>"
      assert data.content == %{"text" => "**hi**"}
    end

    test "leaves non-text rows alone" do
      row = %{id: 2, content_type: "thinking", content: %{"text" => "**hi**"}}

      assert %{event: "display_message", data: data} =
               Hub.present_event(:follow, {:display_message, row})

      refute Map.has_key?(data, :html)
    end

    test "drops turn-only events" do
      assert Hub.present_event(:follow, {:complete, "done"}) == nil
      assert Hub.present_event(:follow, {:error, "boom"}) == nil
    end
  end
end
