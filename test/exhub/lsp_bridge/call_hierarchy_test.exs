defmodule Exhub.LspBridge.CallHierarchyTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.Handlers.{
    CallHierarchyIncoming,
    CallHierarchyOutgoing,
    CallHierarchyPrepare
  }

  @ctx %{path: "/proj/lib/a.ex", language_id: "elixir", args: %{}}

  defp item do
    %{
      "name" => "hello",
      "kind" => 12,
      "uri" => "file:///proj/lib/a.ex",
      "range" => %{"start" => %{"line" => 1, "character" => 0}},
      "selectionRange" => %{"start" => %{"line" => 1, "character" => 4}},
      "data" => %{"opaque" => true}
    }
  end

  describe "CallHierarchyPrepare" do
    test "metadata" do
      assert CallHierarchyPrepare.name() == "call-hierarchy-prepare"
      assert CallHierarchyPrepare.method() == "textDocument/prepareCallHierarchy"
      assert CallHierarchyPrepare.provider() == "call_hierarchy"
      refute CallHierarchyPrepare.cancel_on_change?()
    end

    test "request params carry the position" do
      pos = %{"line" => 3, "character" => 2}

      assert CallHierarchyPrepare.request_params(%{"position" => pos}, @ctx) == %{
               "position" => pos
             }
    end

    test "items pass through unmodified (so `data' round-trips)" do
      i = item()

      assert {:call_hierarchy_items, "/proj/lib/a.ex", [^i]} =
               CallHierarchyPrepare.process_response([i], @ctx)
    end

    test "empty/nil yields a message" do
      assert CallHierarchyPrepare.process_response([], @ctx) ==
               {:message, "No call hierarchy items."}

      assert CallHierarchyPrepare.process_response(nil, @ctx) ==
               {:message, "No call hierarchy items."}
    end
  end

  describe "CallHierarchyIncoming" do
    test "metadata" do
      assert CallHierarchyIncoming.name() == "call-hierarchy-incoming"
      assert CallHierarchyIncoming.method() == "callHierarchy/incomingCalls"
      assert CallHierarchyIncoming.provider() == "call_hierarchy"
    end

    test "request params carry the item" do
      assert CallHierarchyIncoming.request_params(%{"item" => item()}, @ctx) == %{
               "item" => item()
             }
    end

    test "calls become a call_hierarchy payload with the direction" do
      calls = [%{"from" => item(), "fromRanges" => [%{"start" => %{"line" => 0}}]}]

      assert CallHierarchyIncoming.process_response(calls, @ctx) ==
               {:call_hierarchy, "/proj/lib/a.ex", "incoming", calls}
    end

    test "empty/nil yields a message" do
      assert CallHierarchyIncoming.process_response([], @ctx) == {:message, "No incoming calls."}
      assert CallHierarchyIncoming.process_response(nil, @ctx) == {:message, "No incoming calls."}
    end
  end

  describe "CallHierarchyOutgoing" do
    test "metadata" do
      assert CallHierarchyOutgoing.name() == "call-hierarchy-outgoing"
      assert CallHierarchyOutgoing.method() == "callHierarchy/outgoingCalls"
      assert CallHierarchyOutgoing.provider() == "call_hierarchy"
    end

    test "calls become a call_hierarchy payload with the direction" do
      calls = [%{"to" => item(), "fromRanges" => []}]

      assert CallHierarchyOutgoing.process_response(calls, @ctx) ==
               {:call_hierarchy, "/proj/lib/a.ex", "outgoing", calls}
    end

    test "empty/nil yields a message" do
      assert CallHierarchyOutgoing.process_response([], @ctx) == {:message, "No outgoing calls."}
    end
  end
end
