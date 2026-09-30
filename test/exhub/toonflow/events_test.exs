defmodule Exhub.Toonflow.EventsTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Events

  describe "parse_events/1" do
    test "parses a bare JSON array" do
      raw = ~s([{"kind":"conflict","summary":"被逐出","characters":["林昭"]}])

      assert {:ok, [event]} = Events.parse_events(raw)
      assert event["kind"] == "conflict"
      assert event["summary"] == "被逐出"
      assert event["payload"] == %{"characters" => ["林昭"]}
    end

    test "parses a wrapped and fenced object" do
      raw = """
      ```json
      {"events": [{"type": "reveal", "description": "身世之谜", "importance": 5}]}
      ```
      """

      assert {:ok, [event]} = Events.parse_events(raw)
      assert event["kind"] == "reveal"
      assert event["summary"] == "身世之谜"
      assert event["payload"]["importance"] == 5
    end

    test "tolerates prose around the JSON" do
      raw = ~s(好的，以下是事件图：[{"kind":"setup","summary":"开场"}] 希望有帮助！)
      assert {:ok, [event]} = Events.parse_events(raw)
      assert event["kind"] == "setup"
    end

    test "normalizes non-map entries" do
      assert {:ok, [%{"kind" => "event", "summary" => "一句话"}]} =
               Events.parse_events(~s(["一句话"]))
    end

    test "rejects payloads without JSON" do
      assert {:error, :invalid_json} = Events.parse_events("no json here")
    end

    test "rejects JSON without an events list" do
      assert {:error, :missing_events} = Events.parse_events(~s({"foo":"bar"}))
    end
  end
end
