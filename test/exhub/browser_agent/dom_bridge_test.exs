defmodule Exhub.BrowserAgent.DomBridgeTest do
  use ExUnit.Case, async: true

  alias Exhub.BrowserAgent.{DomBridge, Snapshot}

  describe "dom_ref?/1" do
    test "accepts the DOM scheme" do
      assert DomBridge.dom_ref?("d0")
      assert DomBridge.dom_ref?("d12")
    end

    test "rejects accessibility refs and non-binaries" do
      refute DomBridge.dom_ref?("e13")
      refute DomBridge.dom_ref?("e1_24")
      refute DomBridge.dom_ref?("@e13")
      refute DomBridge.dom_ref?(nil)
      refute DomBridge.dom_ref?(7)
    end
  end

  describe "snapshot_script/0" do
    test "stamps actionable elements and returns JSON" do
      script = DomBridge.snapshot_script()

      assert script =~ DomBridge.ref_attribute()
      assert script =~ "JSON.stringify(out)"
      assert script =~ "var PREFIX = 'd';"
      assert script =~ "PREFIX + out.length"
      assert script =~ "ACTIONABLE"
    end

    test "is a single expression so eval accepts it" do
      assert String.starts_with?(String.trim(DomBridge.snapshot_script()), "(function")
    end
  end

  describe "action_script/3" do
    test "targets the stamped element and encodes the value safely" do
      script = DomBridge.action_script("d3", :fill, "he said \"hi\"\n")

      assert script =~ ~s([data-kuri-dom-ref="d3"])
      assert script =~ ~s("he said \\"hi\\"\\n")
      refute script =~ "he said \"hi\"\n"
    end

    test "clicks without a value" do
      assert DomBridge.action_script("d0", :click) =~ "el.click();"
    end

    test "refuses a ref it cannot resolve" do
      assert_raise ArgumentError, fn -> DomBridge.action_script("e13", :click) end
    end
  end

  describe "extract_json/1" do
    test "pulls the array out of surrounding noise" do
      # `kuri-agent eval` can print console noise around the result; take the
      # span between the first `[` and the last `]`.
      wrapped = ~s|debug: [{"ref":"d0","role":"link"}] trailing|

      assert {:ok, json} = DomBridge.extract_json(wrapped)
      assert Jason.decode!(json) == [%{"ref" => "d0", "role" => "link"}]
    end

    test "passes a bare array through" do
      assert {:ok, json} = DomBridge.extract_json(~s([{"ref":"d0"}]))
      assert json == ~s([{"ref":"d0"}])
    end

    test "reports payloads without an array" do
      assert {:error, _message} = DomBridge.extract_json("null")
      assert {:error, _message} = DomBridge.extract_json(nil)
    end
  end

  describe "interpret_action/1" do
    test "accepts a bare or wrapped ok" do
      assert DomBridge.interpret_action("ok") == :ok
      assert DomBridge.interpret_action(~s({"value":"ok"})) == :ok
    end

    test "reports a stale ref" do
      assert {:error, message} = DomBridge.interpret_action("missing")
      assert message =~ "no longer on the page"
    end

    test "reports empty output" do
      assert {:error, _message} = DomBridge.interpret_action(nil)
    end
  end

  describe "integration with Snapshot" do
    test "a DOM table yields the same element table and action space as an a11y snapshot" do
      payload =
        ~s([{"ref":"d0","role":"textbox","name":"Where from?","value":"San Francisco","state":null},) <>
          ~s({"ref":"d1","role":"link","name":"Search","value":null,"state":null}])

      assert {:ok, json} = DomBridge.extract_json(payload)
      elements = json |> Snapshot.parse() |> Snapshot.index()
      {indexed, targets} = Snapshot.action_space(elements)

      assert Enum.map(indexed, & &1.ref) == ["d0", "d1"]
      assert Snapshot.render(indexed) =~ ~s([1] textbox "Where from?")
      assert Map.keys(targets["CLICK"]) == ["2"]
      assert Map.keys(targets["TYPE_TEXT"]) == ["1"]
    end
  end
end
