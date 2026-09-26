defmodule Exhub.BrowserAgent.KuriHttpTest do
  use ExUnit.Case, async: true

  alias Exhub.BrowserAgent.{DomBridge, KuriHttp}

  defmodule FakeHttp do
    @moduledoc false
    # Answers from the test process dictionary (the calls are synchronous) and
    # records each request in the caller's mailbox for assertions.
    def get(url, headers, opts) do
      send(self(), {:http, url, headers, opts})
      Process.get(:http_response, {:error, :no_stub})
    end
  end

  @tab "TAB123"

  setup do
    Process.put(:http_response, {:ok, 200, "{}"})
    on_exit(fn -> Process.delete(:http_response) end)
    :ok
  end

  describe "snap/1" do
    test "keeps only actionable roles and drops unregistered inline-text refs" do
      Process.put(
        :http_response,
        {:ok, 200,
         Jason.encode!([
           %{"ref" => "e1_24", "role" => "RootWebArea", "name" => "Array.prototype.map"},
           %{"ref" => "e1_i280", "role" => "textbox", "name" => "code sample"},
           %{"ref" => "e1_45", "role" => "link", "name" => "Skip to main content"},
           %{"ref" => "e1_9", "role" => "button", "name" => "Run"}
         ])}
      )

      assert {:ok, elements} = KuriHttp.snap(opts())
      assert Enum.map(elements, & &1["ref"]) == ["e1_45", "e1_9"]
    end

    test "keeps a non-empty table for pages with no controls" do
      Process.put(
        :http_response,
        {:ok, 200, Jason.encode!([%{"ref" => "e1_24", "role" => "RootWebArea"}])}
      )

      assert {:ok, [%{"ref" => "e1_24"}]} = KuriHttp.snap(opts())
    end

    test "authorizes and scopes the request to the attached tab" do
      assert {:ok, _elements} = KuriHttp.snap(opts())

      assert_received {:http, url, headers, request}
      assert url == "http://kuri.example/snapshot"
      assert headers == [{"authorization", "Bearer test-token"}]
      assert request[:params] == [tab_id: @tab]
    end
  end

  describe "actions" do
    test "an accessibility ref goes to /action" do
      Process.put(:http_response, {:ok, 200, ~s({"ok":true,"action":"clicked"})})

      assert {:ok, body} = KuriHttp.click("e1_45", opts())
      assert Jason.decode!(body)["action"] == "clicked"

      assert_received {:http, url, _headers, request}
      assert url == "http://kuri.example/action"
      assert request[:params][:ref] == "e1_45"
      assert request[:params][:action] == "click"
    end

    test "a DOM ref replays through /evaluate" do
      Process.put(:http_response, {:ok, 200, script_result("ok")})

      assert {:ok, message} = KuriHttp.click("d3", opts())
      assert message == "dom click d3"

      assert_received {:http, url, _headers, request}
      assert url == "http://kuri.example/evaluate"
      assert request[:params][:expression] =~ ~s([data-kuri-dom-ref="d3"])
    end

    test "filling a DOM ref carries the value" do
      Process.put(:http_response, {:ok, 200, script_result("ok")})

      assert {:ok, _message} = KuriHttp.fill("d7", "San Francisco", opts())

      assert_received {:http, _url, _headers, request}
      assert request[:params][:expression] =~ ~s|setValue("San Francisco")|
    end

    test "a stale DOM ref is reported instead of silently no-op'ing" do
      Process.put(:http_response, {:ok, 200, script_result("missing")})

      assert {:error, message} = KuriHttp.fill("d3", "x", opts())
      assert message =~ "no longer on the page"
    end
  end

  describe "script results" do
    test "text unwraps the nested daemon envelope" do
      Process.put(:http_response, {:ok, 200, script_result("Skip to main content")})

      assert {:ok, "Skip to main content"} = KuriHttp.text(opts())
    end

    test "dom_snapshot returns the extracted JSON array" do
      Process.put(
        :http_response,
        {:ok, 200, script_result(~s([{"ref":"d0","role":"link","name":"Search"}]))}
      )

      assert {:ok, json} = KuriHttp.dom_snapshot(opts())
      assert Jason.decode!(json) == [%{"ref" => "d0", "role" => "link", "name" => "Search"}]
    end

    test "an undefined script result is empty, not an error" do
      Process.put(:http_response, {:ok, 200, ~s({"result":{"result":{"type":"undefined"}}})})

      assert {:ok, ""} = KuriHttp.eval("nope", opts())
    end

    test "scrolling targets the real scroller, not the window" do
      Process.put(
        :http_response,
        {:ok, 200, ~s({"result":{"result":{"type":"object","value":{}}}})}
      )

      assert {:ok, "scrolled down"} = KuriHttp.scroll(:down, opts())

      assert_received {:http, url, _headers, request}
      assert url == "http://kuri.example/evaluate"
      assert request[:params][:expression] =~ "scrollTop"
      refute request[:params][:expression] =~ "window.scrollBy"
    end

    test "a boolean script result is preserved, not read as missing" do
      Process.put(:http_response, {:ok, 200, script_result(false)})

      assert {:ok, "false"} = KuriHttp.eval("nope", opts())
    end

    test "a malformed script payload is an error" do
      Process.put(:http_response, {:ok, 200, ~s({"nope":true})})

      assert {:error, message} = KuriHttp.eval("nope", opts())
      assert message =~ "carried no value"
    end
  end

  describe "failures" do
    test "daemon error payloads surface as errors" do
      Process.put(:http_response, {:ok, 200, ~s({"error":"CDP command failed"})})

      assert {:error, message} = KuriHttp.snap(opts())
      assert message == "kuri: CDP command failed"
    end

    test "non-2xx responses surface with their status" do
      Process.put(:http_response, {:ok, 401, ~s({"error":"Unauthorized"})})

      assert {:error, message} = KuriHttp.snap(opts())
      assert message =~ "401"
    end

    test "transport failures surface with the reason" do
      Process.put(:http_response, {:error, :econnrefused})

      assert {:error, message} = KuriHttp.text(opts())
      assert message =~ "econnrefused"
    end

    test "invalid JSON surfaces as an error" do
      Process.put(:http_response, {:ok, 200, "<html>nope</html>"})

      assert {:error, message} = KuriHttp.snap(opts())
      assert message =~ "invalid JSON"
    end
  end

  describe "tab_id/1" do
    test "an explicit tab_id wins" do
      assert KuriHttp.tab_id(tab_id: @tab) == {:ok, @tab}
    end
  end

  describe "DOM bridge agreement" do
    test "the DOM snapshot script used here is the shared one" do
      Process.put(:http_response, {:ok, 200, script_result("[]")})

      assert {:ok, _json} = KuriHttp.dom_snapshot(opts())

      assert_received {:http, _url, _headers, request}
      assert request[:params][:expression] == DomBridge.snapshot_script()
    end
  end

  # --- helpers ---

  defp opts,
    do: [http: FakeHttp, token: "test-token", base_url: "http://kuri.example", tab_id: @tab]

  defp script_result(value) do
    Jason.encode!(%{
      "id" => 1,
      "result" => %{"result" => %{"type" => "string", "value" => value}}
    })
  end
end
