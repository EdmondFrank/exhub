defmodule Exhub.BrowserAgent.BrowserMcpTest do
  use ExUnit.Case, async: true

  alias Exhub.BrowserAgent.{BrowserMcp, Kuri, Snapshot}

  @snapshot "\u2713 snapshot -i\n" <>
              "- textbox \"Customer name:\" [ref=e5]: Ada\n" <>
              "- group \"Pizza Size\" [ref=e12]:\n" <>
              "  - radio \"Small\" [checked] [active] [ref=e16]\n" <>
              "  - radio \"Medium\" [ref=e19]\n" <>
              "- link \"Learn more\" [url=https://iana.org/help] [ref=e13]\n" <>
              "- button \"Submit order\" [ref=e44]\n" <>
              "Page: https://httpbin.org/forms/post \u2014 \"\""

  @rows """
  ✓ snapshot -i
  - row
    - cell "host-a"
    - cell "10.0.81.9"
    - button [ref=e1]
  - row
    - cell "host-b"
    - cell "10.0.81.10"
    - button [ref=e2]
  Page: https://jump.example — "Assets"
  """

  # Queues canned `browser` tool outputs and records the commands sent.
  defp stub(responses) do
    Process.put(:browser_responses, responses)
    Process.put(:browser_commands, [])

    fn %{"commands" => commands} ->
      [next | rest] = Process.get(:browser_responses)
      Process.put(:browser_responses, rest)
      Process.put(:browser_commands, Process.get(:browser_commands) ++ [commands])
      next
    end
  end

  defp commands, do: Process.get(:browser_commands)

  describe "snap/1" do
    test "parses Browser MCP snapshot lines into the Jev table" do
      call = stub([{:ok, @snapshot}])

      assert {:ok, nodes} = BrowserMcp.snap(call: call)
      assert Enum.map(nodes, & &1["ref"]) == ["e5", "e16", "e19", "e13", "e44"]

      name = Enum.find(nodes, &(&1["ref"] == "e5"))
      assert name["role"] == "textbox"
      assert name["name"] == "Customer name:"
      assert name["value"] == "Ada"

      small = Enum.find(nodes, &(&1["ref"] == "e16"))
      assert small["state"] == "checked"
      assert small["depth"] == 1
    end

    test "drops roles the policy cannot act on, and attribute noise" do
      call = stub([{:ok, @snapshot}])
      assert {:ok, nodes} = BrowserMcp.snap(call: call)

      refute Enum.any?(nodes, &(&1["role"] == "group"))
      assert Enum.find(nodes, &(&1["ref"] == "e13"))["state"] == ""
    end

    test "feeds Exhub.BrowserAgent.Snapshot, so target heads are built" do
      call = stub([{:ok, @snapshot}])
      assert {:ok, payload} = BrowserMcp.snap(call: call)

      {indexed, targets} =
        payload |> Snapshot.parse() |> Snapshot.index() |> Snapshot.action_space()

      assert length(indexed) == 5
      assert Map.keys(targets) |> Enum.sort() == ["CLICK", "TYPE_TEXT"]
      assert targets["TYPE_TEXT"]["1"][:ref] == "e5"
    end

    test "keeps the raw table when a page has no controls" do
      call =
        stub([
          {:ok, "\u2713 snapshot -i\n- heading \"Docs\" [ref=e1]\nPage: https://x \u2014 \"X\""}
        ])

      assert {:ok, [%{"ref" => "e1", "role" => "heading"}]} = BrowserMcp.snap(call: call)
    end

    test "propagates the tool error" do
      call = stub([{:error, "no tab yet"}])

      assert {:error, message} = BrowserMcp.snap(call: call)
      assert message == "no tab yet"
    end
  end

  describe "acting on a target" do
    test "verifies the ref, then acts" do
      call = stub([{:ok, @snapshot}, {:ok, "\u2713 click e16\nPage: https://x \u2014 \"X\""}])

      assert {:ok, _} = BrowserMcp.click("e16", call: call)
      assert commands() == ["snapshot -i", "click e16"]
    end

    test "refuses a ref that vanished from the page" do
      call =
        stub([
          {:ok, "\u2713 snapshot -i\n- button \"Other\" [ref=e44]\nPage: https://x \u2014 \"X\""}
        ])

      assert {:error, message} = BrowserMcp.click("e16", call: call)
      assert message =~ "e16 is no longer on the page"
      assert commands() == ["snapshot -i"]
    end

    test "refuses a ref that now names another element" do
      before_page =
        "\u2713 snapshot -i\n- radio \"Small\" [ref=e16]\nPage: https://x \u2014 \"X\""

      after_page =
        "\u2713 snapshot -i\n- button \"Delete\" [ref=e16]\nPage: https://x \u2014 \"X\""

      call = stub([{:ok, before_page}, {:ok, after_page}])

      assert {:ok, _} = BrowserMcp.snap(call: call)
      assert {:error, message} = BrowserMcp.click("e16", call: call)
      assert message =~ "now names button"
      refute "click e16" in commands()
    end

    test "verify: false skips the re-read" do
      call = stub([{:ok, "\u2713 click e16\nPage: https://x \u2014 \"X\""}])

      assert {:ok, _} = BrowserMcp.click("e16", call: call, verify: false)
      assert commands() == ["click e16"]
    end

    test "fill quotes a value with spaces" do
      call = stub([{:ok, "\u2713 fill\nPage: https://x \u2014 \"X\""}])

      assert {:ok, _} = BrowserMcp.fill("e5", "Ada Lovelace", call: call, verify: false)
      assert commands() == ["fill e5 \"Ada Lovelace\""]
    end

    test "press sends the native key command" do
      call = stub([{:ok, "✓ press Enter\nPage: https://x — \"X\""}])

      assert {:ok, _} = BrowserMcp.press("Enter", call: call)
      assert commands() == ["press Enter"]
    end

    test "follow_new_tab switches to the tab an action opened" do
      listing = """
      ✓ tab list
        1 "Access assets - JumpServer" — https://jump.example/assets
        2 "攻击分析主机" — https://jump.example/luna/session/1
      Page: https://jump.example/assets — "Assets"
      """

      call = stub([{:ok, listing}, {:ok, "✓ tab select 2\nPage: https://x — \"X\""}])

      assert {:ok, "followed new tab 2"} =
               BrowserMcp.follow_new_tab(call: call, before: ["1"])

      assert commands() == ["tab list", "tab select 2"]
    end

    test "follow_new_tab does nothing when no tab appeared" do
      listing = """
      ✓ tab list
        1 "Access assets - JumpServer" — https://jump.example/assets
      Page: https://jump.example/assets — "Assets"
      """

      call = stub([{:ok, listing}])

      assert :none = BrowserMcp.follow_new_tab(call: call, before: ["1"])
      assert commands() == ["tab list"]
    end

    test "scroll maps the policy directions" do
      call = stub([{:ok, "\u2713 scroll up\nPage: https://x \u2014 \"X\""}])

      assert {:ok, _} = BrowserMcp.scroll(:up, call: call)
      assert commands() == ["scroll up"]
    end
  end

  describe "reading the page" do
    test "text drops the command echo, banner and truncation note" do
      text =
        "\u2713 get text --max 8000\nHello\nworld\n\n\u2026 cut at 12 of 900\nPage: https://x \u2014 \"X\""

      call = stub([{:ok, text}])
      assert {:ok, "Hello\nworld"} = BrowserMcp.text(call: call)
    end

    test "eval unwraps the JSON layer the tool prints" do
      inner = Jason.encode!(%{"url" => "https://example.com/"})
      raw = "\u2713 eval\n" <> Jason.encode!(inner) <> "\nPage: https://x \u2014 \"X\""
      call = stub([{:ok, raw}])

      assert {:ok, ^inner} = BrowserMcp.eval("JSON.stringify({url: location.href})", call: call)
      assert commands() == ["eval \"JSON.stringify({url: location.href})\""]
    end

    test "eval sends a multi-line script as one quoted argument" do
      script =
        Enum.join(
          [
            "() => {",
            "  var el = document.querySelector('main, [role=main]');",
            "  return el ? el.innerText : '';",
            "}"
          ],
          "\n"
        )

      call = stub([{:ok, "\u2713 eval\n\"\"\nPage: https://x \u2014 \"X\""}])
      assert {:ok, ""} = BrowserMcp.eval(script, call: call)

      assert [command] = commands()
      assert command =~ "querySelector('main, [role=main]')"
      refute command =~ "\n"
    end

    test "console and tabs pass their output through" do
      call = stub([{:ok, "\u2713 console --errors\n(no messages)\nPage: https://x \u2014 \"X\""}])
      assert {:ok, "(no messages)"} = BrowserMcp.console(call: call)

      tabs =
        "\u2713 tab list\n  931804983 \"Example Domain\" \u2014 https://example.com/\nPage: https://x"

      call = stub([{:ok, tabs}])
      assert {:ok, listing} = BrowserMcp.tabs(call: call)
      assert listing =~ "931804983"
    end
  end

  describe "row context and icon hints" do
    test "attaches the row text so icon-only buttons are distinguishable" do
      call =
        stub([
          {:ok, @rows},
          {:ok,
           "✓ eval\n[[\"host-a | 10.0.81.9\", \"desktop icon\"], [\"host-b | 10.0.81.10\", \"star-o icon\"]]\nPage: https://x — \"X\""}
        ])

      assert {:ok, nodes} = BrowserMcp.snap(call: call)
      assert Enum.find(nodes, &(&1["ref"] == "e1"))["context"] == "host-a | 10.0.81.9"
      assert Enum.find(nodes, &(&1["ref"] == "e2"))["context"] == "host-b | 10.0.81.10"
    end

    test "names icon-only buttons from their icon so two row actions differ" do
      page = """
      ✓ snapshot -i
      - row [ref=e1]:
        - cell "host-a" [ref=e2]
        - button [ref=e3]
        - button [ref=e4]
      Page: https://x — "X"
      """

      call =
        stub([
          {:ok, page},
          {:ok,
           "✓ eval\n[[\"host-a\", \"desktop icon\"], [\"host-a\", \"star-o icon\"]]\nPage: https://x — \"X\""}
        ])

      assert {:ok, nodes} = BrowserMcp.snap(call: call)
      assert Enum.find(nodes, &(&1["ref"] == "e3"))["name"] == "desktop icon"
      assert Enum.find(nodes, &(&1["ref"] == "e4"))["name"] == "star-o icon"
      assert Enum.find(nodes, &(&1["ref"] == "e3"))["context"] == "host-a"
    end

    test "leaves an unnamed button alone when no hint matches its row" do
      page = """
      ✓ snapshot -i
      - row [ref=e1]:
        - cell "host-a" [ref=e2]
        - button [ref=e3]
        - button [ref=e4]
      Page: https://x — "X"
      """

      call =
        stub([
          {:ok, page},
          {:ok, "✓ eval\n[[\"other row\", \"desktop icon\"]]\nPage: https://x — \"X\""}
        ])

      assert {:ok, nodes} = BrowserMcp.snap(call: call)
      assert Enum.find(nodes, &(&1["ref"] == "e3"))["name"] == nil
      assert Enum.find(nodes, &(&1["ref"] == "e4"))["name"] == nil
    end

    test "parses a container's role without the trailing colon" do
      [row, cell] = BrowserMcp.parse_snapshot("- row [ref=e1]:\n  - cell \"x\" [ref=e2]")

      assert row["role"] == "row"
      assert cell["role"] == "cell"
    end
  end

  describe "facade wiring" do
    test "the Kuri facade can select this backend" do
      assert Kuri.backend_for(:chrome) == BrowserMcp
    end
  end
end
