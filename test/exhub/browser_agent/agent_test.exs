defmodule Exhub.BrowserAgent.AgentTest do
  use ExUnit.Case, async: true

  alias Exhub.BrowserAgent.Agent

  defmodule StubKuri do
    @moduledoc false
    def snap do
      Process.put(:snap_count, Process.get(:snap_count, 0) + 1)
      {:ok, ~s(combobox "Where from?" @e2 = San Francisco\nlink "Search" @e5)}
    end

    def text, do: {:ok, "Search flights"}
    def eval(_expression), do: {:ok, ~s({"url":"https://flights.example","title":"Flights"})}
    def click(_ref), do: {:ok, "clicked"}
    def fill(_ref, _value), do: {:ok, "filled"}
    def type(_ref, _value), do: {:ok, "typed"}
    def select(_ref, _value), do: {:ok, "selected"}

    def scroll(direction) do
      Process.put(:scrolls, Process.get(:scrolls, []) ++ [direction])
      {:ok, "scrolled"}
    end

    def go(_url), do: {:ok, "navigated"}
  end

  defmodule DomFallbackKuri do
    @moduledoc false
    # Mimics Chrome failing the accessibility snapshot on a wide DOM (e.g. the
    # DevDocs pages the `api-lookup` skill uses).
    def snap, do: {:error, "kuri: CDP command failed"}

    # A real docs page exposes both an editable search box and clickable
    # links, so the fallback table must carry both target heads for the policy.
    def dom_snapshot,
      do:
        {:ok,
         ~s|[{"ref":"d0","role":"textbox","name":"Search","value":null,"state":null},| <>
           ~s|{"ref":"d1","role":"link","name":"Enum.map (2)","value":null,"state":null}]|}

    def text, do: {:ok, "Elixir 1.20\nEnum.map (2)"}

    def eval(_expression),
      do: {:ok, ~s({"url":"https://devdocs.io/elixir~1.20/enum","title":"Enum"})}

    def click(ref), do: {:ok, "clicked " <> ref}
    def fill(ref, value), do: {:ok, "filled " <> ref <> " with " <> value}
    def type(ref, value), do: {:ok, "typed " <> ref <> " with " <> value}
    def select(ref, value), do: {:ok, "selected " <> ref <> " with " <> value}
    def scroll(_direction), do: {:ok, "scrolled"}
    def go(_url), do: {:ok, "navigated"}
  end

  defmodule BrokenKuri do
    @moduledoc false
    def snap, do: {:error, "a11y down"}
    def dom_snapshot, do: {:error, "no DOM either"}
  end

  defmodule HugeKuri do
    @moduledoc false
    # A wide-DOM equivalent: hundreds of actionable nodes and a long page, the
    # shape that overflows Smart Decide's input limit on documentation sites.
    def snap, do: {:error, "kuri: CDP command failed"}

    def dom_snapshot do
      elements =
        for i <- 1..500 do
          %{
            "ref" => "d#{i}",
            "role" => "link",
            "name" => "link #{i}",
            "value" => nil,
            "state" => nil
          }
        end

      {:ok, Jason.encode!(elements)}
    end

    def text, do: {:ok, String.duplicate("page text ", 1_000)}
    def eval(_expression), do: {:ok, ~s|{"url":"https://big.example","title":"Big"}|}
    def click(ref), do: {:ok, "clicked " <> ref}
    def fill(ref, value), do: {:ok, "filled " <> ref <> " " <> value}
    def type(ref, value), do: {:ok, "typed " <> ref <> " " <> value}
    def select(ref, value), do: {:ok, "selected " <> ref <> " " <> value}
    def scroll(_direction), do: {:ok, "scrolled"}
    def go(_url), do: {:ok, "navigated"}
  end

  defmodule MainContentKuri do
    @moduledoc false
    # A docs page: the whole-body text is navigation, but `<main>` holds the
    # article, so the loop must feed the model the article. A searchbox + link
    # keeps every operation offerable, like StubKuri.
    def snap, do: {:ok, ~s(combobox "Where from?" @e2 = San Francisco\nlink "Search" @e5)}
    def text, do: {:ok, "NAV " <> String.duplicate("nav ", 2_000)}

    def eval(expression) do
      if String.contains?(expression, "role=main") do
        {:ok, Jason.encode!(String.duplicate("article ", 1_000))}
      else
        {:ok, ~s({"url":"https://docs.example","title":"Docs"})}
      end
    end

    def click(_ref), do: {:ok, "clicked"}
    def fill(_ref, _value), do: {:ok, "filled"}
    def type(_ref, _value), do: {:ok, "typed"}
    def select(_ref, _value), do: {:ok, "selected"}
    def scroll(_direction), do: {:ok, "scrolled"}
    def go(_url), do: {:ok, "navigated"}
  end

  defmodule EmptyMainKuri do
    @moduledoc false
    # `<main>` is empty, so the loop must fall back to the whole-page text.
    def snap, do: {:ok, ~s(combobox "Where from?" @e2 = San Francisco\nlink "Search" @e5)}
    def text, do: {:ok, "WHOLE PAGE TEXT"}

    def eval(expression) do
      if String.contains?(expression, "role=main") do
        {:ok, Jason.encode!("")}
      else
        {:ok, ~s({"url":"https://x.example","title":"X"})}
      end
    end

    def click(_ref), do: {:ok, "clicked"}
    def fill(_ref, _value), do: {:ok, "filled"}
    def type(_ref, _value), do: {:ok, "typed"}
    def select(_ref, _value), do: {:ok, "selected"}
    def scroll(_direction), do: {:ok, "scrolled"}
    def go(_url), do: {:ok, "navigated"}
  end

  defmodule WideKuri do
    @moduledoc false
    # A docs page with a long sidebar: forty links after a search box. Clicking
    # never changes the page, so only sliding the offered window can reach a
    # link past the first sixteen.
    def snap do
      links = 1..40 |> Enum.map_join("\n", fn i -> ~s(link "L#{i}" @w#{i}) end)
      {:ok, ~s(combobox "Search" @w0) <> "\n" <> links}
    end

    def text, do: {:ok, "sidebar page"}
    def eval(_expression), do: {:ok, ~s({"url":"https://docs.example","title":"Docs"})}
    def click(ref), do: {:ok, "clicked " <> ref}
    def fill(_ref, _value), do: {:ok, "filled"}
    def type(_ref, _value), do: {:ok, "typed"}
    def select(_ref, _value), do: {:ok, "selected"}
    def scroll(_direction), do: {:ok, "scrolled"}
    def go(_url), do: {:ok, "navigated"}
  end

  defmodule ChangingKuri do
    @moduledoc false
    # Every click "navigates": the page text changes, so the loop must stamp
    # page_changed: true and reset the candidate window.
    def snap, do: {:ok, ~s(combobox "Search" @c1\nlink "Next" @c2)}
    def text, do: {:ok, "page " <> Integer.to_string(Process.get(:page_no, 0))}
    def eval(_expression), do: {:ok, ~s({"url":"https://x.example","title":"Changing"})}

    def click(_ref) do
      Process.put(:page_no, Process.get(:page_no, 0) + 1)
      {:ok, "clicked"}
    end

    def fill(_ref, _value), do: {:ok, "filled"}
    def type(_ref, _value), do: {:ok, "typed"}
    def select(_ref, _value), do: {:ok, "selected"}
    def scroll(_direction), do: {:ok, "scrolled"}
    def go(_url), do: {:ok, "navigated"}
  end

  defmodule FragmentKuri do
    @moduledoc false
    # A docs sidebar anchor click: the URL fragment and the focused element
    # change, but the page did not move, so the candidate window must not reset.
    def snap do
      state = if Process.get(:focused, false), do: " [focused]", else: ""
      links = 1..40 |> Enum.map_join("\n", fn i -> ~s(link "L#{i}" @f#{i}) end)
      {:ok, ~s(combobox "Search" @f0#{state}) <> "\n" <> links}
    end

    def text, do: {:ok, "article text"}

    def eval(_expression) do
      {:ok, ~s({"url":"https://docs.example/page#a#{Process.get(:frag, 0)}","title":"Docs"})}
    end

    def click(_ref) do
      Process.put(:frag, Process.get(:frag, 0) + 1)
      Process.put(:focused, true)
      {:ok, "clicked"}
    end

    def fill(_ref, _value), do: {:ok, "filled"}
    def type(_ref, _value), do: {:ok, "typed"}
    def select(_ref, _value), do: {:ok, "selected"}
    def scroll(_direction), do: {:ok, "scrolled"}
    def go(_url), do: {:ok, "navigated"}
  end

  defmodule DeepKuri do
    @moduledoc false
    # The goal-named link sits thirtieth in a forty-link sidebar (index 31,
    # after the search box); only goal-relevant ranking brings it inside the
    # sixteen-candidate head.
    def snap do
      links =
        1..40
        |> Enum.map_join("\n", fn
          30 -> ~s(link "ClassMethods#before_action" @k30)
          i -> ~s(link "Sidebar link #{i}" @k#{i})
        end)

      {:ok, ~s(combobox "Search" @k0) <> "\n" <> links}
    end

    def text, do: {:ok, "docs"}
    def eval(_expression), do: {:ok, ~s({"url":"https://docs.example/x","title":"Docs"})}
    def click(_ref), do: {:ok, "clicked"}
    def fill(_ref, _value), do: {:ok, "filled"}
    def type(_ref, _value), do: {:ok, "typed"}
    def select(_ref, _value), do: {:ok, "selected"}
    def scroll(_direction), do: {:ok, "scrolled"}
    def go(_url), do: {:ok, "navigated"}
  end

  describe "run/1" do
    test "stops on DONE after a single cycle" do
      agent = Agent.new("find flights", opts(["DONE"])) |> Agent.run()

      assert agent.status == :done
      assert length(agent.history) == 1
      assert hd(agent.history).operation == "DONE"
      assert agent.page.title == "Flights"
    end

    test "clicks the chosen target then finishes" do
      agent = Agent.new("find flights", opts(["CLICK", "DONE"])) |> Agent.run()

      assert agent.status == :done
      assert Enum.map(agent.history, & &1.operation) == ["CLICK", "DONE"]

      [click, done] = agent.history
      assert click.ref == "e5"
      assert click.label == "Search"
      assert done.target == nil
    end

    test "stops with BLOCKED when the budget is exhausted" do
      agent =
        Agent.new("loop forever", opts(["CLICK", "CLICK", "CLICK"], max_steps: 2)) |> Agent.run()

      assert agent.status == :blocked
      assert agent.error =~ "step budget"
      assert length(agent.history) == 2
    end

    test "types a generated value for TYPE_TEXT" do
      agent =
        Agent.new("set origin", opts(["TYPE_TEXT", "DONE"]))
        |> Agent.run()

      assert [typed, _done] = agent.history
      assert typed.operation == "TYPE_TEXT"
      assert typed.ref == "e2"
      assert typed.text == "San Francisco"
    end

    test "takes a fresh snapshot on every cycle" do
      agent = Agent.new("x", opts(["CLICK", "CLICK", "DONE"])) |> Agent.run()

      assert agent.status == :done
      assert Process.get(:snap_count) == 3
    end

    test "routes both scroll directions through the backend" do
      agent = Agent.new("read on", opts(["SCROLL_DOWN", "SCROLL_UP", "DONE"])) |> Agent.run()

      assert agent.status == :done
      assert Enum.map(agent.history, & &1.operation) == ["SCROLL_DOWN", "SCROLL_UP", "DONE"]
      assert Process.get(:scrolls) == [:down, :up]
    end

    test "honours the BLOCKED operation" do
      agent = Agent.new("impossible", opts(["BLOCKED"])) |> Agent.run()

      assert agent.status == :blocked
      assert agent.error == nil
    end

    test "surfaces a decider failure as an error status" do
      decider = fn _state, _questions, _opts -> {:error, "System One unavailable"} end
      agent = Agent.new("x", kuri: StubKuri, decider: decider, max_steps: 1) |> Agent.run()

      assert agent.status == :error
      assert agent.error =~ "System One unavailable"
    end
  end

  describe "candidate window" do
    test "slides the offered head when a target action changes nothing" do
      agent =
        Agent.new("open a deep link",
          kuri: WideKuri,
          decider: click_first_decider(),
          max_steps: 3
        )
        |> Agent.run()

      # No page change and no candidate left past the third window.
      assert agent.status == :blocked
      assert agent.error =~ "step budget"
      assert agent.window == 32
      assert Enum.map(agent.history, & &1.ref) == ["w1", "w17", "w33"]
      assert Enum.map(agent.history, & &1.page_changed) == [false, false, nil]
    end

    test "blocks when repeated target actions stop making progress" do
      agent =
        Agent.new("stuck", opts(["CLICK", "CLICK", "CLICK", "CLICK"], max_steps: 10))
        |> Agent.run()

      assert agent.status == :blocked
      assert agent.error =~ "no progress"
      assert length(agent.history) == 3
    end

    test "resets the window and marks page_changed when the page changes" do
      agent =
        Agent.new("paging", opts(["CLICK", "CLICK", "DONE"], kuri: ChangingKuri)) |> Agent.run()

      assert agent.status == :done
      assert agent.window == 0
      assert Enum.map(agent.history, & &1.page_changed) == [true, true, nil]
    end

    test "a focus or URL-fragment change is not a page change" do
      agent =
        Agent.new("anchor",
          kuri: FragmentKuri,
          decider: click_first_decider(),
          max_steps: 3
        )
        |> Agent.run()

      # Every click changed only the fragment and the focused node, so the
      # window should slide on rather than reset.
      assert Enum.map(agent.history, & &1.page_changed) == [false, false, nil]
      assert agent.window == 32
    end

    test "offers a goal-named element that sits deep in the page" do
      parent = self()

      decider = fn _state, questions, _opts ->
        send(parent, {:click_head, Map.keys(questions["click_target"]["criteria"])})
        {:ok, %{"answers" => %{"operation" => choice("DONE", offered(questions), 0.9)}}}
      end

      agent =
        Agent.new("open the documentation for before_action",
          kuri: DeepKuri,
          decider: decider,
          max_steps: 1
        )
        |> Agent.run()

      assert agent.status == :done
      assert_received {:click_head, head}
      assert "31" in head
    end
  end

  describe "DOM fallback" do
    test "observes via the DOM table when the accessibility snapshot fails" do
      agent = Agent.new("read the docs", opts(["DONE"], kuri: DomFallbackKuri)) |> Agent.run()

      assert agent.status == :done
      assert agent.observation == "dom"
      assert agent.page.title == "Enum"

      snapshot = Agent.snapshot(agent)
      assert snapshot["observation"] == "dom"
      assert snapshot["elements"] =~ ~s|[2] link "Enum.map (2)"|
    end

    test "executes a DOM ref chosen by the policy" do
      agent =
        Agent.new("open the result", opts(["CLICK", "DONE"], kuri: DomFallbackKuri))
        |> Agent.run()

      assert agent.status == :done
      assert Enum.map(agent.history, & &1.operation) == ["CLICK", "DONE"]
      assert hd(agent.history).ref == "d1"
    end

    test "reports both failures when the DOM fallback fails too" do
      agent = Agent.new("x", opts([], kuri: BrokenKuri)) |> Agent.run()

      assert agent.status == :error
      assert agent.error =~ "a11y down"
      assert agent.error =~ "no DOM either"
    end
  end

  describe "observation budget" do
    test "bounds the element table and page text so the prompt fits the model" do
      parent = self()

      decider = fn state, questions, _opts ->
        send(parent, {:state, state})
        {:ok, %{"answers" => %{"operation" => choice("DONE", offered(questions), 0.9)}}}
      end

      agent = Agent.new("browse", kuri: HugeKuri, decider: decider, max_steps: 1) |> Agent.run()

      assert agent.status == :done
      assert agent.observation == "dom"
      assert length(agent.elements) <= 120

      snapshot = Agent.snapshot(agent)
      assert length(String.split(snapshot["elements"], "\n")) <= 120

      assert_received {:state, state}
      assert String.length(state["page"]["text"]) <= 4000

      # ~4 bytes/token: stay comfortably under the 8191-token input limit.
      assert state |> Jason.encode!() |> byte_size() < 20_000
    end

    test "honours an explicit max_elements" do
      decider = fn _state, questions, _opts ->
        {:ok, %{"answers" => %{"operation" => choice("DONE", offered(questions), 0.9)}}}
      end

      agent =
        Agent.new("browse", kuri: HugeKuri, decider: decider, max_steps: 1, max_elements: 5)
        |> Agent.run()

      assert agent.status == :done
      assert length(agent.elements) == 5
    end

    test "keeps the observation when prediction fails" do
      decider = fn _state, _questions, _opts -> {:error, "System One unavailable"} end

      agent = Agent.new("x", kuri: DomFallbackKuri, decider: decider, max_steps: 1) |> Agent.run()

      assert agent.status == :error
      assert agent.observation == "dom"
      assert agent.page.title == "Enum"

      snapshot = Agent.snapshot(agent)
      assert snapshot["observation"] == "dom"
      assert snapshot["elements"] =~ "Enum.map (2)"
    end
  end

  describe "page text" do
    test "prefers the main content pane over the whole-page text" do
      agent = Agent.new("read the docs", opts(["DONE"], kuri: MainContentKuri)) |> Agent.run()

      assert agent.status == :done
      assert agent.page.text =~ "article "
      refute agent.page.text =~ "NAV"
      assert String.length(agent.page.text) <= 4000
    end

    test "falls back to the whole-page text when the main pane is empty" do
      agent = Agent.new("read the docs", opts(["DONE"], kuri: EmptyMainKuri)) |> Agent.run()

      assert agent.page.text == "WHOLE PAGE TEXT"
    end
  end

  describe "snapshot/1" do
    test "returns a JSON-ready view of the agent" do
      agent = Agent.new("find flights", opts(["DONE"])) |> Agent.run()
      snapshot = Agent.snapshot(agent)

      assert snapshot["status"] == "done"
      assert snapshot["title"] == "Flights"
      assert snapshot["observation"] == "a11y"
      assert snapshot["step"] == 1
      assert snapshot["elements"] =~ ~s([2] link "Search")
      assert snapshot["text"] == "Search flights"
    end
  end

  # --- helpers ---

  # `Keyword.merge/2` so callers can override the default stub backend (the
  # default `kuri` would otherwise shadow an override, since `Keyword.get/3`
  # returns the first match).
  defp opts(operations, extra \\ []) do
    Process.put(:step_counter, 0)

    [
      decider: fn state, questions, decider_opts ->
        decide(state, questions, decider_opts, operations)
      end,
      generator: fn _context -> {:ok, "San Francisco", %{model: "stub"}} end
    ]
    |> Keyword.merge(kuri: StubKuri)
    |> Keyword.merge(extra)
  end

  defp decide(_state, _questions, _opts, operations) do
    step = Process.get(:step_counter, 0)
    Process.put(:step_counter, step + 1)
    {:ok, %{"answers" => answer(Enum.at(operations, step) || "BLOCKED")}}
  end

  defp answer("CLICK") do
    %{
      "operation" => choice("CLICK", operations_map(), 0.9),
      "click_target" => choice("2", %{"2" => 0.9, "1" => 0.1}, 0.9),
      "type_text_target" => choice("1", %{"1" => 0.05}, 0.05)
    }
  end

  defp answer("TYPE_TEXT") do
    %{
      "operation" => choice("TYPE_TEXT", operations_map(), 0.9),
      "type_text_target" => choice("1", %{"1" => 0.9}, 0.9),
      "click_target" => choice("2", %{"2" => 0.05}, 0.05)
    }
  end

  defp answer(operation) do
    %{"operation" => choice(operation, operations_map(), 0.9)}
  end

  # The lowest offered target index of `key` (e.g. "click_target").
  defp first_target(questions, key) do
    questions[key]["criteria"]
    |> Map.keys()
    |> Enum.min_by(&String.to_integer/1)
  end

  # Always clicks the lowest offered index, so a step's ref reveals which
  # candidate window the model was shown.
  defp click_first_decider do
    fn _state, questions, _opts ->
      target = first_target(questions, "click_target")

      {:ok,
       %{
         "answers" => %{
           "operation" => choice("CLICK", offered(questions), 0.9),
           "click_target" => choice(target, %{target => 0.9}, 0.9)
         }
       }}
    end
  end

  # A probability map over exactly the offered operation ids (a model never
  # assigns mass to an option it was not shown).
  defp offered(questions) do
    ids = Map.keys(questions["operation"]["criteria"])
    Map.new(ids, fn id -> {id, 1 / length(ids)} end)
  end

  defp operations_map do
    %{
      "CLICK" => 0.3,
      "TYPE_TEXT" => 0.3,
      "SCROLL_UP" => 0.05,
      "SCROLL_DOWN" => 0.05,
      "WAIT" => 0.1,
      "DONE" => 0.1,
      "BLOCKED" => 0.1
    }
  end

  defp choice(value, probabilities, confidence) do
    %{
      "type" => "choice",
      "choice" => value,
      "probabilities" => probabilities,
      "confidence" => confidence
    }
  end
end
