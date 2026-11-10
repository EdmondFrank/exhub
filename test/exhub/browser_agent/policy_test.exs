defmodule Exhub.BrowserAgent.PolicyTest do
  use ExUnit.Case, async: true

  alias Exhub.BrowserAgent.Policy

  @targets %{
    "CLICK" => %{
      "5" => %{index: 5, ref: "e5", role: "link", label: "Search", value: nil, state: nil},
      "6" => %{index: 6, ref: "e6", role: "link", label: "Help", value: nil, state: nil}
    },
    "TYPE_TEXT" => %{
      "1" => %{
        index: 1,
        ref: "e2",
        role: "combobox",
        label: "Where from?",
        value: "San Francisco",
        state: nil
      }
    }
  }

  @page %{url: "https://flights.example", title: "Flights", text: "Search flights"}
  @elements [
    %{
      index: 1,
      ref: "e2",
      role: "combobox",
      name: "Where from?",
      value: "San Francisco",
      state: nil
    },
    %{index: 5, ref: "e5", role: "link", name: "Search", value: nil, state: nil}
  ]

  describe "operation_criteria/1" do
    test "offers only the operations with a target head, plus the controls" do
      criteria = Policy.operation_criteria(%{"CLICK" => %{}})

      assert Map.keys(criteria) |> Enum.sort() ==
               ~w(BLOCKED CLICK DONE SCROLL_DOWN SCROLL_UP WAIT)
    end
  end

  describe "build_questions/2" do
    test "asks a target head with two options and omits a single-option head" do
      questions = Policy.build_questions(@targets, "find flights")

      assert questions["operation"]["type"] == "choice"
      assert Map.has_key?(questions, "click_target")
      refute Map.has_key?(questions, "type_text_target")

      assert questions["click_target"]["criteria"] ==
               %{"5" => ~s([5] link "Search"), "6" => ~s([6] link "Help")}
    end

    test "caps target heads at the System One candidate limit" do
      head =
        for i <- 1..40, into: %{} do
          {to_string(i),
           %{index: i, ref: "e#{i}", role: "link", label: "L#{i}", value: nil, state: nil}}
        end

      questions = Policy.build_questions(%{"CLICK" => head}, "goal")

      assert map_size(questions["click_target"]["criteria"]) == Policy.max_choice_options()
      assert {:ok, _} = Exhub.MCP.Tools.SmartDecide.normalize_questions(questions)
    end

    test "produces questions Smart Decide accepts" do
      questions = Policy.build_questions(@targets, "goal")

      assert {:ok, normalized} = Exhub.MCP.Tools.SmartDecide.normalize_questions(questions)
      # operation + click_target + press_key (an editable field is present).
      assert map_size(normalized) == 3
    end
  end

  describe "build_state/3" do
    test "renders the page plus the numbered element table and recent actions" do
      state =
        Policy.build_state(@page, @elements, [%{action: "click Search", operation: "CLICK"}])

      assert state["page"]["title"] == "Flights"

      assert state["elements"] == [
               %{
                 "index" => "1",
                 "role" => "combobox",
                 "label" => "Where from?",
                 "value" => "San Francisco",
                 "state" => "",
                 "context" => ""
               },
               %{
                 "index" => "5",
                 "role" => "link",
                 "label" => "Search",
                 "value" => "",
                 "state" => "",
                 "context" => ""
               }
             ]

      assert state["recent_actions"] == [%{"action" => "click Search", "operation" => "CLICK"}]
    end
  end

  describe "validate_choice/2" do
    test "accepts an offered choice" do
      assert {:ok, "CLICK"} = Policy.validate_choice(%{"choice" => "CLICK"}, ~w(CLICK DONE))
    end

    test "rejects a choice outside the offered options" do
      assert {:error, message} = Policy.validate_choice(%{"choice" => "NOPE"}, ~w(CLICK))
      assert message =~ "outside the offered options"
    end

    test "rejects invalid probabilities" do
      answer = %{"choice" => "CLICK", "probabilities" => %{"OTHER" => 0.5}}
      assert {:error, message} = Policy.validate_choice(answer, ~w(CLICK))
      assert message =~ "invalid probabilities"
    end

    test "rejects a missing choice" do
      assert {:error, _} = Policy.validate_choice(%{}, ~w(CLICK))
    end
  end

  describe "choose/6" do
    test "selects an operation and its target in one decision" do
      {:ok, decision} =
        Policy.choose(@page, @elements, @targets, "find flights", [], decider: &decider_click/3)

      assert decision.operation == "CLICK"
      assert decision.target == "5"
      assert decision.ref == "e5"
      assert decision.label == "Search"
      assert decision.role == "link"
    end

    test "auto-selects a single-option target head without asking" do
      {:ok, decision} =
        Policy.choose(@page, @elements, @targets, "set origin", [], decider: &decider_type_text/3)

      assert decision.operation == "TYPE_TEXT"
      assert decision.target == "1"
      assert decision.ref == "e2"
    end

    test "selects a control operation with no target" do
      {:ok, decision} =
        Policy.choose(@page, @elements, @targets, "done", [], decider: &decider_done/3)

      assert decision.operation == "DONE"
      assert decision.target == nil
      assert decision.ref == nil
    end

    test "rejects an operation the model was not offered (no target head)" do
      assert {:error, message} =
               Policy.choose(@page, @elements, %{"CLICK" => @targets["CLICK"]}, "x", [],
                 decider: &decider_type_text/3
               )

      assert message =~ "outside the offered options"
    end

    test "defaults to the configured decision model (d3-turbo)" do
      parent = self()

      decider = fn _state, _questions, opts ->
        send(parent, {:decider_opts, opts})
        {:ok, %{"answers" => %{"operation" => %{"choice" => "DONE"}}}}
      end

      assert {:ok, _decision} =
               Policy.choose(@page, @elements, @targets, "done", [], decider: decider)

      assert_received {:decider_opts, opts}
      assert opts[:model] == Policy.default_model()
      assert Policy.default_model() == "d3-turbo"
    end

    test "lets an explicit model override the default" do
      parent = self()

      decider = fn _state, _questions, opts ->
        send(parent, {:decider_opts, opts})
        {:ok, %{"answers" => %{"operation" => %{"choice" => "DONE"}}}}
      end

      assert {:ok, _decision} =
               Policy.choose(@page, @elements, @targets, "done", [],
                 decider: decider,
                 model: "APUS-OpenJev-v1-4B"
               )

      assert_received {:decider_opts, opts}
      assert opts[:model] == "APUS-OpenJev-v1-4B"
    end

    test "propagates a decider error" do
      decider = fn _state, _questions, _opts -> {:error, "System One unavailable"} end

      assert {:error, "System One unavailable"} =
               Policy.choose(@page, @elements, @targets, "x", [], decider: decider)
    end
  end

  describe "PRESS" do
    test "press_available?/2 requires an editable field or a recent TYPE_TEXT" do
      assert Policy.press_available?(@targets, [])
      refute Policy.press_available?(%{"CLICK" => @targets["CLICK"]}, [])

      assert Policy.press_available?(%{"CLICK" => %{}}, [%{operation: "TYPE_TEXT"}])
      assert Policy.press_available?(%{}, [%{"operation" => "TYPE_TEXT"}])
      refute Policy.press_available?(%{}, [%{operation: "CLICK"}])
    end

    test "operation_criteria/2 offers PRESS only when a field is present" do
      criteria = Policy.operation_criteria(@targets, [])
      assert criteria["PRESS"] =~ "Press a key"

      refute Map.has_key?(Policy.operation_criteria(%{"CLICK" => %{}}, []), "PRESS")
    end

    test "build_questions/3 asks for the key with both choices" do
      questions = Policy.build_questions(@targets, "submit the filter")

      assert Map.keys(questions["press_key"]["criteria"]) |> Enum.sort() == ["Enter", "Escape"]

      assert {:ok, _} = Exhub.MCP.Tools.SmartDecide.normalize_questions(questions)
    end

    test "choose/6 turns a PRESS answer into a keyed decision" do
      {:ok, decision} =
        Policy.choose(@page, @elements, @targets, "submit", [], decider: &decider_press/3)

      assert decision.operation == "PRESS"
      assert decision.key == "Enter"
      assert decision.target == nil
      assert decision.ref == nil
      assert decision.label == "Press Enter"
      assert decision.model == Policy.default_model()
    end

    test "choose/6 rejects an unknown key instead of defaulting" do
      decider = fn _state, _questions, _opts ->
        {:ok,
         %{
           "answers" => %{
             "operation" => choice("PRESS", operations(), 0.9),
             "press_key" => choice("Space", %{"Space" => 1.0}, 1.0)
           }
         }}
      end

      assert {:error, message} =
               Policy.choose(@page, @elements, @targets, "submit", [], decider: decider)

      assert message =~ "outside the offered options"
    end
  end

  describe "row context" do
    test "target criteria carry the row text" do
      targets = %{
        "CLICK" => %{
          "1" => %{
            index: 1,
            ref: "e1",
            role: "button",
            label: "(unnamed)",
            value: nil,
            state: nil,
            context: "host-a | 10.0.81.9"
          },
          "2" => %{
            index: 2,
            ref: "e2",
            role: "button",
            label: "(unnamed)",
            value: nil,
            state: nil,
            context: "host-b | 10.0.81.10"
          }
        }
      }

      questions = Policy.build_questions(targets, "connect to host-b")

      assert questions["click_target"]["criteria"]["2"] =~
               ~s{[2] button "(unnamed)" [row: host-b | 10.0.81.10]}
    end

    test "the element table carries context" do
      element = %{
        index: 1,
        ref: "e1",
        role: "button",
        name: nil,
        value: nil,
        state: nil,
        context: "host-a"
      }

      state = Policy.build_state(@page, [element], [])
      assert hd(state["elements"])["context"] == "host-a"
    end
  end

  defp decider_press(_state, _questions, _opts) do
    {:ok,
     %{
       "answers" => %{
         "operation" => choice("PRESS", operations(), 0.9),
         "press_key" => choice("Enter", %{"Enter" => 0.9, "Escape" => 0.1}, 0.9)
       }
     }}
  end

  defp decider_click(_state, _questions, _opts) do
    {:ok,
     %{
       "answers" => %{
         "operation" => choice("CLICK", operations(), 0.9),
         "click_target" => choice("5", %{"5" => 0.95, "6" => 0.05}, 0.95)
       }
     }}
  end

  defp decider_done(_state, _questions, _opts) do
    {:ok, %{"answers" => %{"operation" => choice("DONE", operations(), 0.85)}}}
  end

  defp decider_type_text(_state, _questions, _opts) do
    {:ok, %{"answers" => %{"operation" => choice("TYPE_TEXT", operations(), 0.8)}}}
  end

  defp operations do
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
