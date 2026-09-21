defmodule Exhub.MCP.Tools.SmartDecideTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.SmartDecide

  describe "normalize_state/1" do
    test "keeps plain text as-is" do
      assert SmartDecide.normalize_state("Help! My payouts have been failing for 3 days.") ==
               "Help! My payouts have been failing for 3 days."
    end

    test "decodes JSON object strings" do
      assert SmartDecide.normalize_state(~s({"amount": 42, "currency": "USD"})) ==
               %{"amount" => 42, "currency" => "USD"}
    end

    test "decodes JSON array strings" do
      assert SmartDecide.normalize_state("[1, 2, 3]") == [1, 2, 3]
    end

    test "leaves scalar-looking plain text untouched" do
      assert SmartDecide.normalize_state("123") == "123"
      assert SmartDecide.normalize_state("true") == "true"
    end

    test "passes through structured states" do
      state = %{"user" => %{"id" => 1}}

      assert SmartDecide.normalize_state(state) == state
    end

    test "falls back to the raw string for malformed JSON objects" do
      assert SmartDecide.normalize_state("{not valid json") == "{not valid json"
    end
  end

  describe "normalize_questions/1" do
    test "accepts noul, choice, and score questions" do
      questions = %{
        "is_urgent" => %{"type" => "noul", "instructions" => "Does this convey urgency?"},
        "department" => %{
          "type" => "choice",
          "instructions" => "Which team should handle this?",
          "criteria" => %{"billing" => "Payments", "technical" => "Bugs"}
        },
        "frustration" => %{
          "type" => "score",
          "instructions" => "How frustrated is the customer?",
          "criteria" => ["Calm", "Frustrated", "Very angry"]
        }
      }

      assert {:ok, normalized} = SmartDecide.normalize_questions(questions)

      assert normalized["is_urgent"] == %{
               "type" => "noul",
               "instructions" => "Does this convey urgency?"
             }

      assert normalized["department"]["criteria"] == %{
               "billing" => "Payments",
               "technical" => "Bugs"
             }

      assert normalized["frustration"]["criteria"] == ["Calm", "Frustrated", "Very angry"]
    end

    test "expands a choice criteria list into an option => option map" do
      questions = %{
        "department" => %{
          "type" => "choice",
          "instructions" => "Which team?",
          "criteria" => ["billing", "technical", "sales"]
        }
      }

      assert {:ok, normalized} = SmartDecide.normalize_questions(questions)

      assert normalized["department"]["criteria"] == %{
               "billing" => "billing",
               "technical" => "technical",
               "sales" => "sales"
             }
    end

    test "defaults blank choice descriptions to the option name" do
      questions = %{
        "department" => %{
          "type" => "choice",
          "instructions" => "Which team?",
          "criteria" => %{"billing" => "Payments", "technical" => nil, "sales" => "  "}
        }
      }

      assert {:ok, normalized} = SmartDecide.normalize_questions(questions)

      assert normalized["department"]["criteria"] == %{
               "billing" => "Payments",
               "technical" => "technical",
               "sales" => "sales"
             }
    end

    test "requires non-empty instructions" do
      assert {:error, missing} =
               SmartDecide.normalize_questions(%{"x" => %{"type" => "noul"}})

      assert missing =~ "non-empty `instructions`"

      assert {:error, blank} =
               SmartDecide.normalize_questions(%{
                 "x" => %{"type" => "noul", "instructions" => "  "}
               })

      assert blank =~ "non-empty `instructions`"
    end

    test "requires at least 2 choice options" do
      assert {:error, message} =
               SmartDecide.normalize_questions(%{
                 "x" => %{
                   "type" => "choice",
                   "instructions" => "Which?",
                   "criteria" => %{"only" => "One"}
                 }
               })

      assert message =~ "at least 2 options"
    end

    test "accepts a JSON string for questions" do
      json = ~s({"eligible": {"type": "noul", "instructions": "Within the return window?"}})

      assert {:ok, normalized} = SmartDecide.normalize_questions(json)
      assert normalized["eligible"]["type"] == "noul"
    end

    test "rejects an invalid type" do
      assert {:error, message} =
               SmartDecide.normalize_questions(%{"x" => %{"type" => "banana"}})

      assert message =~ "`x`"
      assert message =~ "expected one of noul, choice, score"
    end

    test "requires criteria for choice questions" do
      assert {:error, message} =
               SmartDecide.normalize_questions(%{
                 "x" => %{"type" => "choice", "instructions" => "Which?"}
               })

      assert message =~ "requires criteria"
    end

    test "requires criteria for score questions" do
      assert {:error, message} =
               SmartDecide.normalize_questions(%{
                 "x" => %{
                   "type" => "score",
                   "instructions" => "How much?",
                   "criteria" => ["only one"]
                 }
               })

      assert message =~ "at least 2 levels"
    end

    test "rejects a question that is not an object" do
      assert {:error, message} = SmartDecide.normalize_questions(%{"x" => "noul"})
      assert message =~ "must be an object"
    end

    test "rejects empty or non-map questions" do
      assert {:error, _} = SmartDecide.normalize_questions(%{})
      assert {:error, _} = SmartDecide.normalize_questions("not json")
      assert {:error, _} = SmartDecide.normalize_questions(nil)
    end
  end

  describe "compact_answers/1" do
    test "keeps only the chosen value for each answer type" do
      answers = %{
        "is_urgent" => %{"type" => "noul", "noul" => 0.92},
        "department" => %{
          "type" => "choice",
          "choice" => "technical",
          "probabilities" => %{"billing" => 0.08, "technical" => 0.85, "sales" => 0.07},
          "confidence" => 0.82
        },
        "frustration" => %{
          "type" => "score",
          "score" => 1.6,
          "legend" => %{"0" => "Calm", "1" => "Frustrated", "2" => "Very angry"},
          "probabilities" => %{"0" => 0.05, "1" => 0.3, "2" => 0.65},
          "confidence" => 0.78
        }
      }

      assert SmartDecide.compact_answers(answers) == %{
               "is_urgent" => %{"type" => "noul", "noul" => 0.92},
               "department" => %{"type" => "choice", "choice" => "technical"},
               "frustration" => %{"type" => "score", "score" => 1.6}
             }
    end

    test "passes through unknown answer shapes" do
      assert SmartDecide.compact_answers(%{"x" => "raw"}) == %{"x" => "raw"}
      assert SmartDecide.compact_answers(nil) == nil
    end
  end

  describe "decide/3" do
    test "validates state, questions, and model before any request" do
      assert {:error, message} = SmartDecide.decide(nil, %{"x" => %{}}, [])
      assert message =~ "`state` is required"

      assert {:error, message} = SmartDecide.decide("", %{"x" => %{}}, [])
      assert message =~ "must not be empty"

      assert {:error, message} = SmartDecide.decide("text", nil, [])
      assert message =~ "`questions` is required"

      assert {:error, message} = SmartDecide.decide("text", %{"x" => %{}}, model: "  ")
      assert message =~ "`model` must not be empty"
    end

    test "reports a missing API key" do
      assert {:error, message} = SmartDecide.decide("text", %{"x" => %{}}, api_key: "")
      assert message =~ "API key not configured"
    end
  end
end
