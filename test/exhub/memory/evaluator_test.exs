defmodule Exhub.Memory.EvaluatorTest do
  use ExUnit.Case, async: true

  alias Exhub.Memory.Evaluator

  defp decider(task, reusable, evidence) do
    fn _state, _questions, _opts ->
      {:ok,
       %{
         "answers" => %{
           "task_success" => %{"type" => "noul", "noul" => task},
           "reusable" => %{"type" => "noul", "noul" => reusable},
           "evidence_supported" => %{"type" => "noul", "noul" => evidence}
         }
       }}
    end
  end

  test "promotes when task_success and the mean pass the gate" do
    {:ok, evaluation} = Evaluator.evaluate("some session", decider: decider(0.9, 0.8, 0.7))

    assert evaluation["promoted"] == true
    assert evaluation["probabilities"]["task_success"] == 0.9
    assert evaluation["mean"] > 0.6
    assert Evaluator.promoted?(evaluation)
  end

  test "rejects when the task failed" do
    {:ok, evaluation} = Evaluator.evaluate("session", decider: decider(0.4, 0.9, 0.9))
    refute evaluation["promoted"]
  end

  test "rejects when the mean is below the gate" do
    {:ok, evaluation} = Evaluator.evaluate("session", decider: decider(0.9, 0.1, 0.1))
    refute evaluation["promoted"]
  end

  test "short-circuits when disabled" do
    assert {:error, :disabled} =
             Evaluator.evaluate("x", decider: decider(1.0, 1.0, 1.0), enabled: false)
  end

  test "propagates decider errors" do
    failing = fn _s, _q, _o -> {:error, "no api key"} end
    assert {:error, "no api key"} = Evaluator.evaluate("x", decider: failing)
  end

  test "questions/0 exposes the three fixed noul questions" do
    assert Evaluator.question_ids() == ["task_success", "reusable", "evidence_supported"]
    assert Enum.all?(Evaluator.question_ids(), &(Evaluator.questions()[&1]["type"] == "noul"))
  end
end
