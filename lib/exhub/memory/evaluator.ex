defmodule Exhub.Memory.Evaluator do
  @moduledoc """
  Beacon-style trace evaluation via the Smart Decide (System One) model.

  Mirrors Beacon's Jev evaluation: three typed `noul` (yes/no) questions are
  scored against a session/lesson `state`, returning calibrated probabilities
  and a promotion verdict. A trace is promoted only when the task succeeded
  (`task_success >= 0.50`) *and* the mean of the three probabilities is at
  least `0.60` — the same gate Beacon uses.

  The model returns probabilities only; it never writes lesson text. The lesson
  body is authored by the agent and approved by the human (`Exhub.Memory.Review`).
  """

  alias Exhub.MCP.Tools.SmartDecide
  alias Exhub.Memory.Store

  @question_ids ["task_success", "reusable", "evidence_supported"]

  @defaults [
    enabled: true,
    model: "Intern-Decision-4B",
    task_success_min: 0.50,
    mean_min: 0.60,
    state_char_limit: 16_000
  ]

  @questions %{
    "task_success" => %{
      "type" => "noul",
      "instructions" =>
        "Did the session described in the state complete its engineering task " <>
          "successfully? Answer yes only if the goal was reached and verified " <>
          "(a passing test, clean build, or explicit confirmation)."
    },
    "reusable" => %{
      "type" => "noul",
      "instructions" =>
        "Does the state contain a reusable workflow, correction, debugging " <>
          "pattern, gotcha, or repository convention that would help a future " <>
          "agent working in the same repository? Answer no for one-off or " <>
          "routine work with no transferable lesson."
    },
    "evidence_supported" => %{
      "type" => "noul",
      "instructions" =>
        "Is the reusable lesson supported by concrete evidence in the state — a " <>
          "command, error message, file path, or explicit user correction — " <>
          "rather than a vague claim? When unsure, answer yes."
    }
  }

  @doc "Effective evaluator configuration."
  @spec config() :: keyword()
  def config do
    Keyword.merge(@defaults, Keyword.get(Store.config(), :evaluator, []) || [])
  end

  @doc "The fixed System One question bundle."
  @spec questions() :: map()
  def questions, do: @questions

  @doc "The question ids, in order."
  @spec question_ids() :: [String.t()]
  def question_ids, do: @question_ids

  @doc """
  Evaluate `state` and return `{:ok, evaluation}` or `{:error, reason}`.

  `evaluation` is a map with `"probabilities"` (id => probability),
  `"mean"`, `"promoted"` and the `"model"` used. Options:

    * `:decider` — `(state, questions, opts -> {:ok, result} | {:error, msg})`,
      defaults to `&Exhub.MCP.Tools.SmartDecide.decide/3`; injectable for tests.
    * `:model` — System One model id.
    * `:enabled` — set `false` to short-circuit to `{:error, :disabled}`.
  """
  @spec evaluate(term(), keyword()) :: {:ok, map()} | {:error, term()}
  def evaluate(state, opts \\ []) do
    cfg = config()
    decider = Keyword.get(opts, :decider, &SmartDecide.decide/3)
    model = Keyword.get(opts, :model, cfg[:model])

    cond do
      Keyword.get(opts, :enabled, cfg[:enabled]) == false ->
        {:error, :disabled}

      true ->
        state = state |> to_state() |> truncate(cfg[:state_char_limit])

        case safe_decide(decider, state, @questions, model) do
          {:ok, result} -> {:ok, parse(result, model, cfg)}
          {:error, reason} -> {:error, reason}
        end
    end
  end

  @doc "Whether an evaluation map was promoted past the gate."
  @spec promoted?(map()) :: boolean()
  def promoted?(%{"promoted" => promoted}), do: promoted == true
  def promoted?(_), do: false

  # ── private ──────────────────────────────────────────────────────────────

  defp safe_decide(decider, state, questions, model) do
    case decider.(state, questions, model: model) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, reason}
      other -> {:error, {:unexpected_decider_result, other}}
    end
  rescue
    e -> {:error, Exception.message(e)}
  catch
    kind, reason -> {:error, {kind, reason}}
  end

  defp parse(%{"answers" => answers}, model, cfg) when is_map(answers) do
    probabilities =
      Map.new(@question_ids, fn id -> {id, noul(Map.get(answers, id))} end)

    task_success = probabilities["task_success"]
    mean = mean(probabilities)

    promoted =
      is_number(task_success) and task_success >= cfg[:task_success_min] and
        mean >= cfg[:mean_min]

    %{
      "model" => model,
      "probabilities" => probabilities,
      "mean" => mean,
      "promoted" => promoted,
      "thresholds" => %{
        "task_success_min" => cfg[:task_success_min],
        "mean_min" => cfg[:mean_min]
      }
    }
  end

  defp parse(_result, _model, _cfg), do: %{"promoted" => false, "probabilities" => %{}}

  defp noul(answer) when is_map(answer), do: Map.get(answer, "noul")
  defp noul(_), do: nil

  defp mean(probabilities) do
    values = probabilities |> Map.values() |> Enum.filter(&is_number/1)

    case values do
      [] -> 0.0
      _ -> Enum.sum(values) / length(values)
    end
  end

  defp to_state(value) when is_binary(value), do: value
  defp to_state(value) when is_map(value) or is_list(value), do: Jason.encode!(value)
  defp to_state(value), do: to_string(value)

  defp truncate(value, limit) when is_binary(value) and is_integer(limit) do
    String.slice(value, 0, limit)
  end

  defp truncate(value, _limit), do: value
end
