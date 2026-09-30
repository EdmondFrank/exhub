defmodule Exhub.MCP.Tools.Memory.Distill do
  @moduledoc "MCP Tool: memory_distill — turn a session/lesson into a memory candidate."

  alias Exhub.Memory.Evaluator
  alias Exhub.Memory.SecretScan
  alias Exhub.Memory.Store
  alias Exhub.MCP.Tools.Memory.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "memory_distill"

  @impl true
  def description do
    """
    Turn a session, fix, or debugging effort into a reviewable project-memory
    candidate (Beacon's "distill" step).

    The evaluator (Smart Decide / System One) scores the supplied `summary`
    against three yes/no questions — did the task succeed, is there a reusable
    lesson, is it evidence-backed — and creates a candidate only when it passes
    the gate (task_success >= 0.50 and mean >= 0.60). Set `evaluate: false` to
    skip the networked evaluation and record the candidate directly (use this
    when no evaluator key is configured).

    The candidate is NOT memory yet: a person must approve it with
    `memory_approve`. Never put secrets in `summary`, `title`, or `body` — the
    tool refuses anything that looks like a credential.
    """
  end

  schema do
    field(:summary, {:required, :string},
      description: "The session/lesson content to evaluate and distill (the 'state')."
    )

    field(:title, :string, description: "Lesson title (default: first line of summary)")

    field(:kind, :string,
      description: "Memory kind: workflow | correction | debugging_pattern | gotcha | convention",
      default: "workflow"
    )

    field(:applicability, :string, description: "When this lesson applies")

    field(:body, :string, description: "Explicit lesson body (default: the summary itself)")

    field(:project, :string, description: "Project this memory is scoped to")

    field(:tags, :any, description: "List of tags (or a comma-separated string)")

    field(:source, :string, description: "Provenance, e.g. a session id")

    field(:evidence, :any, description: "Structured evidence, e.g. trace event refs")

    field(:evaluate, :boolean,
      description: "Run the System One evaluation gate (default: true)",
      default: true
    )

    field(:force, :boolean,
      description:
        "Create the candidate even when the evaluation did not promote it (default: false)",
      default: false
    )
  end

  @impl true
  def execute(params, frame) do
    summary = params |> Map.get(:summary) |> to_string()
    kind = Map.get(params, :kind, "workflow")
    body = Map.get(params, :body) || summary
    title = Map.get(params, :title) || Helpers.default_title(summary)
    tags = Helpers.str_list(Map.get(params, :tags))
    applicability = Map.get(params, :applicability)
    project = Map.get(params, :project)
    source = Map.get(params, :source)
    evidence = Map.get(params, :evidence)
    evaluate? = Map.get(params, :evaluate, true) != false
    force? = Map.get(params, :force, false) == true

    with :ok <- validate_kind(kind),
         :ok <- validate_summary(summary),
         :ok <- SecretScan.scan_many([title, body, applicability, tags]) |> secret_error(),
         {:ok, evaluation} <- maybe_evaluate(summary, evaluate?) do
      if evaluation && not Evaluator.promoted?(evaluation) && not force? do
        Helpers.json(frame, %{
          "created" => false,
          "promoted" => false,
          "evaluation" => evaluation,
          "hint" =>
            "Evaluation did not promote this session (need task_success >= " <>
              "#{get_in(evaluation, ["thresholds", "task_success_min"])} and mean >= " <>
              "#{get_in(evaluation, ["thresholds", "mean_min"])}). Pass force: true to " <>
              "record it anyway."
        })
      else
        meta = %{
          "memory_id" => Store.new_id(),
          "status" => "candidate",
          "kind" => to_string(kind),
          "title" => title,
          "applicability" => applicability,
          "project" => project,
          "tags" => tags,
          "source" => source,
          "evidence" => evidence,
          "evaluation" => evaluation
        }

        case Store.create(meta, body) do
          {:ok, memory_id, path} ->
            Helpers.json(frame, %{
              "created" => true,
              "memory_id" => memory_id,
              "status" => "candidate",
              "promoted" => evaluation && Evaluator.promoted?(evaluation),
              "path" => path,
              "evaluation" => evaluation
            })

          {:error, reason} ->
            Helpers.error(frame, reason)
        end
      end
    else
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end

  defp maybe_evaluate(_summary, false), do: {:ok, nil}

  defp maybe_evaluate(summary, true) do
    case Evaluator.evaluate(summary) do
      {:ok, evaluation} -> {:ok, evaluation}
      {:error, reason} -> {:error, evaluator_error(reason)}
    end
  end

  defp evaluator_error(:disabled) do
    "evaluator is disabled; pass evaluate: false to record the candidate directly"
  end

  defp evaluator_error(reason) when is_binary(reason) do
    "#{reason} (set the Gitee AI key, or pass evaluate: false)"
  end

  defp evaluator_error(reason), do: inspect(reason)

  defp validate_kind(kind) do
    if to_string(kind) in Store.kinds() do
      :ok
    else
      {:error, "invalid kind #{inspect(kind)}; expected one of #{Enum.join(Store.kinds(), ", ")}"}
    end
  end

  defp validate_summary(summary) do
    if String.trim(summary) == "", do: {:error, "`summary` must not be empty"}, else: :ok
  end

  defp secret_error(:ok), do: :ok

  defp secret_error({:error, findings}) do
    {:error, "refusing to store possible secret(s): #{Enum.join(findings, ", ")}"}
  end
end
