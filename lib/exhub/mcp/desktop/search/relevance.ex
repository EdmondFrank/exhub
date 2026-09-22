defmodule Exhub.MCP.Desktop.Search.Relevance do
  @moduledoc """
  Precision filter for Desktop `search_files` semantic results, powered by the
  Smart Decide (System One) decision model.

  Probe (`probe search`) recalls and ranks a pool of code blocks cheaply with
  BM25, but returns every block in the token budget regardless of whether it is
  actually relevant to the task. This module adds a second, sharper pass: each
  candidate block is judged by a single `noul` (yes/no) System One question and
  only the blocks the model considers relevant are kept.

  Each candidate is sent as one request on its own — the model has a ~2k-token
  context, so a single code block is all the state it receives. Judgments run
  concurrently (`Task.async_stream`) and the pass degrades gracefully:

    * a per-block failure is treated as *relevant* (fail-open, preserving
      recall) and counted in `:errors`;
    * a candidate whose state is longer than `:max_judgeable_chars` is kept
      unjudged rather than judged on truncated code, so an oversized block can
      never be dropped from incomplete data (`:skipped`); otherwise the state is
      truncated to `:state_char_limit` before judging;
    * a blank task, or an empty candidate list, skips the pass entirely;
    * if every candidate is judged relevant the result is the input;
    * if *no* candidate is judged relevant the pass falls back to the ranked
      pool (`:fallback`) so callers still receive the best-ranked guesses.

  The judged task is the caller's search `purpose` (a natural-language
  description of what they are trying to accomplish), falling back to the raw
  `query` when no purpose is supplied.

  Configuration lives under `:exhub, Exhub.MCP.Desktop.Search.Relevance`;
  in-code defaults apply when it is absent.
  """

  require Logger

  alias Exhub.MCP.Tools.SmartDecide

  @defaults [
    enabled: true,
    candidate_limit: 20,
    max_concurrency: 8,
    threshold: 0.5,
    timeout: 30_000,
    state_char_limit: 4000,
    max_judgeable_chars: 6000,
    query_char_limit: 800,
    fallback: true
  ]

  @typedoc "Counters describing one filter pass."
  @type stats :: %{
          candidates: non_neg_integer(),
          relevant: non_neg_integer(),
          errors: non_neg_integer(),
          skipped: non_neg_integer(),
          filtered: boolean(),
          fallback: boolean()
        }

  @doc """
  Returns the effective configuration, merging `:exhub,
  Exhub.MCP.Desktop.Search.Relevance` over the in-code defaults.
  """
  @spec config() :: keyword()
  def config do
    Keyword.merge(@defaults, Application.get_env(:exhub, __MODULE__, []))
  end

  @doc "Whether Smart Decide filtering is enabled by default."
  @spec enabled?() :: boolean()
  def enabled?, do: Keyword.get(config(), :enabled, true)

  @doc """
  Filters `candidates` down to the code blocks Smart Decide judges relevant to
  `purpose`.

  Returns `{relevant, stats}`. `relevant` preserves the input order.

  Options (all default to the corresponding config value):

    * `:decider` — `(state, questions, opts -> {:ok, result} | {:error, msg})`,
      defaults to `&Exhub.MCP.Tools.SmartDecide.decide/3`. Injectable so tests
      can avoid network calls.
    * `:threshold` — minimum `noul` probability to keep a block (`0.5`)
    * `:max_concurrency` — concurrent System One requests (`8`)
    * `:timeout` — per-request timeout in milliseconds (`30_000`)
    * `:state_char_limit` — max characters of code text sent as `state` when
      judging (`4000`)
    * `:max_judgeable_chars` — candidates whose state is longer than this are
      kept unjudged rather than judged on truncated code (`6000`)
    * `:query_char_limit` — max characters of the task in `instructions` (`800`)
    * `:fallback` — return the ranked pool when nothing is judged relevant (`true`)
  """
  @spec filter(String.t(), [map()], keyword()) :: {[map()], stats()}
  def filter(purpose, candidates, opts \\ []) when is_list(candidates) do
    maps = Enum.filter(candidates, &is_map/1)
    total = length(maps)
    fallback? = Keyword.get(opts, :fallback, Keyword.fetch!(@defaults, :fallback))

    cond do
      total == 0 or not judgeable_purpose?(purpose) ->
        {maps, stats(total, total, 0, 0, false, false)}

      true ->
        {relevant, errors, skipped} = maps |> judge_all(purpose, opts) |> collect()

        cond do
          relevant != [] ->
            {relevant, stats(total, length(relevant), errors, skipped, true, false)}

          fallback? ->
            Logger.debug(
              "[DesktopSearch] Relevance: nothing relevant for " <>
                "#{inspect(purpose)} — falling back to the ranked pool"
            )

            {maps, stats(total, total, errors, skipped, true, true)}

          true ->
            {[], stats(total, 0, errors, skipped, true, false)}
        end
    end
  end

  @doc """
  Interprets a Smart Decide result as a relevance verdict.

  Keeps the block unless the `relevant` answer is a `noul` probability below
  `threshold`. Unparsable answers are treated as relevant (fail-open).
  """
  @spec relevant?(term(), number()) :: boolean()
  def relevant?(%{"answers" => answers}, threshold) when is_map(answers) do
    case Map.get(answers, "relevant") do
      answer when is_map(answer) -> noul_probability(answer) >= threshold
      _ -> true
    end
  end

  def relevant?(_result, _threshold), do: true

  # --- Private ---

  defp judgeable_purpose?(purpose), do: is_binary(purpose) and String.trim(purpose) != ""

  defp collect(results) do
    {kept, errors, skipped} =
      Enum.reduce(results, {[], 0, 0}, fn
        {block, :keep, nil}, {kept, errors, skipped} ->
          {[block | kept], errors, skipped}

        {block, :keep, :oversized}, {kept, errors, skipped} ->
          {[block | kept], errors, skipped + 1}

        {block, :keep, _reason}, {kept, errors, skipped} ->
          {[block | kept], errors + 1, skipped}

        {_block, :drop, _reason}, {kept, errors, skipped} ->
          {kept, errors, skipped}
      end)

    if errors > 0 do
      Logger.debug("[DesktopSearch] Relevance: #{errors} judgment(s) failed (kept as relevant)")
    end

    if skipped > 0 do
      Logger.debug(
        "[DesktopSearch] Relevance: #{skipped} oversized block(s) kept without judging"
      )
    end

    {Enum.reverse(kept), errors, skipped}
  end

  defp judge_all(candidates, purpose, opts) do
    decider = Keyword.get(opts, :decider, &SmartDecide.decide/3)
    threshold = Keyword.get(opts, :threshold, Keyword.fetch!(@defaults, :threshold))

    max_concurrency =
      Keyword.get(opts, :max_concurrency, Keyword.fetch!(@defaults, :max_concurrency))

    timeout = Keyword.get(opts, :timeout, Keyword.fetch!(@defaults, :timeout))

    state_limit =
      Keyword.get(opts, :state_char_limit, Keyword.fetch!(@defaults, :state_char_limit))

    max_judgeable =
      Keyword.get(opts, :max_judgeable_chars, Keyword.fetch!(@defaults, :max_judgeable_chars))

    purpose_limit =
      Keyword.get(opts, :query_char_limit, Keyword.fetch!(@defaults, :query_char_limit))

    instructions = instructions(purpose, purpose_limit)

    candidates
    |> Task.async_stream(
      fn block -> judge(block, instructions, state_limit, max_judgeable, threshold, decider) end,
      max_concurrency: max_concurrency,
      timeout: timeout,
      on_timeout: :kill_task,
      ordered: true
    )
    |> Enum.zip(candidates)
    |> Enum.map(fn
      {{:ok, result}, _block} -> result
      {{:exit, reason}, block} -> {block, :keep, {:timeout, reason}}
    end)
  end

  defp judge(block, instructions, state_limit, max_judgeable, threshold, decider) do
    state = build_state(block)

    if String.length(state) > max_judgeable do
      # Past this point the block is too big to judge honestly: the code that
      # makes it relevant may be beyond any truncation, so keep it unjudged.
      {block, :keep, :oversized}
    else
      questions = %{"relevant" => %{"type" => "noul", "instructions" => instructions}}

      case safe_decide(decider, truncate(state, state_limit), questions) do
        {:ok, result} ->
          if relevant?(result, threshold), do: {block, :keep, nil}, else: {block, :drop, nil}

        {:error, reason} ->
          {block, :keep, reason}
      end
    end
  end

  defp safe_decide(decider, state, questions) do
    case decider.(state, questions, []) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, reason}
      other -> {:error, {:unexpected_decider_result, other}}
    end
  rescue
    e -> {:error, Exception.message(e)}
  catch
    kind, reason -> {:error, {kind, reason}}
  end

  defp build_state(result) do
    file = Map.get(result, "file") || Map.get(result, :file) || "unknown"
    symbol = Map.get(result, "owner_symbol") || Map.get(result, :owner_symbol)
    code = Map.get(result, "code") || Map.get(result, :code) || ""

    [
      "File: #{file}",
      (is_binary(symbol) and symbol != "") && "Symbol: #{symbol}",
      code != "" && "Code:\n#{code}"
    ]
    |> Enum.reject(&(&1 == false))
    |> Enum.join("\n")
  end

  defp instructions(purpose, limit) do
    p = purpose |> to_string() |> String.trim() |> truncate(limit)

    ~s(Decide whether the code block described in the state could help with this task: "#{p}". ) <>
      "Answer yes if the code is relevant, related or plausibly useful — partial " <>
      "matches and sensible intermediate steps count. Answer no only if the code " <>
      "is clearly unrelated. When unsure, answer yes."
  end

  defp noul_probability(answer) when is_map(answer) do
    probabilities = Map.get(answer, "probabilities", %{})

    cond do
      is_number(Map.get(answer, "noul")) -> Map.get(answer, "noul")
      is_number(Map.get(probabilities, "true")) -> Map.get(probabilities, "true")
      is_number(Map.get(answer, "score")) -> Map.get(answer, "score")
      Map.get(answer, "choice") in ["no", "false", false] -> 0.0
      true -> 1.0
    end
  end

  defp noul_probability(_answer), do: 1.0

  defp truncate(text, limit) when is_integer(limit) and limit > 0 do
    if String.length(text) > limit, do: String.slice(text, 0, limit) <> "…", else: text
  end

  defp truncate(text, _limit), do: text

  defp stats(candidates, relevant, errors, skipped, filtered, fallback) do
    %{
      candidates: candidates,
      relevant: relevant,
      errors: errors,
      skipped: skipped,
      filtered: filtered,
      fallback: fallback
    }
  end
end
