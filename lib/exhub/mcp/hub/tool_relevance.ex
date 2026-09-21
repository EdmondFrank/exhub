defmodule Exhub.MCP.Hub.ToolRelevance do
  @moduledoc """
  Precision filter for MCP tool discovery, powered by the Smart Decide
  (System One) decision model.

  `Exhub.MCP.Hub.ToolSearch` ranks a wide candidate pool cheaply with TF-IDF,
  but it returns a fixed number of results regardless of whether they are
  actually relevant to the query. This module adds a second, sharper pass: each
  candidate is judged by a single `noul` (yes/no) System One question and only
  the tools the model considers relevant are kept.

  Each candidate is sent as one request on its own — the model has a ~2k-token
  context, so a single tool description is all the state it receives. Judgments
  run concurrently (`Task.async_stream`) and the pass degrades gracefully:

    * a per-tool failure is treated as *relevant* (fail-open, preserving recall)
      and counted in `:errors`;
    * a blank query, or an empty candidate list, skips the pass entirely;
    * meta servers (`:exclude_servers` — the hub's own tools and the decision
      engine) are dropped before judging: asking Smart Decide whether Smart
      Decide is relevant is circular;
    * if every candidate is judged relevant the result is the input;
    * if *no* candidate is judged relevant the pass falls back to the TF-IDF
      pool (`:fallback`) so callers still receive the best-ranked guesses.

  Configuration lives under `:exhub, Exhub.MCP.Hub.ToolRelevance`;
  in-code defaults apply when it is absent.
  """

  require Logger

  alias Exhub.MCP.Tools.SmartDecide

  @defaults [
    enabled: true,
    candidate_limit: 30,
    max_concurrency: 8,
    threshold: 0.5,
    timeout: 30_000,
    state_char_limit: 1500,
    query_char_limit: 800,
    exclude_servers: ["mcp-hub"],
    fallback: true
  ]

  @typedoc "Counters describing one filter pass."
  @type stats :: %{
          candidates: non_neg_integer(),
          relevant: non_neg_integer(),
          errors: non_neg_integer(),
          excluded: non_neg_integer(),
          filtered: boolean(),
          fallback: boolean()
        }

  @doc """
  Returns the effective configuration, merging `:exhub,
  Exhub.MCP.Hub.ToolRelevance` over the in-code defaults.
  """
  @spec config() :: keyword()
  def config do
    Keyword.merge(@defaults, Application.get_env(:exhub, __MODULE__, []))
  end

  @doc "Whether Smart Decide filtering is enabled by default."
  @spec enabled?() :: boolean()
  def enabled?, do: Keyword.get(config(), :enabled, true)

  @doc """
  Filters `candidates` down to the tools Smart Decide judges relevant to
  `query`.

  Returns `{relevant, stats}`. `relevant` preserves the input order.

  Options (all default to the corresponding config value):

    * `:decider` — `(state, questions, opts -> {:ok, result} | {:error, msg})`,
      defaults to `&Exhub.MCP.Tools.SmartDecide.decide/3`. Injectable so tests
      can avoid network calls.
    * `:threshold` — minimum `noul` probability to keep a tool (`0.5`)
    * `:max_concurrency` — concurrent System One requests (`8`)
    * `:timeout` — per-request timeout in milliseconds (`30_000`)
    * `:state_char_limit` — max characters of tool text sent as `state` (`1500`)
    * `:query_char_limit` — max characters of the query in `instructions` (`800`)
    * `:exclude_servers` — servers dropped from the pool before judging
      (`["mcp-hub"]`)
    * `:fallback` — return the TF-IDF pool when nothing is judged relevant (`true`)
  """
  @spec filter(String.t(), [map()], keyword()) :: {[map()], stats()}
  def filter(query, candidates, opts \\ []) when is_list(candidates) do
    maps = Enum.filter(candidates, &is_map/1)
    total = length(maps)

    exclude_servers =
      Keyword.get(opts, :exclude_servers, Keyword.fetch!(@defaults, :exclude_servers))

    pool = Enum.reject(maps, &(Map.get(&1, "server") in exclude_servers))
    excluded = total - length(pool)
    fallback? = Keyword.get(opts, :fallback, Keyword.fetch!(@defaults, :fallback))

    cond do
      total == 0 or not judgeable_query?(query) ->
        {pool, stats(total, length(pool), 0, excluded, false, false)}

      pool == [] ->
        {[], stats(total, 0, 0, excluded, false, false)}

      true ->
        {relevant, errors} = pool |> judge_all(query, opts) |> collect()

        cond do
          relevant != [] ->
            {relevant, stats(total, length(relevant), errors, excluded, true, false)}

          fallback? ->
            Logger.debug(
              "[MCP Hub] ToolRelevance: nothing relevant for " <>
                "#{inspect(query)} — falling back to the TF-IDF pool"
            )

            {pool, stats(total, length(pool), errors, excluded, true, true)}

          true ->
            {[], stats(total, 0, errors, excluded, true, false)}
        end
    end
  end

  @doc """
  Interprets a Smart Decide result as a relevance verdict.

  Keeps the tool unless the `relevant` answer is a `noul` probability below
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

  defp judgeable_query?(query), do: is_binary(query) and String.trim(query) != ""

  defp collect(results) do
    {kept, errors} =
      Enum.reduce(results, {[], 0}, fn
        {tool, :keep, nil}, {kept, errors} -> {[tool | kept], errors}
        {tool, :keep, _reason}, {kept, errors} -> {[tool | kept], errors + 1}
        {_tool, :drop, _reason}, {kept, errors} -> {kept, errors}
      end)

    if errors > 0 do
      Logger.debug("[MCP Hub] ToolRelevance: #{errors} judgment(s) failed (kept as relevant)")
    end

    {Enum.reverse(kept), errors}
  end

  defp judge_all(candidates, query, opts) do
    decider = Keyword.get(opts, :decider, &SmartDecide.decide/3)
    threshold = Keyword.get(opts, :threshold, Keyword.fetch!(@defaults, :threshold))

    max_concurrency =
      Keyword.get(opts, :max_concurrency, Keyword.fetch!(@defaults, :max_concurrency))

    timeout = Keyword.get(opts, :timeout, Keyword.fetch!(@defaults, :timeout))

    state_limit =
      Keyword.get(opts, :state_char_limit, Keyword.fetch!(@defaults, :state_char_limit))

    query_limit =
      Keyword.get(opts, :query_char_limit, Keyword.fetch!(@defaults, :query_char_limit))

    instructions = instructions(query, query_limit)

    candidates
    |> Task.async_stream(
      fn tool -> judge(tool, instructions, state_limit, threshold, decider) end,
      max_concurrency: max_concurrency,
      timeout: timeout,
      on_timeout: :kill_task,
      ordered: true
    )
    |> Enum.zip(candidates)
    |> Enum.map(fn
      {{:ok, result}, _tool} -> result
      {{:exit, reason}, tool} -> {tool, :keep, {:timeout, reason}}
    end)
  end

  defp judge(tool, instructions, state_limit, threshold, decider) do
    state = build_state(tool, state_limit)
    questions = %{"relevant" => %{"type" => "noul", "instructions" => instructions}}

    case safe_decide(decider, state, questions) do
      {:ok, result} ->
        if relevant?(result, threshold), do: {tool, :keep, nil}, else: {tool, :drop, nil}

      {:error, reason} ->
        {tool, :keep, reason}
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

  defp build_state(tool, limit) do
    name = Map.get(tool, "full_name") || Map.get(tool, "name") || "unknown"
    server = Map.get(tool, "server", "unknown")
    description = Map.get(tool, "description", "")

    "Tool: #{name}\nServer: #{server}\nDescription: #{description}"
    |> truncate(limit)
  end

  defp instructions(query, limit) do
    q = query |> to_string() |> String.trim() |> truncate(limit)

    ~s(Decide whether the tool described in the state could help with this task: "#{q}". ) <>
      "Answer yes if the tool is relevant, related or plausibly useful — partial " <>
      "matches and sensible intermediate steps count. Answer no only if the tool " <>
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

  defp stats(candidates, relevant, errors, excluded, filtered, fallback) do
    %{
      candidates: candidates,
      relevant: relevant,
      errors: errors,
      excluded: excluded,
      filtered: filtered,
      fallback: fallback
    }
  end
end
