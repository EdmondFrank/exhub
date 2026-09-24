defmodule Exhub.MCP.Desktop.WorkingDir do
  @moduledoc """
  Decides whether a shell command requires a `working_dir`, so the Desktop
  `execute_command` and `start_process` tools require one only when it actually
  matters.

  The decision is layered:

    1. **Anchored fast path** — a command that specifies its own location (an
       absolute/`~` path, or a `cd` into an absolute/`~` target) never needs a
       `working_dir`. This is resolved locally, without a network call.
    2. **Smart Decide** — any other command is judged by a single `noul`
       (yes/no) System One question: *does running this command depend on the
       current working directory?* A model generalises where a static heuristic
       cannot, so commands that are independent of the cwd (a URL fetch, a tool
       version check) are no longer forced to supply one.
    3. **Deterministic fallback** — when the model is disabled or the request
       fails, the previous pure heuristic (`Exhub.MCP.Desktop.Helpers`) decides.
       It fails closed (requires a `working_dir` unless the command is clearly
       anchored), so an API/network problem never silently runs a command in the
       wrong directory.

  Verdicts are cached per command string, so a repeated command does not re-hit
  the API. The cache is a lazily-created named ETS table (no supervision-tree
  child), matching `Exhub.BlinkSearch.Backends.FindFile`.

  Configuration lives under `:exhub, Exhub.MCP.Desktop.WorkingDir`; in-code
  defaults apply when it is absent.
  """

  require Logger

  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Tools.SmartDecide

  @cache_table :exhub_working_dir_cache

  @defaults [
    enabled: true,
    threshold: 0.5,
    timeout: 30_000,
    cache_ttl_ms: 60_000,
    cache_limit: 2_000
  ]

  @doc """
  Returns the effective configuration, merging `:exhub,
  Exhub.MCP.Desktop.WorkingDir` over the in-code defaults.
  """
  @spec config() :: keyword()
  def config do
    Keyword.merge(@defaults, Application.get_env(:exhub, __MODULE__, []))
  end

  @doc "Whether Smart Decide is enabled by default."
  @spec enabled?() :: boolean()
  def enabled?, do: Keyword.get(config(), :enabled, true)

  @doc """
  Returns `true` when `command` requires an explicit `working_dir`.

  Anchored commands (absolute/`~` paths, or a `cd` into an absolute/`~` target)
  resolve locally; every other command is judged by Smart Decide, falling back
  to the pure heuristic when the model is disabled or errors. Blank commands
  fail closed (`true`).

  Options (all default to the corresponding config value):

    * `:enabled` — master switch; when false the pure heuristic is used (`true`)
    * `:decider` — `(state, questions, opts -> {:ok, result} | {:error, msg})`,
      defaults to `&Exhub.MCP.Tools.SmartDecide.decide/3`. Injectable so tests
      can avoid network calls; providing it also disables the cache unless
      `:cache` is set explicitly.
    * `:threshold` — minimum `noul` probability to require a `working_dir` (`0.5`)
    * `:timeout` — per-request timeout in milliseconds (`30_000`)
    * `:cache` — cache this verdict (`true`; off when a `:decider` is injected)
  """
  @spec needs_working_dir?(String.t(), keyword()) :: boolean()
  def needs_working_dir?(command, opts \\ [])

  def needs_working_dir?(command, opts) when is_binary(command) do
    cond do
      blank?(command) ->
        # A blank command is not a confident "no" — fail closed.
        true

      cacheable?(opts) ->
        case cache_get(command) do
          {:ok, value} ->
            value

          :miss ->
            value = compute(command, opts)
            cache_put(command, value)
            value
        end

      true ->
        compute(command, opts)
    end
  end

  # Fail closed on anything that is not a command string.
  def needs_working_dir?(_command, _opts), do: true

  defp blank?(command), do: String.trim(command) == ""

  @doc """
  Interprets a Smart Decide result as a `needs_working_dir` verdict.

  Unparsable answers are treated as `true` (fail closed): without a confident
  "no" the command is required to name its working directory.
  """
  @spec needs_working_dir_result?(term(), number()) :: boolean()
  def needs_working_dir_result?(%{"answers" => answers}, threshold) when is_map(answers) do
    case Map.get(answers, "needs_working_dir") do
      answer when is_map(answer) -> noul_probability(answer) >= threshold
      _ -> true
    end
  end

  def needs_working_dir_result?(_result, _threshold), do: true

  @doc "Empties the decision cache."
  @spec clear_cache() :: :ok
  def clear_cache do
    if :ets.whereis(@cache_table) != :undefined do
      :ets.delete_all_objects(@cache_table)
    end

    :ok
  end

  # --- Decision ---

  defp compute(command, opts) do
    if Helpers.anchored?(command) do
      false
    else
      judge(command, opts)
    end
  end

  defp judge(command, opts) do
    if Keyword.get(opts, :enabled, enabled?()) do
      case decide(command, opts) do
        {:ok, result} ->
          needs_working_dir_result?(result, opt(opts, :threshold))

        {:error, reason} ->
          Logger.debug(
            "[WorkingDir] Smart Decide failed for #{inspect(command)}: " <>
              "#{inspect(reason)} — using heuristic"
          )

          Helpers.needs_working_dir?(command)
      end
    else
      Helpers.needs_working_dir?(command)
    end
  end

  defp decide(command, opts) do
    decider = Keyword.get(opts, :decider, &SmartDecide.decide/3)
    questions = %{"needs_working_dir" => %{"type" => "noul", "instructions" => instructions()}}

    task = Task.async(fn -> safe_decider(decider, command, questions) end)

    case Task.yield(task, opt(opts, :timeout)) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
    end
  end

  defp safe_decider(decider, state, questions) do
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

  defp instructions do
    "Does running this shell command depend on the current working directory? " <>
      "Answer yes if it reads or writes files relative to the current directory, " <>
      "or operates on a project or repository there — for example relative paths, " <>
      "`make`, `mix test`, `git status`, `npm install`, or downloads and clones " <>
      "that write into the current directory (`wget`, `curl -O`, `git clone`). " <>
      "Answer no only if the command is fully independent of the working " <>
      "directory — for example it prints a constant, queries a tool version, " <>
      "talks to a daemon, or fetches a URL without writing files to the current " <>
      "directory (`echo hello`, `node --version`, `docker ps`). " <>
      "When unsure, answer yes."
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

  # --- Options & cache ---

  defp opt(opts, key), do: Keyword.get(opts, key, Keyword.get(config(), key))

  defp cacheable?(opts) do
    case Keyword.fetch(opts, :cache) do
      {:ok, value} -> value
      :error -> not Keyword.has_key?(opts, :decider)
    end
  end

  defp cache_get(key) do
    if :ets.whereis(@cache_table) == :undefined do
      :miss
    else
      case :ets.lookup(@cache_table, key) do
        [{^key, expires_at, value}] ->
          if System.monotonic_time(:millisecond) < expires_at do
            {:ok, value}
          else
            :ets.delete(@cache_table, key)
            :miss
          end

        [] ->
          :miss
      end
    end
  end

  defp cache_put(key, value) do
    ensure_cache_table()

    if :ets.info(@cache_table, :size) >= opt([], :cache_limit) do
      :ets.delete_all_objects(@cache_table)
    end

    expires_at = System.monotonic_time(:millisecond) + opt([], :cache_ttl_ms)
    :ets.insert(@cache_table, {key, expires_at, value})
    :ok
  end

  defp ensure_cache_table do
    if :ets.whereis(@cache_table) == :undefined do
      try do
        :ets.new(@cache_table, [:set, :named_table, :public, read_concurrency: true])
      rescue
        # Another process created it concurrently
        ArgumentError -> :ok
      end
    end

    :ok
  end
end
