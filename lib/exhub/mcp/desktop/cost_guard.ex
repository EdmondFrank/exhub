defmodule Exhub.MCP.Desktop.CostGuard do
  @moduledoc """
  Pre-emptively interrupts Desktop tool calls that would scan a very large
  portion of the filesystem — the whole home directory or the entire disk.

  A `search_files`, `list_directory`, `execute_command` or `start_process`
  invocation is cheap when it is scoped to a project directory, but pathological
  when it targets `~`, `/`, `/Users` or a whole-disk scan command (`find /`,
  `grep -r /…`, `du -s ~`, `mdfind`, …). Those calls can run for minutes, spin
  the disk, and burn the tool's entire timeout. This guard refuses them *before*
  the tool runs, so the caller can narrow the scope instead of waiting.

  The decision is layered, mirroring `Exhub.MCP.Desktop.WorkingDir`:

    1. **Deterministic fast path** — a path argument that expands to a broad
       root (`~`, `/`, `/Users`, `/Volumes`, …) or a shell command that scans
       one (`find ~`, `grep -r /Users`, `du -s /`) is refused locally, without a
       network call. Conservative: it only fires on blatant whole-home/whole-disk
       targets, so a scoped-but-large tree (`~/projects`, `/data`) is not caught.
    2. **Smart Decide** — everything else (a scoped path that might still be
       huge, an unfamiliar command) is judged by a single `noul` (yes/no) System
       One question: *does this invocation scan a very large portion of the
       filesystem?* The model generalises where a static list cannot.
    3. **Deterministic fallback** — when the model is disabled or the request
       fails, the call is **allowed** (fail open). Unlike the working-dir gate,
       a false refusal here would block legitimate work, so the guard prefers to
       let an ambiguous call through; the fast-path refusals above still apply.

  Verdicts are cached per `{tool, arguments}`, so a repeated call does not
  re-hit the API. The cache is a named ETS table owned by this module's
  supervised process (`start_link/1`), started alongside the Desktop
  `ProcessStore`/`WorkingDir`/`ProxyEnv` children. Ownership matters: Anubis
  runs each `tools/call` in a transient task, so a table created lazily by a
  request would be destroyed as soon as that request returned.
  `ensure_cache_table/0` still creates the table on demand as a fallback for
  tests and for a hot-reloaded VM whose supervisor child is not running yet.

  Configuration lives under `:exhub, Exhub.MCP.Desktop.CostGuard`; in-code
  defaults apply when it is absent.
  """

  use GenServer

  require Logger

  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Tools.SmartDecide

  @cache_table :exhub_cost_guard_cache

  @defaults [
    enabled: true,
    model: "APUS-OpenJev-v1-9B",
    threshold: 0.5,
    timeout: 30_000,
    cache_ttl_ms: 60_000,
    cache_limit: 2_000,
    guarded_tools: ~w(search_files list_directory execute_command start_process),
    always_allow: []
  ]

  # Paths that name the whole home directory or the whole disk, so a scan rooted
  # at them is refused deterministically.
  @broad_dirs ~w(/ /Users /Volumes /System /Library /private /opt /usr /var /etc /Applications /Network)

  # Commands whose *purpose* is to walk a tree; ANDed with a broad path token,
  # so `grep foo /Users/me/file` (no recursion, specific file) is not caught.
  @scan_verbs ~w(find mdfind locate du fd rg grep ag ack)

  # Commands that always scan a whole pre-built index, whatever their arguments.
  @whole_disk_verbs ~w(mdfind locate)

  # A `list_directory` over a broad root only becomes expensive once it recurses.
  @broad_listing_depth 2

  # --- Supervision ---

  @doc """
  Starts the cache owner.

  The ETS table is created in `init/1`, so it is owned by this long-lived
  process rather than by whichever request populates it first.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: Keyword.get(opts, :name, __MODULE__))
  end

  @impl true
  def init(_opts) do
    ensure_cache_table()
    {:ok, %{}}
  end

  @doc """
  Returns the effective configuration, merging `:exhub,
  Exhub.MCP.Desktop.CostGuard` over the in-code defaults.
  """
  @spec config() :: keyword()
  def config do
    Keyword.merge(@defaults, Application.get_env(:exhub, __MODULE__, []))
  end

  @doc "Whether the Smart Decide layer is enabled by default."
  @spec enabled?() :: boolean()
  def enabled?, do: Keyword.get(config(), :enabled, true)

  # --- Entry points ---

  @doc """
  Judges an MCP `tools/call` request, extracting its tool name and arguments.

  Returns `:ok` when the call may run, or `{:refuse, message}` with a
  caller-facing explanation. Any shape other than a well-formed `tools/call`
  request is allowed.
  """
  @spec check_request(term(), keyword()) :: :ok | {:refuse, String.t()}
  def check_request(request, opts \\ [])

  def check_request(%{"params" => %{"name" => name} = params}, opts) when is_binary(name) do
    check(name, Map.get(params, "arguments", %{}), opts)
  end

  def check_request(_request, _opts), do: :ok

  @doc """
  Returns `:ok` when `tool` with `arguments` may run, or `{:refuse, message}`
  when the invocation looks set to scan the whole home directory or disk.

  Options (all default to the corresponding config value):

    * `:enabled` — master switch; when false only the deterministic fast path
      refuses, and the model is never consulted (`true`)
    * `:decider` — `(state, questions, opts -> {:ok, result} | {:error, msg})`,
      defaults to `&Exhub.MCP.Tools.SmartDecide.decide/3`. Injectable so tests
      can avoid network calls; providing it also disables the cache unless
      `:cache` is set explicitly.
    * `:model` — System One model id for this decision, overriding the
      CostGuard default (`APUS-OpenJev-v1-9B`, chosen over the Smart Decide
      default by a labeled accuracy sweep)
    * `:threshold` — minimum `noul` probability to refuse (`0.5`)
    * `:timeout` — per-request timeout in milliseconds (`30_000`)
    * `:cache` — cache this verdict (`true`; off when a `:decider` is injected)
    * `:guarded_tools` — tool names this gate applies to
    * `:always_allow` — tool names never gated
  """
  @spec check(String.t(), term(), keyword()) :: :ok | {:refuse, String.t()}
  def check(tool, arguments, opts \\ [])

  def check(tool, arguments, opts) when is_binary(tool) do
    arguments = normalize_args(arguments)

    cond do
      not guarded?(tool, opts) ->
        :ok

      tool in opt(opts, :always_allow) ->
        :ok

      cacheable?(opts) ->
        cache_get_or_compute({tool, arguments}, fn -> compute(tool, arguments, opts) end)

      true ->
        compute(tool, arguments, opts)
    end
  end

  # Fail open on anything that is not a tool name.
  def check(_tool, _arguments, _opts), do: :ok

  @doc "Empties the verdict cache."
  @spec clear_cache() :: :ok
  def clear_cache do
    if :ets.whereis(@cache_table) != :undefined do
      :ets.delete_all_objects(@cache_table)
    end

    :ok
  end

  # --- Deterministic helpers (pure; public for tests) ---

  @doc """
  Returns `true` when `path` (after `~` expansion) names the home directory or a
  whole-disk root — `/`, `/Users`, `/Volumes`, `/System`, `/Library`, … — so a
  scan rooted there touches the entire home directory or filesystem.
  """
  @spec broad_path?(term()) :: boolean()
  def broad_path?(path) when is_binary(path) do
    case Helpers.expand_path(String.trim(path)) do
      expanded when is_binary(expanded) and expanded != "" ->
        MapSet.member?(broad_roots(), expanded)

      _ ->
        false
    end
  end

  def broad_path?(_path), do: false

  @doc """
  Returns `true` when `command` is a tree-walking command (`find`, `grep -r`,
  `rg`, `du`, `mdfind`, …) whose target is a broad root (`~`, `/`, `/Users`), or
  a whole-index command (`mdfind`, `locate`) that always scans the disk.

  Scoped targets (`find /code -name x`, `grep -r foo ~/projects`) return `false`
  and are left to the Smart Decide layer.
  """
  @spec broad_command?(term()) :: boolean()
  def broad_command?(command) when is_binary(command) do
    cond do
      first_verb(command) in @whole_disk_verbs -> true
      scan_command?(command) -> Enum.any?(path_tokens(command), &broad_path?/1)
      true -> false
    end
  end

  def broad_command?(_command), do: false

  # --- Decision ---

  defp compute(tool, arguments, opts) do
    cond do
      fast_refuse?(tool, arguments) ->
        {:refuse, refuse_message(tool, arguments)}

      true ->
        judge(tool, arguments, opts)
    end
  end

  defp judge(tool, arguments, opts) do
    if enabled?(opts) do
      case decide(tool, arguments, opts) do
        {:ok, result} ->
          if refuse_result?(result, opt(opts, :threshold)) do
            {:refuse, refuse_message(tool, arguments)}
          else
            :ok
          end

        {:error, reason} ->
          Logger.debug(
            "[CostGuard] Smart Decide failed for #{tool} #{inspect(arguments)}: " <>
              "#{inspect(reason)} — allowing"
          )

          :ok
      end
    else
      :ok
    end
  end

  # An injected `:decider` is an explicit request to run the decision (tests,
  # overrides), so it implies enabled unless `:enabled` says otherwise.
  defp enabled?(opts) do
    case Keyword.fetch(opts, :enabled) do
      {:ok, value} -> value
      :error -> Keyword.has_key?(opts, :decider) or enabled?()
    end
  end

  defp decide(tool, arguments, opts) do
    decider = Keyword.get(opts, :decider, &SmartDecide.decide/3)
    decide_opts = decide_opts(opt(opts, :model))
    state = state(tool, arguments)
    questions = %{"expensive" => %{"type" => "noul", "instructions" => instructions()}}

    task = Task.async(fn -> safe_decider(decider, state, questions, decide_opts) end)

    case Task.yield(task, opt(opts, :timeout)) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      nil -> {:error, :timeout}
    end
  end

  defp safe_decider(decider, state, questions, decide_opts) do
    case decider.(state, questions, decide_opts) do
      {:ok, result} -> {:ok, result}
      {:error, reason} -> {:error, reason}
      other -> {:error, {:unexpected_decider_result, other}}
    end
  rescue
    e -> {:error, Exception.message(e)}
  catch
    kind, reason -> {:error, {kind, reason}}
  end

  @doc """
  Interprets a Smart Decide result as an "expensive" verdict.

  Only a confident yes (`probability >= threshold`) refuses; anything unparsable
  is treated as `false` (allow), so a malformed answer never blocks a call.
  """
  @spec refuse_result?(term(), number()) :: boolean()
  def refuse_result?(%{"answers" => answers}, threshold) when is_map(answers) do
    case Map.get(answers, "expensive") do
      answer when is_map(answer) -> noul_probability(answer) >= threshold
      _ -> false
    end
  end

  def refuse_result?(_result, _threshold), do: false

  defp state(tool, arguments) do
    "tool: #{tool}\narguments: #{inspect(arguments, limit: 20, printable_limit: 2000)}"
  end

  defp instructions do
    "The user is about to run the tool invocation described by the state on " <>
      "their personal computer. Will it read or scan a very large portion of " <>
      "the filesystem — the user's entire home directory or the whole disk — " <>
      "making it slow and resource-intensive? Answer yes if the target path or " <>
      "command is rooted at the whole home directory or filesystem root (for " <>
      "example searching, listing, finding, grepping, indexing or hashing from " <>
      "`~`, `/`, or `/Users`), or if it would recurse over the whole disk. " <>
      "Answer no if it is scoped to a specific project, subdirectory, or a " <>
      "small set of files. When unsure, answer no."
  end

  # Pass the configured System One model to the decider. `nil`/blank means the
  # SmartDecide default (Intern-Decision-4B).
  defp decide_opts(model) when is_binary(model) do
    case String.trim(model) do
      "" -> []
      trimmed -> [model: trimmed]
    end
  end

  defp decide_opts(_model), do: []

  # --- Fast path ---

  defp fast_refuse?("search_files", arguments), do: broad_path?(arguments["path"])

  defp fast_refuse?("list_directory", arguments) do
    broad_path?(arguments["path"]) and recursive_depth?(arguments["depth"])
  end

  defp fast_refuse?(tool, arguments) when tool in ["execute_command", "start_process"] do
    broad_command?(arguments["command"])
  end

  defp fast_refuse?(_tool, _arguments), do: false

  defp recursive_depth?(depth) when is_integer(depth), do: depth >= @broad_listing_depth
  defp recursive_depth?(_depth), do: false

  defp first_verb(command) do
    command
    |> String.split(~r/\s+/, trim: true)
    |> List.first()
    |> to_string()
    |> Path.basename()
    |> String.trim("\"'")
  end

  defp scan_command?(command) do
    command
    |> String.split(~r/\s+/)
    |> Enum.map(&Path.basename(String.trim(&1, "\"'")))
    |> Enum.any?(&(&1 in @scan_verbs))
  end

  defp path_tokens(command) do
    command
    |> String.split(~r/\s+/)
    |> Enum.map(&String.trim(&1, "\"'"))
    |> Enum.reject(&(&1 == "" or String.starts_with?(&1, "-")))
  end

  defp broad_roots do
    MapSet.new([System.user_home!() | @broad_dirs])
  end

  # --- Refusal message ---

  defp refuse_message(tool, arguments) do
    target = target(arguments)

    """
    Refused: this `#{tool}` call looks set to scan the entire home directory or \
    filesystem (#{target}), which can run for minutes and heavily load the \
    machine.

    Narrow the scope, then retry:
    - point `path` at a specific project or subdirectory (e.g. ~/projects/<repo>), not ~ or /
    - for shell scans, target a concrete tree (e.g. `grep -r foo ~/projects/<repo>`), not / or ~
    - if the broad scan is genuinely intended, allow `#{tool}` under \
    `:exhub, Exhub.MCP.Desktop.CostGuard` (`always_allow`) \
    """
    |> String.trim_trailing()
  end

  defp target(arguments) do
    cond do
      is_binary(arguments["path"]) -> "path: #{arguments["path"]}"
      is_binary(arguments["command"]) -> "command: #{arguments["command"]}"
      true -> inspect(arguments, limit: 10, printable_limit: 200)
    end
  end

  # --- Options, cache, guards ---

  defp guarded?(tool, opts), do: tool in opt(opts, :guarded_tools)

  defp opt(opts, key), do: Keyword.get(opts, key, Keyword.get(config(), key))

  defp normalize_args(args) when is_map(args) do
    Map.new(args, fn {k, v} -> {to_string(k), v} end)
  end

  defp normalize_args(_args), do: %{}

  defp cacheable?(opts) do
    case Keyword.fetch(opts, :cache) do
      {:ok, value} -> value
      :error -> not Keyword.has_key?(opts, :decider)
    end
  end

  defp cache_get_or_compute(key, fun) do
    case cache_get(key) do
      {:ok, value} ->
        value

      :miss ->
        value = fun.()
        cache_put(key, value)
        value
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

  # Fallback table creation for when the supervised owner is not running (unit
  # tests, or a hot-reloaded VM before the supervisor child is attached). In
  # production `init/1` creates the table first, so this is a no-op.
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
end
