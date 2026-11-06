defmodule Exhub.MCP.Desktop.SecretGuard do
  @moduledoc """
  Pre-emptively interrupts Desktop tool calls that look set to expose secrets —
  a credential, private key, envless vault value, or production/release config —
  into the tool result and therefore into the conversation.

  This is the secret-safety counterpart of `Exhub.MCP.Desktop.CostGuard`: the
  same layered design, the same injectable decider, and the same request-scoped
  ETS cache owned by a supervised process.

  The decision is layered:

    1. **Deterministic fast path** — the blatant cases are refused locally,
       without a network call: the `envless` CLI printing a value (`envless get`,
       `envless exec -- env`), an environment dump (`env`, `printenv`, `set`), a
       read of a `.env`/private-key/credential file, a path under an envless
       vault's `.envless/` or `secrets/` directories, or a production/release
       configuration file (`config/runtime.exs`, `prod.exs`, `sys.config`).
    2. **Smart Decide** — everything else is judged by a single `noul` (yes/no)
       System One question: *would this invocation expose a secret value into
       the output?* The model generalises where a static list cannot.
    3. **Deterministic fallback** — when the model is disabled or the request
       fails, the call is **allowed** (fail open). Unlike the working-dir gate, a
       false refusal here would block legitimate work, so the guard prefers to
       let an ambiguous call through; the fast-path refusals above still apply.

  Verdicts are cached per `{tool, arguments}`, so a repeated call does not
  re-hit the API. The cache is a named ETS table owned by this module's
  supervised process (`start_link/1`), started alongside the Desktop
  `ProcessStore`/`WorkingDir`/`ProxyEnv`/`CostGuard` children. Ownership
  matters: Anubis runs each `tools/call` in a transient task, so a table created
  lazily by a request would be destroyed as soon as that request returned.
  `ensure_cache_table/0` still creates the table on demand as a fallback for
  tests and for a hot-reloaded VM whose supervisor child is not running yet.

  Configuration lives under `:exhub, Exhub.MCP.Desktop.SecretGuard`; in-code
  defaults apply when it is absent.
  """

  use GenServer

  require Logger

  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Tools.SmartDecide

  @cache_table :exhub_secret_guard_cache

  # Model + threshold chosen by a labeled sweep for *this* task, not inherited
  # from CostGuard. On 18 secret-risk cases (9 secret / 9 not — the residual the
  # fast path leaves to the model) `SemIf-OpenJev-4B` scored 17/18 at 0.3 with no
  # missed secret, whereas CostGuard's `APUS-OpenJev-v1-9B` managed only 10/18 and
  # emitted out-of-range probabilities (up to 9.1). The ranking is task-specific:
  # do not copy CostGuard's model here. See
  # `knowledge-base/exhub/decision-models.md` → *SecretGuard gate*.
  @defaults [
    enabled: true,
    model: "SemIf-OpenJev-4B",
    threshold: 0.3,
    timeout: 30_000,
    cache_ttl_ms: 60_000,
    cache_limit: 2_000,
    guarded_tools: ~w(read_file read_multiple_files execute_command start_process search_files),
    always_allow: [],
    vault_roots: nil
  ]

  # The envless on-disk layout keeps secret material under these subdirectories
  # of the vault root: `<root>/.envless/` (identity.key, recipients) and
  # `<root>/secrets/` (encrypted `*.env.enc`). Reading *plain* notes that share
  # the vault directory (e.g. `~/GTD/PKB`) is not refused — only the store.
  @envless_secret_dirs ~w(.envless secrets)

  # Files that are secret material by exact name, wherever they live.
  @secret_basenames ~w(.env credentials .netrc id_rsa id_ed25519 id_dsa id_ecdsa .htpasswd .npmrc .pypirc .dockercfg)

  # Secret-ish file extensions.
  @secret_exts ~w(.pem .key .p12 .pfx .jks .keystore .kdbx .age)

  # Template/sample copies carry no real secret and are not refused.
  @template_exts ~w(.example .sample .template .dist .tmpl)

  # Production / release configuration files: they may embed injected secrets or
  # point at them (see `config/runtime.exs`, `releases/*/sys.config`).
  @prod_config_basenames ~w(runtime.exs prod.exs prod.secret.exs sys.config sys.config.src vm.args env.sh)

  # `envless` subcommands that print a secret value to stdout.
  @envless_value_verbs ~w(get export show print)

  # Programs that dump the process environment.
  @env_dumpers ~w(env printenv set)

  # Commands that read or serialise a file's bytes.
  @file_read_verbs ~w(cat less more head tail bat tac strings xxd od base64 base32 openssl gpg sed awk grep egrep fgrep rg ag ack cp scp rsync curl nc pbcopy source open code vi vim nano)

  # Secret-manager / keychain invocations that print a credential.
  @secret_manager_patterns [
    ~r/\bsecurity\s+find-(generic|internet)-password\b/,
    ~r/\bop\s+(read|item\s+get)\b/,
    ~r/\bpass\s+(show|get)\b/,
    ~r/\bgh\s+auth\s+token\b/,
    ~r/\bgpg\b[^\n]*--decrypt\b/,
    ~r/\bsops\s+(-d|--decrypt)\b/,
    ~r/\baws\s+secretsmanager\s+get-secret-value\b/,
    ~r/\baws\s+configure\s+get\b/,
    ~r/\bkubectl\b[^\n]*\bget\s+secrets?\b/
  ]

  # A variable name that looks like it holds a credential.
  @secret_var_re ~r/(?:^|_)(?:SECRET|SECRETS|PASS|PASSWD|PASSWORD|TOKEN|KEY|API|APIKEY|CRED|CREDS|CREDENTIAL|COTP|PRIVATE)(?:_|$)/i

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
  Exhub.MCP.Desktop.SecretGuard` over the in-code defaults.
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
  when the invocation looks set to expose a secret.

  Options (all default to the corresponding config value):

    * `:enabled` — master switch; when false only the deterministic fast path
      refuses, and the model is never consulted (`true`)
    * `:decider` — `(state, questions, opts -> {:ok, result} | {:error, msg})`,
      defaults to `&Exhub.MCP.Tools.SmartDecide.decide/3`. Injectable so tests
      can avoid network calls; providing it also disables the cache unless
      `:cache` is set explicitly.
    * `:model` — System One model id for this decision
    * `:threshold` — minimum `noul` probability to refuse (`0.3`, tuned to
      `SemIf-OpenJev-4B`; for a security gate a lower cut buys recall)
    * `:timeout` — per-request timeout in milliseconds (`30_000`)
    * `:cache` — cache this verdict (`true`; off when a `:decider` is injected)
    * `:guarded_tools` — tool names this gate applies to
    * `:always_allow` — tool names never gated
    * `:vault_roots` — envless vault roots whose `.envless/`/`secrets/` subtrees
      are refused (default: `$ENVLESS_ROOT`, else `~/GTD`)
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
  Returns the envless vault roots treated as secret stores.

  Reads `:vault_roots` from `opts`/config when set, otherwise `$ENVLESS_ROOT`,
  otherwise `~/GTD`.
  """
  @spec vault_roots(keyword()) :: [String.t()]
  def vault_roots(opts \\ []) do
    roots = Keyword.get(opts, :vault_roots, Keyword.get(config(), :vault_roots))

    case roots do
      list when is_list(list) and list != [] ->
        list |> Enum.map(&expand_root/1) |> Enum.reject(&is_nil/1)

      _ ->
        default_vault_roots()
    end
  end

  @doc """
  Returns `true` when `path` names secret material: a file inside an envless
  vault's `.envless/`/`secrets/` subtree, a `.env`/private-key/credential file,
  or a production/release configuration file.
  """
  @spec secret_path?(term(), keyword()) :: boolean()
  def secret_path?(path, opts \\ [])

  def secret_path?(path, opts) when is_binary(path) do
    case Helpers.expand_path(String.trim(path)) do
      expanded when is_binary(expanded) and expanded != "" ->
        base = Path.basename(expanded)

        envless_secret_path?(expanded, vault_roots(opts)) or
          secret_basename?(base) or
          prod_config_path?(base)

      _ ->
        false
    end
  end

  def secret_path?(_path, _opts), do: false

  @doc """
  Returns `true` when `command` prints or reads a secret: the `envless` CLI
  printing a value, an environment dump, a keychain/secret-manager read, or a
  read of a secret file.
  """
  @spec secret_command?(term(), keyword()) :: boolean()
  def secret_command?(command, opts \\ [])

  def secret_command?(command, opts) when is_binary(command) do
    tokens = tokens(command)
    verbs = Enum.map(tokens, &Path.basename/1)

    envless_value_dump?(verbs) or
      envless_env_dump?(verbs) or
      secret_manager_command?(command) or
      env_dump?(verbs, tokens) or
      reads_secret_file?(tokens, opts)
  end

  def secret_command?(_command, _opts), do: false

  # --- Decision ---

  defp compute(tool, arguments, opts) do
    if fast_refuse?(tool, arguments, opts) do
      {:refuse, refuse_message(tool, arguments)}
    else
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
            "[SecretGuard] Smart Decide failed for #{tool} #{inspect(arguments)}: " <>
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
    questions = %{"secret_risk" => %{"type" => "noul", "instructions" => instructions()}}

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
  Interprets a Smart Decide result as a "secret risk" verdict.

  Only a confident yes (`probability >= threshold`) refuses; anything unparsable
  is treated as `false` (allow), so a malformed answer never blocks a call.
  """
  @spec refuse_result?(term(), number()) :: boolean()
  def refuse_result?(%{"answers" => answers}, threshold) when is_map(answers) do
    case Map.get(answers, "secret_risk") do
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
      "their personal computer. Would it expose a secret into the tool's output " <>
      "— the plaintext value of an API key, password or token, a private key or " <>
      "credential file, the contents of an encrypted secrets vault, or a " <>
      "production/release configuration file that carries injected secrets? " <>
      "Answer yes if the command or path prints, dumps, or reads such material " <>
      "(for example `envless get`, `envless exec -- env`, `env`, `printenv`, " <>
      "`security find-generic-password`, `op read`, a `.env`/`.pem`/identity or " <>
      "`secrets/` file, or `config/runtime.exs`, `prod.exs`, `sys.config`). " <>
      "Answer no for ordinary source code, documentation, notes, or a command " <>
      "that does not touch credentials. When unsure, answer no."
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

  defp fast_refuse?(tool, arguments, opts) when tool in ["execute_command", "start_process"] do
    secret_command?(arguments["command"], opts)
  end

  defp fast_refuse?("read_file", arguments, opts), do: secret_path?(arguments["path"], opts)

  defp fast_refuse?("read_multiple_files", arguments, opts) do
    arguments["paths"]
    |> List.wrap()
    |> Enum.any?(&secret_path?(&1, opts))
  end

  defp fast_refuse?("search_files", arguments, opts), do: secret_path?(arguments["path"], opts)

  defp fast_refuse?(_tool, _arguments, _opts), do: false

  # --- Deterministic command heuristics ---

  # `envless get`/`export`/`show`/`print KEY` prints a value to stdout.
  defp envless_value_dump?(verbs) do
    "envless" in verbs and Enum.any?(verbs, &(&1 in @envless_value_verbs))
  end

  # `envless exec -- env`/`printenv`/`set` injects secrets into a child that
  # then prints them all.
  defp envless_env_dump?(verbs) do
    "envless" in verbs and "exec" in verbs and Enum.any?(verbs, &(&1 in @env_dumpers))
  end

  defp secret_manager_command?(command) do
    Enum.any?(@secret_manager_patterns, &Regex.match?(&1, command))
  end

  # A bare `env`/`printenv`/`set` (dumps everything), or one naming a
  # credential-shaped variable. `env FOO=1 cmd` (an assignment) is not caught.
  defp env_dump?(verbs, tokens) do
    if Enum.any?(verbs, &(&1 in @env_dumpers)) do
      argless? =
        tokens
        |> Enum.reject(&(&1 in @env_dumpers or String.starts_with?(&1, "-")))
        |> Enum.empty?()

      argless? or Enum.any?(tokens, &secret_var_name?/1)
    else
      false
    end
  end

  defp secret_var_name?(token) do
    token = token |> String.trim("\"'") |> String.trim() |> String.split("=") |> List.first()
    Regex.match?(@secret_var_re, token)
  end

  # A file-reading command whose argument (a path-like token) is secret material.
  defp reads_secret_file?(tokens, opts) do
    reads? = Enum.any?(tokens, &(Path.basename(&1) in @file_read_verbs))

    reads? and Enum.any?(tokens, &(path_like?(&1) and secret_path?(&1, opts)))
  end

  defp path_like?(token) do
    String.contains?(token, "/") or String.starts_with?(token, ["~", "."])
  end

  defp tokens(command) do
    command
    |> String.split(~r/\s+/)
    |> Enum.map(&String.trim(&1, "\"'"))
    |> Enum.reject(&(&1 == ""))
  end

  # --- Deterministic path heuristics ---

  defp envless_secret_path?(expanded, roots) do
    Enum.any?(roots, fn root -> vault_member?(expanded, root) end)
  end

  defp vault_member?(expanded, root) when is_binary(root) and root != "" do
    if expanded == root or String.starts_with?(expanded, root <> "/") do
      rel = Path.relative_to(expanded, root)

      case Path.split(rel) do
        [first | _] -> first in @envless_secret_dirs or rel == "MANIFEST.json"
        _ -> false
      end
    else
      false
    end
  end

  defp vault_member?(_expanded, _root), do: false

  defp secret_basename?(base) do
    lower = String.downcase(base)

    Path.extname(lower) not in @template_exts and
      (base in @secret_basenames or
         String.starts_with?(lower, ".env") or
         Path.extname(lower) in @secret_exts or
         String.contains?(lower, "secret") or
         String.contains?(lower, "credential") or
         String.contains?(lower, "password"))
  end

  defp prod_config_path?(base), do: base in @prod_config_basenames

  defp expand_root(root) when is_binary(root) do
    case Helpers.expand_path(String.trim(root)) do
      expanded when is_binary(expanded) and expanded != "" -> expanded
      _ -> nil
    end
  end

  defp expand_root(_root), do: nil

  defp default_vault_roots do
    case System.get_env("ENVLESS_ROOT") do
      root when is_binary(root) and root != "" -> [Path.expand(root)]
      _ -> [Path.join(System.user_home!(), "GTD")]
    end
  end

  # --- Refusal message ---

  defp refuse_message(tool, arguments) do
    target = target(arguments)

    """
    Refused: this `#{tool}` call looks set to expose secrets — a credential, \
    private key, or vault/production-config value — into the conversation \
    (#{target}).

    Use the sanctioned path instead:
    - list envless keys with `envless list`; never print a value (`envless get`, \
    `envless exec -- env`) — let a child consume it (`envless exec -- CMD`) or \
    pipe it to the clipboard
    - read application configuration through the app, not by opening \
    `config/runtime.exs` / `sys.config`
    - if this is genuinely intended, allow `#{tool}` under \
    `:exhub, Exhub.MCP.Desktop.SecretGuard` (`always_allow`)
    """
    |> String.trim_trailing()
  end

  defp target(arguments) do
    cond do
      is_binary(arguments["path"]) -> "path: #{arguments["path"]}"
      is_binary(arguments["command"]) -> "command: #{arguments["command"]}"
      is_list(arguments["paths"]) -> "paths: #{inspect(arguments["paths"], limit: 10)}"
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
