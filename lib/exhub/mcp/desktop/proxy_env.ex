defmodule Exhub.MCP.Desktop.ProxyEnv do
  @moduledoc """
  Decides whether an outbound network command should have HTTP(S) proxy
  environment variables injected, and builds those variables for the Desktop
  shell tools.

  This is the network counterpart of `Exhub.MCP.Desktop.WorkingDir`: the same
  layered, fail-closed design, the same injectable decider, and the same
  request-scoped ETS cache owned by a supervised process.

  The decision is layered:

    1. **Deterministic fast path** — only commands that actually make an
       outbound HTTP(S) request are candidates, and (in the default `:on_fail`
       mode) only when the first run failed with a proxy-shaped error. A
       command that reported success is never re-run. Resolved locally,
       without a network call.
    2. **Hard guards** — a credential in the command URL, or the absence of a
       TCP-reachable proxy candidate, stops the decision before any API call.
       These are security and feasibility facts, not judgment calls.
    3. **Smart Decide** — what is left is judged by one System One call
       (`needs_proxy` noul, `mechanism` choice, `leak_risk` score), fed only
       observed evidence: the model has no network access of its own.
    4. **Fail closed** — any error, timeout, or low-confidence verdict means
       "leave the environment untouched", never "guess a proxy". Verdicts
       below `:min_confidence` are abstentions, and abstention does nothing.

  Setup is per child process: variables are merged into the `env` list handed
  to one command. Nothing is persisted to shell rc files, apt config, or git
  config, and the command itself is never rewritten.
  """

  use GenServer

  require Logger

  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Tools.SmartDecide

  @cache_table :exhub_proxy_env_cache

  @proxy_env_keys ~w(https_proxy http_proxy all_proxy)

  # Variables this module injects (upper- and lower-case variants).
  @proxy_vars ~w(HTTPS_PROXY HTTP_PROXY ALL_PROXY NO_PROXY)

  # Endpoints that must keep going direct even when a proxy is in effect.
  # Merged with the operator's `:no_proxy` config and any existing NO_PROXY.
  @default_no_proxy ~w(localhost 127.0.0.1 ::1)

  # The network premise stated in every question's instructions.
  #
  # Measured on the live model: with no premise, a `curl https://www.google.com`
  # timeout that this host plainly needs a proxy for scored `needs_proxy` 0.033 —
  # the model has no idea where the machine is. Stating the premise moved the
  # same byte-identical evidence to 0.992, so the assumption is carried in the
  # prompt rather than left for the model to guess. Override
  # `:network_premise` for another network; set it to `nil` to drop it.
  @mainland_premise "the machine running this command is in mainland China, behind the " <>
                      "Great Firewall: direct egress to overseas domains (www.google.com, " <>
                      "github.com, raw.githubusercontent.com, api.openai.com, pypi.org, " <>
                      "registry-1.docker.io, docker.io) is routinely blocked — it times out, " <>
                      "is refused or reset, or fails the TLS handshake — while domestic domains " <>
                      "(baidu.com, gitee.com, aliyun.com, qq.com, jd.com) work directly; a " <>
                      "locally running HTTP CONNECT proxy (a Clash/v2ray listener on 127.0.0.1, " <>
                      "or the operator's corporate proxy) is the normal, intended way to reach " <>
                      "overseas endpoints from this network"

  @defaults [
    enabled: true,
    mode: :on_fail,
    model: nil,
    proxy_url: nil,
    fallback_proxies: ["http://127.0.0.1:7890", "http://127.0.0.1:1080"],
    no_proxy: [],
    network_premise: @mainland_premise,
    network_notes: nil,
    target_probe: true,
    target_probe_timeout_ms: 1_500,
    min_confidence: 0.6,
    max_leak_risk: 2.0,
    timeout: 30_000,
    probe_timeout_ms: 50,
    cache_ttl_ms: 600_000,
    cache_limit: 2_000
  ]

  # Output fragments meaning "this failed at the transport/proxy layer". Only
  # the shape of the error is checked; whether a proxy is the right fix is the
  # model's job.
  @failure_signatures [
    "http 000",
    "http/1.1 000",
    "received http code 000",
    "ssl_error_syscall",
    "ssl_error_ssl",
    "could not resolve host",
    "no such host",
    "temporary failure in name resolution",
    "unable to resolve host address",
    "connection refused",
    "connection timed out",
    "connection reset by peer",
    "network is unreachable",
    "failed to connect",
    "unable to access",
    "unable to establish ssl connection",
    "empty reply from server",
    "tls handshake timeout",
    "i/o timeout",
    "dial tcp",
    "proxy authentication required",
    "failed to fetch",
    "curl: (5)",
    "curl: (6)",
    "curl: (7)",
    "curl: (28)",
    "curl: (35)",
    "curl: (52)",
    "curl: (56)",
    "curl: (60)",
    "fatal: unable to access"
  ]

  # Verbs that can reach out over HTTP(S). Deliberately over-inclusive: it only
  # gates the much narrower failure check, so a false positive costs one
  # string comparison.
  @fetch_verbs ~w(
    curl wget git go npm pnpm yarn bun pip pipx uv poetry cargo brew apt apt-get apk
    docker podman buildah helm kubectl gh aws gcloud az mix rebar gradle mvn composer
    dotnet nuget deno node python python3 ruby wget2 aria2c axel xmake vcpkg conda
    gem bundle pip3
  )

  # --- Supervision ---

  @doc """
  Starts the cache owner.

  The ETS table is created in `init/1` so it is owned by this long-lived
  process rather than by the per-request tool task that first populates it
  (Anubis runs each `tools/call` in a transient task, so a lazily created table
  would be destroyed as soon as that request returned).
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
  Exhub.MCP.Desktop.ProxyEnv` over the in-code defaults.
  """
  @spec config() :: keyword()
  def config do
    Keyword.merge(@defaults, Application.get_env(:exhub, __MODULE__, []))
  end

  @doc "Whether the proxy decision is enabled."
  @spec enabled?() :: boolean()
  def enabled?, do: Keyword.get(config(), :enabled, true)

  @doc "The configured decision mode: `:on_fail` (default) or `:pre`."
  @spec mode() :: atom()
  def mode, do: normalize_mode(Keyword.get(config(), :mode, :on_fail))

  # Only the two known modes are recognised; anything else (including a string
  # from runtime config) falls back to `:on_fail`, so no atom is created from
  # operator-uncontrolled input.
  defp normalize_mode(:on_fail), do: :on_fail
  defp normalize_mode(:pre), do: :pre
  defp normalize_mode("on_fail"), do: :on_fail
  defp normalize_mode("pre"), do: :pre
  defp normalize_mode(_), do: :on_fail

  @doc "The proxy variables this module may inject."
  @spec proxy_vars() :: [String.t()]
  def proxy_vars, do: @proxy_vars

  # --- Deterministic gates (no API call) ---

  @doc """
  Returns `true` when `command` plausibly makes an outbound HTTP(S) request.

  Purely lexical: either it names an `http(s)://` URL or it invokes a known
  networked package manager / client.
  """
  @spec network_fetch?(String.t() | nil) :: boolean()
  def network_fetch?(command) when is_binary(command) do
    stripped =
      command
      |> String.replace(~r/'[^']*'/, ~s(""))
      |> String.replace(~r/"[^"]*"/, ~s(""))

    url_in(stripped) != nil or verb?(stripped)
  end

  def network_fetch?(_command), do: false

  defp verb?(command) do
    verbs = @fetch_verbs

    command
    |> String.split()
    |> Enum.any?(fn word ->
      base = word |> String.trim_leading("./") |> Path.basename()
      Enum.member?(verbs, base)
    end)
  end

  @doc "First `http(s)://` URL appearing in `text`, or `nil`."
  @spec url_in(String.t() | nil) :: String.t() | nil
  def url_in(text) when is_binary(text) do
    case Regex.run(~r{https?://\S+}, text) do
      [url | _] -> url
      _ -> nil
    end
  end

  def url_in(_), do: nil

  @doc """
  Returns `true` when the command carries a credential in its URL or flags.

  Routing such a request through a third-party CONNECT proxy would hand that
  credential to the proxy, so the decision stops here without consulting the
  model.
  """
  @spec credentials_in_url?(String.t() | nil) :: boolean()
  def credentials_in_url?(command) when is_binary(command) do
    # A credential can travel in the URL userinfo, in a query parameter, or in
    # an auth header/flag — all of them are exposed to a CONNECT proxy.
    explicit =
      Regex.match?(
        ~r{oauth2:|authorization:\s*\S+|--(?:access-)?token[=\s]\S+|-token\s+\S+}i,
        command
      )

    case url_in(command) do
      nil ->
        explicit

      url ->
        explicit or URI.parse(url).userinfo not in [nil, ""] or
          Regex.match?(~r{[?&](?:access_token|token|password)=}i, url)
    end
  end

  def credentials_in_url?(_command), do: false

  @doc """
  Returns `true` when a command's output looks like a transport/proxy failure.
  """
  @spec proxy_failure?(term()) :: boolean()
  def proxy_failure?(payload) do
    text = payload_text(payload) |> String.downcase()

    text != "" and Enum.any?(@failure_signatures, &String.contains?(text, &1))
  end

  @doc """
  Returns `true` when the first run of `command` is worth a proxy retry.

  Requires an outbound request, a proxy-shaped error, and — for a completed
  run — a non-zero exit code, so a command that reported success is never
  re-run.

  Deliberately ignores `:enabled`: this is a cheap lexical gate, and the master
  switch lives in `judge/3`, so turning the module off costs one string match
  per failed command rather than changing what this predicate means.
  """
  @spec retry_candidate?(String.t() | nil, {:ok | :timeout | :error, term()}) :: boolean()
  def retry_candidate?(command, {:ok, result}) do
    network_fetch?(command) and
      Map.get(result, "exit_code") not in [0, nil] and
      proxy_failure?(result)
  end

  # A run that hit the tool timeout has no exit code; a proxy-shaped error in
  # the partial output is the evidence (a blackholed route hangs rather than
  # fails).
  def retry_candidate?(command, {:timeout, result}) do
    network_fetch?(command) and proxy_failure?(result)
  end

  def retry_candidate?(_command, _tagged), do: false

  # Kept in its original case: this text is also shown to the model as evidence.
  defp payload_text(payload) when is_map(payload) do
    [Map.get(payload, "stdout"), Map.get(payload, "stderr")]
    |> Enum.map(&to_string/1)
    |> Enum.join("\n")
  end

  defp payload_text(payload) when is_binary(payload), do: payload
  defp payload_text(_), do: ""

  # --- Decision ---

  @doc """
  Judges whether `command` should be retried through a proxy.

  Takes the tagged result of the first run and the environment the child would
  inherit, and returns:

    * `{:inject, additions, detail}` — retry with `additions` (a `{key, value}`
      env list, see `build_env/3`) merged into the child's environment;
    * `{:noop, reason, detail | nil}` — leave the environment untouched.

  Options (each defaulting to the corresponding config value):

    * `:enabled` — master switch
    * `:decider` — `(state, questions, opts -> {:ok, result} | {:error, msg})`,
      defaults to `&Exhub.MCP.Tools.SmartDecide.decide/3`; injectable so tests
      never hit the network
    * `:probe` — `(url, timeout_ms -> :reachable | :unreachable)`, the TCP
      reachability check for the proxy candidate; defaults to `tcp_reachable?/2`
    * `:proxy_url` — candidate proxy URL; otherwise config `:proxy_url`, then an
      already-exported `HTTPS_PROXY`/`https_proxy` in the environment
    * `:env` — the child's current environment (`Helpers.clean_env/0`)
    * `:model`, `:min_confidence`, `:max_leak_risk`, `:timeout`,
      `:probe_timeout_ms`, `:cache`
    * `:target_probe` — whether to measure the target host directly (default on)
    * `:target_probe_fn` — `(host, port, timeout_ms -> {status, note})`, defaults
      to `probe_host/3`; injectable so tests stay off the network
    * `:target_probe_timeout_ms` — budget for that connect
    * `:network_premise` — the network context stated in every question's
      instructions; `nil` asks the model with no regional assumption
    * `:network_notes` — operator's proxy inventory, shown as advisory reference
      material and never enforced

  Every guard and every failure path returns `{:noop, ...}`, so a model or
  network problem leaves the caller's environment exactly as it was.
  """
  @spec judge(String.t() | nil, {:ok | :timeout, term()} | term(), keyword()) ::
          {:inject, [{String.t(), String.t()}], map()} | {:noop, atom(), map() | nil}
  def judge(command, first_result, opts \\ [])

  def judge(command, {:ok, result}, opts), do: decide(command, result, opts)
  def judge(command, {:timeout, result}, opts), do: decide(command, result, opts)
  def judge(_command, _other, _opts), do: {:noop, :not_retryable, nil}

  defp decide(command, result, opts) do
    env = Keyword.get(opts, :env, Helpers.clean_env())

    cond do
      not enabled?(opts) ->
        {:noop, :disabled, nil}

      not is_binary(command) or String.trim(command) == "" ->
        {:noop, :blank_command, nil}

      not network_fetch?(command) ->
        {:noop, :not_a_fetch, nil}

      not proxy_failure?(result) ->
        {:noop, :not_a_network_failure, nil}

      credentials_in_url?(command) ->
        {:noop, :credentials_in_url, nil}

      proxy_already_set?(env) ->
        {:noop, :proxy_already_set, nil}

      true ->
        case resolve_proxy(env, opts) do
          nil ->
            {:noop, :no_proxy_candidate, nil}

          proxy_url ->
            # Probed once; carried in opts so the evidence and the verdict reuse it.
            opts = Keyword.put(opts, :proxy_url, proxy_url)

            case ask(command, result, env, opts) do
              {:ok, answers} ->
                verdict = interpret(answers, opts)
                maybe_cache(command, verdict, opts)

              {:error, reason} ->
                Logger.debug(
                  "[ProxyEnv] Smart Decide failed for #{inspect(command)}: " <>
                    "#{inspect(reason)} — leaving environment untouched"
                )

                {:noop, :decide_failed, nil}
            end
        end
    end
  end

  defp maybe_cache(command, {:inject, _additions, _detail} = verdict, opts) do
    if cacheable?(opts), do: cache_put(command, verdict)
    verdict
  end

  defp maybe_cache(command, {:noop, reason, _detail} = verdict, _opts) do
    Logger.debug("[ProxyEnv] #{inspect(command)} → noop (#{inspect(reason)})")
    verdict
  end

  defp maybe_cache(_command, verdict, _opts), do: verdict

  @doc """
  Interprets a Smart Decide answer map into an action, purely.

  Accepts either a whole result (`%{"answers" => ...}`) or the answers map.
  Injection requires all three: `needs_proxy` at or above `:min_confidence`,
  the chosen `mechanism` equal to `"proxy_env"`, and `leak_risk` at or below
  `:max_leak_risk`. Anything else — including an absent or unparsable answer —
  returns `{:noop, reason, detail}`.

  Requires `:proxy_url` in `opts` (the candidate that was judged); `:env` is
  used to merge an existing `NO_PROXY`.
  """
  @spec interpret(map() | term(), keyword()) ::
          {:inject, [{String.t(), String.t()}], map()} | {:noop, atom(), map() | nil}
  def interpret(%{"answers" => answers}, opts) when is_map(answers), do: interpret(answers, opts)

  def interpret(answers, opts) when is_map(answers) do
    proxy_url = Keyword.get(opts, :proxy_url)
    needs = noul_probability(Map.get(answers, "needs_proxy"))
    mechanism = choice_value(Map.get(answers, "mechanism"))
    leak = score_value(Map.get(answers, "leak_risk"))

    detail = %{
      "needs_proxy" => needs,
      "mechanism" => mechanism,
      "leak_risk" => leak,
      "proxy_url" => proxy_url
    }

    cond do
      not is_number(needs) ->
        {:noop, :no_verdict, detail}

      needs < Keyword.get(opts, :min_confidence, opt([], :min_confidence)) ->
        {:noop, :low_confidence, detail}

      mechanism != "proxy_env" ->
        {:noop, :other_mechanism, detail}

      is_number(leak) and leak > opt(opts, :max_leak_risk) ->
        {:noop, :leak_risk, detail}

      not is_binary(proxy_url) or proxy_url == "" ->
        {:noop, :no_proxy_candidate, detail}

      true ->
        {:inject, build_env(proxy_url, Keyword.get(opts, :env, []), opts), detail}
    end
  end

  def interpret(_answers, _opts), do: {:noop, :no_verdict, nil}

  # --- Setup (per child process, never persisted) ---

  @doc """
  The env list to inject for `proxy_url`: upper- and lower-case proxy variables
  plus a merged `NO_PROXY`.

  `NO_PROXY` merges (in order) the caller's existing value, the configured
  `:no_proxy` list, and the built-in loopback defaults, de-duplicated
  case-insensitively. Both cases are set because curl, git, Go, npm and apt
  each read a different convention.
  """
  @spec build_env(String.t(), [{String.t(), String.t()}], keyword()) ::
          [{String.t(), String.t()}]
  def build_env(proxy_url, env \\ [], opts \\ []) when is_binary(proxy_url) do
    no_proxy = no_proxy_value(env, opts)

    [{~s(HTTPS_PROXY), proxy_url}, {~s(HTTP_PROXY), proxy_url}, {~s(ALL_PROXY), proxy_url}]
    |> maybe_append({~s(NO_PROXY), no_proxy}, no_proxy != "")
    |> Enum.flat_map(fn {key, value} -> [{key, value}, {String.downcase(key), value}] end)
  end

  defp maybe_append(list, item, true), do: list ++ [item]
  defp maybe_append(list, _item, false), do: list

  @doc "Merged `NO_PROXY` value for `env` + config; `\"\"` when nothing to bypass."
  @spec no_proxy_value([{String.t(), String.t()}], keyword()) :: String.t()
  def no_proxy_value(env \\ [], opts \\ []) do
    existing = env_get(env, "NO_PROXY") || env_get(env, "no_proxy") || ""

    (@default_no_proxy ++ List.wrap(opt(opts, :no_proxy) || []) ++ split_list(existing))
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
    |> dedupe()
    |> Enum.join(",")
  end

  defp split_list(value) when is_binary(value),
    do: String.split(value, [",", " ", ";"], trim: true)

  defp split_list(values) when is_list(values), do: List.wrap(values)
  defp split_list(_), do: []

  defp dedupe(list) do
    {_, acc} =
      Enum.reduce(list, {MapSet.new(), []}, fn item, {seen, acc} ->
        key = String.downcase(item)

        if MapSet.member?(seen, key) do
          {seen, acc}
        else
          {MapSet.put(seen, key), acc ++ [item]}
        end
      end)

    acc
  end

  @doc """
  Merges `additions` into a base environment, with the additions winning.

  Drops the case variants of every injected key from the base, so an injected
  `HTTPS_PROXY` cannot be shadowed by a stale lowercase `https_proxy` (or the
  other way round).
  """
  @spec apply_to_env([{String.t(), String.t()}], [{String.t(), String.t()}]) ::
          [{String.t(), String.t()}]
  def apply_to_env(base, additions) when is_list(base) and is_list(additions) do
    injected = MapSet.new(additions, fn {key, _} -> String.downcase(key) end)

    base
    |> Enum.reject(fn {key, _} -> MapSet.member?(injected, String.downcase(to_string(key))) end)
    |> Kernel.++(additions)
  end

  def apply_to_env(base, _additions), do: base

  @doc """
  Additions from a verdict already reached for this exact `command`.

  `start_process` returns before its child produces output and therefore cannot
  observe a failure, so it reuses the verdict `execute_command` recorded rather
  than making its own model call. In `:pre` mode it instead builds the
  environment straight from the configured proxy (no model call), and returns
  `[]` when there is nothing to inject.
  """
  @spec cached_additions(String.t() | nil) :: [{String.t(), String.t()}]
  def cached_additions(command) when is_binary(command) do
    cond do
      not enabled?() ->
        []

      mode() == :pre ->
        env = Helpers.clean_env()

        case resolve_proxy(env, []) do
          nil -> []
          proxy_url -> build_env(proxy_url, env, proxy_url: proxy_url)
        end

      true ->
        case cache_get(command) do
          {:inject, additions, _detail} -> additions
          _ -> []
        end
    end
  end

  def cached_additions(_command), do: []

  @doc "Empties the decision cache."
  @spec clear_cache() :: :ok
  def clear_cache do
    if :ets.whereis(@cache_table) != :undefined do
      :ets.delete_all_objects(@cache_table)
    end

    :ok
  end

  # --- Facts ---

  defp proxy_already_set?(env) do
    Enum.any?(env, fn {key, value} ->
      String.downcase(to_string(key)) in @proxy_env_keys and to_string(value) != ""
    end)
  end

  # Candidate order: explicit option, configured proxy, an already-exported
  # value, then a live loopback proxy (Clash and friends). Each candidate is
  # TCP-probed and the first reachable one wins, so nothing is ever injected
  # that cannot connect from this host.
  defp resolve_proxy(env, opts) do
    candidates =
      [
        Keyword.get(opts, :proxy_url),
        opt(opts, :proxy_url),
        env_get(env, "HTTPS_PROXY") || env_get(env, "https_proxy")
      ] ++ List.wrap(opt(opts, :fallback_proxies))

    candidates
    |> Enum.filter(fn url -> is_binary(url) and url != "" end)
    |> Enum.find(fn url -> reachable?(url, opts) end)
  end

  defp env_get(env, key) do
    needle = String.downcase(key)

    case Enum.find(env, fn {k, _} -> String.downcase(to_string(k)) == needle end) do
      {_k, value} -> if to_string(value) == "", do: nil, else: to_string(value)
      nil -> nil
    end
  end

  defp reachable?(url, opts) do
    probe = Keyword.get(opts, :probe, &tcp_reachable?/2)
    timeout = Keyword.get(opts, :probe_timeout_ms, opt([], :probe_timeout_ms))

    case probe.(url, timeout) do
      :reachable ->
        true

      :unreachable ->
        Logger.debug("[ProxyEnv] proxy candidate #{url} not reachable")
        false

      other ->
        Logger.debug("[ProxyEnv] unexpected probe result: #{inspect(other)}")
        false
    end
  rescue
    e ->
      Logger.debug("[ProxyEnv] probe crashed: #{Exception.message(e)}")
      false
  end

  @doc """
  Default reachability probe: a short TCP connect to the proxy's host and port.

  Sends no data, so it cannot leak anything; it only answers "is something
  listening there". Port defaults to 443 for `https://`, 80 for `http://`, 1080
  for `socks*://`. A host that does not resolve is unreachable.
  """
  @spec tcp_reachable?(String.t(), pos_integer()) :: :reachable | :unreachable
  def tcp_reachable?(url, timeout \\ 50)

  def tcp_reachable?(url, timeout) when is_binary(url) and is_integer(timeout) do
    uri = URI.parse(url)
    host = uri.host || url
    port = uri.port || default_port(uri.scheme)

    with {:ok, addr} <- :inet.getaddr(String.to_charlist(host), :inet),
         {:ok, socket} <- :gen_tcp.connect(addr, port, [:binary, active: false], timeout) do
      :gen_tcp.close(socket)
      :reachable
    else
      _ -> :unreachable
    end
  end

  def tcp_reachable?(_url, _timeout), do: :unreachable

  defp default_port("https"), do: 443
  defp default_port("http"), do: 80
  defp default_port(scheme) when scheme in ["socks5", "socks5h", "socks4"], do: 1080
  defp default_port(_), do: 443

  # --- Model call ---

  defp ask(command, result, env, opts) do
    decider = Keyword.get(opts, :decider, &SmartDecide.decide/3)
    decide_opts = decide_opts(opt(opts, :model))
    state = evidence(command, result, env, opts)

    task = Task.async(fn -> safe_decider(decider, state, questions(opts), decide_opts) end)

    case Task.yield(task, opt(opts, :timeout)) || Task.shutdown(task, :brutal_kill) do
      {:ok, {:ok, %{"answers" => answers}}} -> {:ok, answers}
      {:ok, {:error, reason}} -> {:error, reason}
      {:ok, other} -> {:error, {:unexpected_decider_result, other}}
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

  # An injected `:decider` is an explicit request to run the decision (tests,
  # overrides), so it implies enabled unless `:enabled` says otherwise —
  # otherwise `config/test.exs` (`enabled: false`) would silently ignore it.
  defp enabled?(opts) do
    case Keyword.fetch(opts, :enabled) do
      {:ok, value} -> value
      :error -> Keyword.has_key?(opts, :decider) or enabled?()
    end
  end

  # Each question is judged independently (the schema is flat), so the network
  # premise and the "measured probe is decisive" rule are repeated per question
  # rather than stated once in the shared state.
  defp questions(opts) do
    premise = Keyword.get(opts, :network_premise, opt([], :network_premise))
    probed? = Keyword.get(opts, :target_probe, opt([], :target_probe))

    %{
      "needs_proxy" => %{
        "type" => "noul",
        "instructions" =>
          premise_clause(premise) <>
            "Question: does this failed outbound network command need an HTTP(S) proxy " <>
            "environment (HTTPS_PROXY/HTTP_PROXY) to reach its destination? " <>
            decisive_clause(probed?) <>
            "Answer no when the failure is authentication or authorization (401/403), a " <>
            "private or unregistered hostname that a DNS record or dnsmasq entry would " <>
            "resolve, a missing local file or bad path, or a domestic endpoint that " <>
            "normally works directly. The proxy inventory and NO_PROXY list in the " <>
            "evidence are reference information about this host, NOT a rule you must " <>
            "obey: weigh them, but decide from the failure evidence. When genuinely " <>
            "unsure, answer no.",
        "criteria" => %{
          "true" => "an HTTP(S) proxy is the better way to reach the destination",
          "false" => "no proxy is needed, or a proxy is not the fix"
        }
      },
      "mechanism" => %{
        "type" => "choice",
        "instructions" => mechanism_instructions(premise, probed?),
        "criteria" => %{
          "proxy_env" =>
            "export HTTPS_PROXY/HTTP_PROXY plus a NO_PROXY bypass list for this command",
          "direct_no_proxy" =>
            "connect directly and set no proxy variable (bypass or drop the proxy instead)",
          "url_rewrite" =>
            "stay direct but rewrite the URL through a public accelerator prefix such as gh-proxy",
          "tool_own_config" =>
            "configure the proxy in the tool's own setting (apt Acquire::https::proxy, git http.proxy, Docker daemon)",
          "unrelated_failure" =>
            "the failure is not a transport problem, so no network change helps"
        }
      },
      "leak_risk" => %{
        "type" => "score",
        "instructions" =>
          "How much credential-exposure risk does routing this command through the " <>
            "candidate proxy create? Assume the candidate is a personal local proxy on " <>
            "this machine or the operator's own corporate proxy, and judge only on what " <>
            "leaves the host: secrets carried in the URL, in headers, or in the uploaded " <>
            "body.",
        "criteria" => ["no risk", "low", "moderate", "severe"]
      }
    }
  end

  defp premise_clause(premise) when is_binary(premise) do
    if String.trim(premise) == "", do: "", else: "Context premise: #{premise}. "
  end

  defp premise_clause(_premise), do: ""

  defp decisive_clause(true) do
    "The decisive fact is whether the target host itself is reachable by direct TCP from " <>
      "this machine, which the evidence states as a measured probe: answer yes when that " <>
      "probe is unreachable, refused, reset or timed out for an endpoint the premise says " <>
      "is blocked, and answer no when the probe shows the target is directly reachable — " <>
      "then the failure sits above the transport layer and a proxy changes nothing. "
  end

  defp decisive_clause(_opts),
    do:
      "Answer yes when the destination is unreachable, refused, reset or TLS-broken from " <>
        "this host and a reachable proxy candidate exists. "

  defp mechanism_instructions(premise, probed?) do
    # The premise is quoted verbatim rather than paraphrased: an operator
    # overriding `:network_premise` must not end up with a stale "mainland
    # China" clause describing a different network.
    context =
      if is_binary(premise) and String.trim(premise) != "" do
        "this host's network premise (#{premise})"
      else
        "the evidence alone, with no assumed regional restriction"
      end

    proxy_env_hint =
      if probed? do
        " Choose proxy_env when the endpoint is overseas and the measured direct probe of " <>
          "the target host was unreachable, refused or reset."
      else
        " Choose proxy_env when the endpoint is overseas and only reachable through a proxy."
      end

    "Given " <>
      context <>
      ", which fix is better for this command?" <>
      proxy_env_hint <>
      " Choose tool_own_config when the tool needs the proxy in its own setting (apt " <>
      "Acquire::https::proxy, git http.proxy, the Docker daemon) rather than in the " <>
      "environment."
  end

  # The model only judges what it is told — it has no network access of its own.
  # Evidence is truncated so the whole prompt stays inside the 8K default model,
  # and any credential in a URL is masked before it leaves this process.
  defp evidence(command, result, env, opts) do
    proxy_url = Keyword.get(opts, :proxy_url)

    [
      "Command: #{truncate(mask_credentials(command), 1_000)}",
      "Target URL: #{mask_credentials(url_in(command) || "none detected")}",
      target_probe_line(command, opts),
      "Candidate proxy: #{proxy_url} (TCP probe from this host: reachable)",
      network_notes_line(opts),
      "Proxy variables already exported: #{proxy_env_summary(env)}",
      "Existing NO_PROXY: #{env_get(env, "NO_PROXY") || env_get(env, "no_proxy") || "none"}",
      "Exit code: #{inspect(Map.get(result, "exit_code"))}",
      "Failure output (tail): #{truncate(mask_credentials(payload_text(result)), 1_500)}"
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  @doc """
  The `{host, port}` a command is trying to reach, for the direct probe.

  Takes the first `http(s)://` URL when present (its own port, else 443/80 by
  scheme); otherwise an `ssh`/`scp`/`sftp` target or a `git@host:path` remote on
  port 22. Returns `nil` when no host can be identified, which the evidence
  reports as an explicit line rather than as a missing one.
  """
  @spec target_host(String.t() | nil) :: {String.t(), pos_integer()} | nil
  def target_host(command) when is_binary(command) do
    case url_in(command) do
      nil -> shell_host(command)
      url -> url_host(url)
    end
  end

  def target_host(_command), do: nil

  defp url_host(url) do
    uri = URI.parse(clean_url(url))

    if is_binary(uri.host) and uri.host != "" do
      {uri.host, uri.port || default_port(uri.scheme)}
    end
  end

  # `url_in/1` grabs `\S+`, so a quote, bracket or trailing comma can ride along.
  defp clean_url(url), do: Regex.replace(~r{["',;)\]<>]+$}, url, "")

  defp shell_host(command) do
    cond do
      host = transport_host(command) -> {host, 22}
      true -> nil
    end
  end

  # Hosts after `ssh`/`scp`/`sftp` (skipping their flags) and `git@host:` remotes.
  @transport_verbs ~w(ssh scp sftp)

  defp transport_host(command) do
    words = String.split(command)

    case Enum.find_index(words, &(&1 in @transport_verbs)) do
      nil ->
        git_remote_host(command)

      index ->
        words
        |> Enum.drop(index + 1)
        |> Enum.find_value(&host_token/1)
        |> case do
          nil -> git_remote_host(command)
          host -> host
        end
    end
  end

  defp host_token(word) do
    candidate =
      word
      |> String.split("@")
      |> List.last()
      |> String.split(":")
      |> List.first()
      |> String.trim_trailing(".")

    if Regex.match?(~r/^[A-Za-z0-9][A-Za-z0-9._-]*$/, candidate) and
         candidate not in @transport_verbs do
      candidate
    end
  end

  defp git_remote_host(command) do
    case Regex.run(~r{(?:[A-Za-z0-9._-]+@)([A-Za-z0-9._-]+):}, command) do
      [_, host | _] -> host
      _ -> nil
    end
  end

  defp target_probe_line(command, opts) do
    if Keyword.get(opts, :target_probe, opt([], :target_probe)) do
      case target_host(command) do
        nil ->
          "Direct TCP probe of target host, no proxy: no target host detected in the command"

        {host, port} ->
          {status, note} = cached_target_probe(host, port, opts)
          "Direct TCP probe of target host, no proxy: #{host}:#{port} → #{status}#{note}"
      end
    else
      "Direct TCP probe of target host: not probed (target_probe disabled)"
    end
  end

  # Reference notes only — the operator's proxy inventory is evidence the model
  # may weigh, never a veto it must obey (see docs/modules/desktop.md).
  defp network_notes_line(opts) do
    case Keyword.get(opts, :network_notes, opt([], :network_notes)) do
      text when is_binary(text) ->
        if String.trim(text) == "" do
          nil
        else
          "Advisory host network notes (reference only, not a rule): #{truncate(text, 600)}"
        end

      _ ->
        nil
    end
  end

  @doc """
  Direct TCP connect to `host:port` — no proxy, no payload, nothing leaked.

  Returns the outcome plus the phrase the model reads, because the *shape* of
  unreachability is what separates a blocked route from a live listener:
  `{:reachable, " (connect 12 ms)"}`, `{:unreachable, " (SYN timeout after 1500 ms)"}`.
  """
  @spec probe_host(String.t(), pos_integer(), pos_integer()) ::
          {:reachable | :unreachable, String.t()}
  def probe_host(host, port, timeout \\ 1_500)

  def probe_host(host, port, timeout) when is_binary(host) and is_integer(port) do
    case resolve_addr(host) do
      {:ok, addr} ->
        {microseconds, connect_result} =
          :timer.tc(fn ->
            :gen_tcp.connect(addr, port, [:binary, active: false], timeout)
          end)

        case connect_result do
          {:ok, socket} ->
            :gen_tcp.close(socket)
            {:reachable, " (connect #{div(microseconds, 1_000)} ms)"}

          {:error, reason} ->
            {:unreachable, " (#{connect_reason(reason, timeout)})"}
        end

      {:error, _} ->
        {:unreachable, " (DNS did not resolve)"}
    end
  rescue
    e -> {:unreachable, " (probe error: #{Exception.message(e)})"}
  end

  def probe_host(_host, _port, _timeout), do: {:unreachable, " (invalid probe arguments)"}

  defp resolve_addr(host) do
    charlist = String.to_charlist(host)

    case :inet.parse_address(charlist) do
      {:ok, addr} -> {:ok, addr}
      _ -> :inet.getaddr(charlist, :inet)
    end
  end

  defp connect_reason(reason, timeout) do
    case reason do
      :etimedout -> "SYN timeout after #{timeout} ms"
      :timeout -> "SYN timeout after #{timeout} ms"
      :econnrefused -> "connection refused"
      :econnreset -> "connection reset by peer"
      :eaddrnotavail -> "no route from this host"
      :ehostunreach -> "network is unreachable"
      :netunreach -> "network is unreachable"
      other -> "connect failed: #{inspect(other)}"
    end
  end

  # Probe results are facts with a short shelf life: cached in the same table as
  # the verdicts, so a burst of failures against one host pays for one connect.
  defp cached_target_probe(host, port, opts) do
    key = {:target_probe, host, port}

    case cache_get(key) do
      :miss ->
        probe = Keyword.get(opts, :target_probe_fn, &probe_host/3)
        timeout = Keyword.get(opts, :target_probe_timeout_ms, opt([], :target_probe_timeout_ms))
        result = probe.(host, port, timeout)

        if cacheable?(opts), do: cache_put(key, result)

        result

      value ->
        value
    end
  rescue
    e ->
      Logger.debug("[ProxyEnv] target probe crashed: #{Exception.message(e)}")
      {:unreachable, " (probe crashed)"}
  end

  defp proxy_env_summary(env) do
    found =
      env
      |> Enum.filter(fn {key, _} -> String.downcase(to_string(key)) in @proxy_env_keys end)
      |> Enum.map(fn {key, value} -> "#{key}=#{value}" end)

    if found == [], do: "none", else: Enum.join(found, ", ")
  end

  defp mask_credentials(text) when is_binary(text) do
    text
    |> then(fn value -> Regex.replace(~r{(\w+://)[^\s/@:]+:[^\s/@]+@}, value, "\\1***:***@") end)
    |> then(fn value ->
      Regex.replace(~r{([?&](?:access_token|token|password)=)\S+}, value, "\\1***")
    end)
  end

  defp mask_credentials(_), do: ""

  defp truncate(text, limit) when is_binary(text) do
    if String.valid?(text) and byte_size(text) > limit do
      "...#{binary_part(text, byte_size(text) - limit, limit)}"
    else
      text
    end
  end

  defp truncate(text, _limit), do: to_string(text)

  # --- Answer extraction ---

  defp noul_probability(answer) when is_map(answer) do
    probabilities = Map.get(answer, "probabilities", %{})

    cond do
      is_number(Map.get(answer, "noul")) -> Map.get(answer, "noul")
      is_number(Map.get(probabilities, "true")) -> Map.get(probabilities, "true")
      is_number(Map.get(probabilities, "yes")) -> Map.get(probabilities, "yes")
      true -> nil
    end
  end

  defp noul_probability(_answer), do: nil

  defp choice_value(answer) when is_map(answer) do
    case Map.get(answer, "choice") do
      value when is_binary(value) -> value
      _ -> nil
    end
  end

  defp choice_value(_answer), do: nil

  defp score_value(answer) when is_map(answer) do
    case Map.get(answer, "score") do
      value when is_number(value) -> value
      _ -> nil
    end
  end

  defp score_value(_answer), do: nil

  # --- Options & cache ---

  defp opt(opts, key), do: Keyword.get(opts, key, Keyword.get(config(), key))

  defp cacheable?(opts) do
    case Keyword.fetch(opts, :cache) do
      {:ok, value} -> value
      :error -> not Keyword.has_key?(opts, :decider)
    end
  end

  # `nil`/blank means the SmartDecide default (Intern-Decision-4B).
  defp decide_opts(model) when is_binary(model) do
    case String.trim(model) do
      "" -> []
      trimmed -> [model: trimmed]
    end
  end

  defp decide_opts(_model), do: []

  defp cache_get(key) do
    if :ets.whereis(@cache_table) == :undefined do
      :miss
    else
      case :ets.lookup(@cache_table, key) do
        [{^key, expires_at, value}] ->
          if System.monotonic_time(:millisecond) < expires_at do
            value
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
end
