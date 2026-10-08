defmodule Exhub.MCP.WebTools.Proxy do
  @moduledoc """
  Proxy selection for the outbound HTTP requests made by the web tools, decided
  by Smart Decide.

  `web_fetch` used to read `Application.get_env(:exhub, :proxy)` and attach that
  proxy to every hackney request. A static injection is wrong in both
  directions: loopback and domestic endpoints get routed through a third-party
  CONNECT proxy (and credential-bearing URLs get handed to it), while a broken
  proxy has no direct fallback — the request simply fails and the tool reports
  it as the target's fault.

  This module makes the HTTP path follow the Desktop precedent,
  `Exhub.MCP.Desktop.ProxyEnv`: a request starts **direct**, and only a failure
  shaped like a blocked route is escalated to one System One call. The judgment
  itself is deliberately not duplicated here — the gates, the candidate list,
  the measured target probe and the three questions (`needs_proxy` noul,
  `mechanism` choice, `leak_risk` score) all live in `ProxyEnv`, and its verdict
  cache is shared with the shell tools. What this module adds is the
  transport-specific glue:

    * `transport_failure/1` — maps hackney/HTTPoison error terms (`:timeout`,
      `:econnrefused`, `{:failed_connect, [{:message, "Name lookup failure…"}]}`,
      `{:tls_alert, …}`) onto the failure-signature vocabulary `ProxyEnv`
      matches. An HTTP status, a certificate rejection or a parse error is not a
      transport failure, so it never asks for a proxy;
    * `evidence_command/2` — renders the request as the `curl …` line `ProxyEnv`
      judges, which is also the key of the shared verdict cache;
    * `route/2` — where the first attempt goes: direct, the configured candidate
      in `:pre` mode, or the cached verdict for this exact request. The
      loopback/`NO_PROXY` bypass is enforced mechanically on all three, because
      there is no child environment to carry `NO_PROXY` — the only way to honour
      it is not to set `proxy:` at all;
    * `judge/4` — turns the verdict back into a proxy URL;
    * `add_proxy/2` and `annotate/3` — request options plus the `"proxy"` field
      the tool returns, so a proxied fetch is visible instead of mysterious.

  Fail closed, like the Desktop gate: a model error, a timeout, an abstention
  below `:min_confidence`, `mechanism != proxy_env`, `leak_risk` above
  `:max_leak_risk`, an unreachable candidate or a credential in the URL all
  leave the request exactly as it was. There is no heuristic fallback — the
  heuristic answer to "is a proxy the fix?" is a guess about the network.

  Two transport constraints shape this glue. hackney reads the OS proxy
  environment (`HTTPS_PROXY`/`HTTP_PROXY`/`ALL_PROXY`) by default, which would
  be a route no decision approved, so `Exhub.MCP.Tools.WebFetch` passes
  `no_proxy_env: true` and the only proxy hackney ever sees is the explicit
  `proxy:` option this module adds. And hackney 1.23.0 cannot CONNECT an
  `https://` target through an `https://` proxy (`:invalid_proxy_transport`), so
  a candidate must be `http://` or socks.

  Mode, thresholds, network premise, advisory notes, probe budgets and the
  cache are read from `Exhub.MCP.Desktop.ProxyEnv` (shared with the shell
  tools). This module's own keys, under `:exhub, Exhub.MCP.WebTools.Proxy`:

    * `:enabled` — master switch. `false` restores the legacy behaviour: the
      static `:exhub, :proxy` on every request, with no model call.
    * `:shared_proxy_candidate` — offer `:exhub, :proxy` (the egress proxy the
      router's LLM routes use) as the first candidate to the decision.
      `ProxyEnv` does not read that key itself, so without this the operator's
      configured proxy would only be reachable through
      `ProxyEnv`'s `:proxy_url`/`fallback_proxies`.

  ## See Also
  - `Exhub.MCP.Desktop.ProxyEnv` — the decision itself (gates + Smart Decide)
  - `Exhub.MCP.Tools.WebFetch` — the caller
  - `docs/modules/web-tools.md` / `docs/modules/desktop.md`
  """

  require Logger

  alias Exhub.MCP.Desktop.ProxyEnv

  @defaults [
    enabled: true,
    shared_proxy_candidate: true
  ]

  # hackney/HTTPoison transport terms rendered into the failure-signature
  # vocabulary `ProxyEnv.proxy_failure?/1` already matches. Naming the shape is
  # all this table does — whether a proxy is the fix stays the model's call.
  @transport_phrases %{
    timeout: "connection timed out",
    connect_timeout: "connection timed out",
    recv_timeout: "connection timed out",
    etimedout: "connection timed out",
    econnrefused: "connection refused",
    econnreset: "connection reset by peer",
    econnaborted: "connection reset by peer",
    ehostunreach: "network is unreachable",
    enetunreach: "network is unreachable",
    enetdown: "network is unreachable",
    enetreset: "network is unreachable",
    eaddrnotavail: "no route from this host",
    eafnosupport: "network is unreachable",
    remote_closed: "empty reply from server",
    nxdomain: "could not resolve host",
    ehostnotfound: "could not resolve host",
    servicenotavailable: "could not resolve host",
    failed_connect: "failed to connect",
    tls_alert: "unable to establish ssl connection",
    ssl_upgrade_failure: "unable to establish ssl connection",
    ssl_upgrade_error: "unable to establish ssl connection",
    handshake_failure: "tls handshake failed",
    unexpected_message: "tls handshake failed",
    record_overflow: "tls handshake failed",
    decode_error: "tls handshake failed",
    http_error_conn: "connection reset by peer"
  }

  # Failures above the transport layer. A proxy cannot fix a rejected
  # certificate, and routing one through a third party is a needless exposure,
  # so the presence of any of these vetoes the decision outright.
  @validation_failures [
    :bad_cert,
    :certificate_expired,
    :certificate_unknown,
    :certificate_revoked,
    :unknown_ca,
    :no_trusted_cert,
    :peer_cert_invalid_sig,
    :peer_cert_not_issued,
    :peer_cert_unknown_ca,
    :peer_cert_unknown_cert,
    :hostname_check_failed,
    :bad_digest,
    :unsupported_certificate
  ]

  @typedoc "Where a request should be sent."
  @type route :: {:direct, map()} | {:proxy, String.t(), map()}

  @typedoc "Outcome of a Smart Decide proxy verdict."
  @type verdict :: {:proxy, String.t(), map()} | {:noop, atom(), map() | nil}

  # --- Configuration ---

  @doc """
  Returns the effective configuration, merging `:exhub, Exhub.MCP.WebTools.Proxy`
  over the in-code defaults.
  """
  @spec config() :: keyword()
  def config do
    Keyword.merge(@defaults, Application.get_env(:exhub, __MODULE__, []))
  end

  @doc "Whether the Smart-Decide proxy path is enabled."
  @spec enabled?(keyword()) :: boolean()
  def enabled?(opts \\ []) do
    Keyword.get(cfg(opts), :enabled, true)
  end

  @doc """
  Whether a failure may be escalated to Smart Decide.

  Requires both this module and `ProxyEnv` to be enabled — `ProxyEnv` is the
  decision engine, so with it off `judge/4` would only ever abstain.
  """
  @spec decision_enabled?() :: boolean()
  def decision_enabled? do
    enabled?() and ProxyEnv.enabled?()
  end

  @doc "The effective decision mode, inherited from `ProxyEnv` (`:on_fail` | `:pre`)."
  @spec mode(keyword()) :: :on_fail | :pre
  def mode(opts \\ []) do
    normalize_mode(Keyword.get(opts, :mode, ProxyEnv.mode()))
  end

  defp normalize_mode(:on_fail), do: :on_fail
  defp normalize_mode(:pre), do: :pre
  defp normalize_mode("on_fail"), do: :on_fail
  defp normalize_mode("pre"), do: :pre
  defp normalize_mode(_), do: :on_fail

  defp cfg(opts) do
    Keyword.merge(config(), Keyword.take(opts, [:enabled, :shared_proxy_candidate]))
  end

  # --- Evidence ---

  @doc """
  The request as the evidence line `ProxyEnv` judges.

  Deliberately unquoted (`ProxyEnv.network_fetch?/1` strips quoted strings
  before looking for a URL) and header-free: a `curl` line carrying an
  `Authorization:` header would trip `ProxyEnv`'s credential guard on every
  authenticated API call, while the `leak_risk` question already weighs headers
  and bodies as risk factors.
  """
  @spec evidence_command(String.t() | nil, String.t() | nil) :: String.t()
  def evidence_command(method, url) do
    m = method |> to_string() |> String.upcase()
    "curl -fsSL -X #{m} #{url}"
  end

  # --- Transport gate ---

  @doc """
  The failure text of `reason` when it is shaped like a blocked route, else `nil`.

  Accepts an `HTTPoison.Error`, a bare atom (`:timeout`), a hackney term
  (`{:failed_connect, […]}`, `{:tls_alert, {:handshake_failure, _}}`) or a
  string. Returns the normalized text rather than a boolean so the caller can
  hand it to `judge/4` as the evidence it quotes back.
  """
  @spec transport_failure(term()) :: String.t() | nil
  def transport_failure(%HTTPoison.Error{reason: reason}) do
    transport_failure(reason)
  end

  def transport_failure(reason) do
    text = failure_text(reason)

    if text != "" and ProxyEnv.proxy_failure?(text), do: text, else: nil
  end

  defp failure_text(reason) when is_binary(reason), do: reason

  defp failure_text(reason) do
    if validation_failure?(reason) do
      ""
    else
      reason
      |> collect_phrases()
      |> Enum.reject(&(&1 == ""))
      |> Enum.uniq()
      |> Enum.join("; ")
    end
  end

  defp collect_phrases(term) when is_tuple(term), do: term |> Tuple.to_list() |> collect_phrases()

  defp collect_phrases(term) when is_list(term) do
    cond do
      term == [] -> []
      printable_charlist?(term) -> [List.to_string(term)]
      true -> Enum.flat_map(term, &collect_phrases/1)
    end
  end

  defp collect_phrases(term) when is_atom(term), do: [Map.get(@transport_phrases, term, "")]
  defp collect_phrases(term) when is_binary(term), do: [term]
  defp collect_phrases(_term), do: []

  # An IP tuple (`{127, 0, 0, 1}`) is a list of integers too: only treat a list
  # as a charlist when every element is a printable character.
  defp printable_charlist?([head | tail]) do
    Enum.all?([head | tail], fn char -> is_integer(char) and char in 32..126 end)
  end

  defp printable_charlist?(_), do: false

  defp validation_failure?(term) do
    terms(term) |> Enum.any?(&(&1 in @validation_failures))
  end

  defp terms(term) when is_tuple(term), do: term |> Tuple.to_list() |> Enum.flat_map(&terms/1)
  defp terms(term) when is_list(term), do: Enum.flat_map(term, &terms/1)
  defp terms(term) when is_atom(term), do: [term]
  defp terms(_term), do: []

  # --- Route of the first attempt ---

  @doc """
  Where the first attempt at `command` (an `evidence_command/2` string) should go.

  Returns `{:direct, meta}` or `{:proxy, proxy_url, meta}`.

  The legacy branch (this module disabled) is returned untouched — the static
  `:exhub, :proxy` on every request, with no credential or bypass guard, exactly
  the pre-decision behaviour, so `enabled: false` is a true escape hatch rather
  than a silent change of transport. On every other branch nothing here consults
  the model, so the mechanical guards are enforced in this function: a credential
  in the URL is refused outright, and a loopback/`NO_PROXY` host is sent direct
  (a cached verdict for some other URL must not drag a local fetch along). Then:

    * `ProxyEnv` in `:pre` mode — the first reachable candidate;
    * `:on_fail` mode with a cached Smart Decide verdict for this exact request
      (recorded by an earlier fetch, or by `execute_command` on the same URL),
      so a repeat fetch pays neither a failing attempt nor a model call.

  So a `NO_PROXY` host is exempt on the judged path as well (`judge/4`), which
  is the one place this differs from the shell tools, where the list is only
  advisory evidence.
  """
  @spec route(String.t() | nil, keyword()) :: route()
  def route(command, opts \\ [])

  def route(command, opts) when is_binary(command) do
    cond do
      not enabled?(opts) ->
        legacy_route()

      ProxyEnv.credentials_in_url?(command) ->
        {:direct, %{"decision" => "direct", "reason" => "credentials_in_url"}}

      # Neither branch below makes a model call, so the mechanical bypass has to
      # be applied here: a proxied loopback request breaks local sockets, and a
      # cached verdict for some other URL must not drag a local fetch along.
      bypassed?(command) ->
        {:direct, %{"decision" => "direct", "reason" => "bypassed"}}

      mode(opts) == :pre ->
        case candidate(opts) do
          nil -> {:direct, %{"decision" => "direct", "reason" => "no_proxy_candidate"}}
          proxy_url -> {:proxy, proxy_url, %{"decision" => "pre", "proxy_url" => proxy_url}}
        end

      true ->
        case command |> ProxyEnv.cached_additions() |> proxy_from_additions() do
          nil -> {:direct, %{"decision" => "direct", "mode" => "on_fail"}}
          proxy_url -> {:proxy, proxy_url, %{"decision" => "cached", "proxy_url" => proxy_url}}
        end
    end
  end

  def route(_command, _opts), do: {:direct, %{"decision" => "direct", "reason" => "no_request"}}

  # The legacy branch is byte-identical to the pre-decision behaviour: the static
  # `:exhub, :proxy` on every request, with no credential or bypass guard, so
  # `enabled: false` is a true escape hatch rather than a silent change of
  # transport.
  defp legacy_route do
    case shared_proxy() do
      nil -> {:direct, %{"decision" => "disabled"}}
      proxy_url -> {:proxy, proxy_url, %{"decision" => "static", "proxy_url" => proxy_url}}
    end
  end

  defp proxy_from_additions(additions) when is_list(additions) do
    case Enum.find(additions, fn {key, _} -> to_string(key) == "HTTPS_PROXY" end) do
      {_key, value} -> proxy_url_value(value)
      nil -> nil
    end
  end

  defp proxy_from_additions(_additions), do: nil

  # --- Candidates ---

  @doc """
  The proxy candidates, in priority order.

  Explicit option first, then the shared `:exhub, :proxy` egress proxy (the key
  `ProxyEnv` does not read, so it is bridged here), then `ProxyEnv`'s own
  `:proxy_url`, an exported `HTTPS_PROXY`, and its `fallback_proxies` (a live
  local Clash-style listener). Blank, duplicated and non-URL entries are
  dropped.
  """
  @spec candidates(keyword()) :: [String.t()]
  def candidates(opts \\ []) do
    pc = ProxyEnv.config()

    shared =
      if Keyword.get(cfg(opts), :shared_proxy_candidate, true), do: shared_proxy(), else: nil

    ([
       Keyword.get(opts, :proxy_url),
       shared,
       Keyword.get(pc, :proxy_url),
       System.get_env("HTTPS_PROXY") || System.get_env("https_proxy")
     ] ++ List.wrap(Keyword.get(pc, :fallback_proxies, [])))
    |> Enum.map(&proxy_url_value/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
  end

  @doc "The first candidate that TCP-connects from this host, or `nil`."
  @spec candidate(keyword()) :: String.t() | nil
  def candidate(opts \\ []) do
    probe = Keyword.get(opts, :probe, &ProxyEnv.tcp_reachable?/2)

    timeout =
      Keyword.get(opts, :probe_timeout_ms, Keyword.get(ProxyEnv.config(), :probe_timeout_ms, 50))

    Enum.find(candidates(opts), fn url ->
      case safe_probe(probe, url, timeout) do
        true ->
          log_https_candidate(url)
          true

        false ->
          Logger.debug("[WebTools.Proxy] candidate #{url} not reachable")
          false
      end
    end)
  end

  defp safe_probe(probe, url, timeout) do
    probe.(url, timeout) == :reachable
  rescue
    _ -> false
  catch
    _, _ -> false
  end

  # hackney 1.23.0 rejects an https proxy used against an https target
  # (`:invalid_proxy_transport`), so name the trap once when the chosen
  # candidate is one instead of letting the retry fail mysteriously.
  defp log_https_candidate(url) do
    case URI.parse(url) do
      %URI{scheme: "https"} ->
        Logger.debug(
          "[WebTools.Proxy] #{url} is an https proxy: hackney 1.23.0 cannot CONNECT " <>
            "an https target through it — use http:// or socks"
        )

      _ ->
        :ok
    end
  end

  defp shared_proxy, do: proxy_url_value(Application.get_env(:exhub, :proxy, ""))

  defp proxy_url_value(value) when is_binary(value) do
    trimmed = String.trim(value)

    case URI.parse(trimmed) do
      %URI{host: host} when is_binary(host) and host != "" -> trimmed
      _ -> nil
    end
  end

  defp proxy_url_value(_value), do: nil

  # --- Mechanical bypass ---

  @doc """
  Whether the host of `command` is exempt from any proxy (`NO_PROXY`).

  Loopback is always exempt (`ProxyEnv`'s built-in default), because a proxy
  with no loopback exemption breaks local sockets. This is a mechanical guard,
  applied only where no model call happens; in the judged path the bypass list
  is evidence the model may overrule, exactly as in `ProxyEnv`.
  """
  @spec bypassed?(String.t() | nil) :: boolean()
  def bypassed?(command) when is_binary(command) do
    case ProxyEnv.target_host(command) do
      {host, _port} -> bypassed_host?(host)
      nil -> false
    end
  end

  def bypassed?(_command), do: false

  defp bypassed_host?(host) do
    host
    |> String.downcase()
    |> then(fn h ->
      ProxyEnv.no_proxy_value(no_proxy_env(), [])
      |> String.split(",", trim: true)
      |> Enum.map(&normalize_bypass_entry/1)
      |> Enum.reject(&(&1 == ""))
      |> Enum.any?(fn entry ->
        entry == "*" or h == entry or String.ends_with?(h, "." <> entry)
      end)
    end)
  end

  # The `NO_PROXY` conventions all mean the same thing here: `example.com`,
  # `.example.com` and `*.example.com` each cover `a.example.com`.
  defp normalize_bypass_entry(entry) do
    entry
    |> String.trim()
    |> String.downcase()
    |> String.replace_leading("*.", "")
    |> String.trim_leading(".")
  end

  # Only `NO_PROXY` is taken from the VM environment: forwarding an exported
  # `HTTPS_PROXY` as well would let `ProxyEnv`'s `proxy_already_set?` gate
  # abstain on every request. `Exhub.MCP.Tools.WebFetch` in turn tells hackney to
  # ignore the OS proxy environment (`no_proxy_env: true`), so for this caller
  # the env is a bypass list, not a transport.
  defp no_proxy_env do
    System.get_env()
    |> Enum.filter(fn {key, _} -> String.downcase(key) == "no_proxy" end)
    |> Enum.map(fn {key, value} -> {key, to_string(value)} end)
  end

  # --- Verdict ---

  @doc """
  Asks Smart Decide whether `method url` should go through a proxy.

  `failure` is the normalized text from `transport_failure/1`; it travels to
  `ProxyEnv` as the stderr of the judged request. Options are forwarded to
  `ProxyEnv.judge/3`, so tests inject `:decider` (and `:probe`) and stay offline.

  Returns `{:proxy, proxy_url, detail}` when the verdict is to proxy, or
  `{:noop, reason, detail}` — fail closed, the request stays as it was.

  A `NO_PROXY`/loopback target short-circuits to `{:noop, :bypassed, …}` without
  reaching the model: `ProxyEnv` writes that exemption into the child's
  environment mechanically, and here there is no environment to exempt — only a
  proxy that would ignore it. Pass `respect_bypass: false` to judge anyway.
  """
  @spec judge(String.t() | nil, String.t() | nil, String.t() | term(), keyword()) :: verdict()
  def judge(method, url, failure, opts \\ []) do
    command = evidence_command(method, url)

    if Keyword.get(opts, :respect_bypass, true) and bypassed?(command) do
      {:noop, :bypassed, %{"proxy_url" => nil, "command" => command}}
    else
      judge_request(command, failure, opts)
    end
  end

  defp judge_request(command, failure, opts) do
    payload = %{
      "exit_code" => 1,
      "stdout" => "",
      "stderr" => to_string(failure),
      "command" => command
    }

    opts = opts |> with_candidate() |> Keyword.put_new(:env, no_proxy_env())

    case ProxyEnv.judge(command, {:ok, payload}, opts) do
      {:inject, additions, detail} ->
        detail = detail || %{}

        case proxy_from_additions(additions) || Map.get(detail, "proxy_url") do
          proxy_url when is_binary(proxy_url) and proxy_url != "" -> {:proxy, proxy_url, detail}
          _ -> {:noop, :no_proxy_candidate, detail}
        end

      {:noop, reason, detail} ->
        {:noop, reason, detail}
    end
  end

  # Resolve the candidate list first, so `ProxyEnv` judges the proxy this caller
  # would actually use and never one that cannot connect from here.
  defp with_candidate(opts) do
    if Keyword.has_key?(opts, :proxy_url) do
      opts
    else
      Keyword.put(opts, :proxy_url, candidate(opts))
    end
  end

  @doc "Whether a `noop` verdict actively advises going direct (`mechanism == direct_no_proxy`)."
  @spec advises_direct?(atom() | nil, map() | nil) :: boolean()
  def advises_direct?(_reason, detail) when is_map(detail) do
    Map.get(detail, "mechanism") == "direct_no_proxy"
  end

  def advises_direct?(_reason, _detail), do: false

  # --- Request options and reporting ---

  @doc """
  Adds `proxy_url` to a request option list's `:hackney` keyword.

  No-op when the proxy is `nil`/blank, so an unjudged request is byte-identical
  to one that never consulted this module.
  """
  @spec add_proxy(keyword(), String.t() | nil) :: keyword()
  def add_proxy(options, proxy_url) when is_list(options) do
    case proxy_url_value(proxy_url) do
      nil -> options
      url -> Keyword.update(options, :hackney, [proxy: url], &Keyword.put(&1, :proxy, url))
    end
  end

  @doc """
  The `"proxy"` field returned by the tool: what was used and why.

  Mirrors the `proxy_env` note `execute_command` adds, so a proxied fetch is
  visible to the caller instead of arriving as an unexplained second attempt.
  """
  @spec annotate(String.t() | nil, map() | nil, pos_integer()) :: map()
  def annotate(proxy_url, detail \\ nil, attempt \\ 1) do
    detail = detail || %{}

    %{
      "proxy_url" => proxy_url,
      "decision" => Map.get(detail, "decision"),
      "needs_proxy" => Map.get(detail, "needs_proxy"),
      "mechanism" => Map.get(detail, "mechanism"),
      "leak_risk" => Map.get(detail, "leak_risk"),
      "attempt" => attempt
    }
  end

  @doc """
  A short suffix explaining the proxy path, for an error message.

  Returns `""` when there is nothing to report, so an ordinary failure keeps its
  original wording.
  """
  @spec failure_note(keyword()) :: String.t()
  def failure_note(opts \\ []) do
    parts =
      [
        transport_phrase(Keyword.get(opts, :proxy_url), Keyword.get(opts, :decision)),
        verdict_phrase(Keyword.get(opts, :verdict), Keyword.get(opts, :detail))
      ]
      |> Enum.reject(&(is_nil(&1) || &1 == ""))

    case parts do
      [] -> ""
      parts -> "; " <> Enum.join(parts, "; ")
    end
  end

  defp transport_phrase(nil, _decision), do: nil
  defp transport_phrase(url, "static"), do: "the configured proxy #{url} was used"
  defp transport_phrase(url, "pre"), do: "the pre-selected proxy #{url} was used"
  defp transport_phrase(url, "cached"), do: "the cached proxy verdict (#{url}) was used"

  defp transport_phrase(url, "smart_decide"),
    do: "the Smart Decide proxy #{url} was used"

  defp transport_phrase(url, _decision), do: "via proxy #{url}"

  defp verdict_phrase(nil, _detail), do: nil

  defp verdict_phrase(:decision_disabled, _detail),
    do: "the proxy decision is disabled (Exhub.MCP.WebTools.Proxy/ProxyEnv)"

  defp verdict_phrase(:attempts_exhausted, _detail),
    do: "the judged proxy also failed"

  defp verdict_phrase(:already_proxied, _detail),
    do: "Smart Decide chose the proxy already in use"

  # Pre-model guards: facts about the request, not judgment calls.
  defp verdict_phrase(:bypassed, _detail),
    do: "the target is exempt from any proxy (NO_PROXY)"

  defp verdict_phrase(:credentials_in_url, _detail),
    do: "the URL carries a credential, which is never handed to a proxy"

  defp verdict_phrase(:no_proxy_candidate, _detail),
    do: "no proxy candidate is reachable from this host"

  defp verdict_phrase(reason, detail) do
    detail = detail || %{}
    needs = format_number(Map.get(detail, "needs_proxy"))
    mechanism = Map.get(detail, "mechanism") || "n/a"
    leak = format_number(Map.get(detail, "leak_risk"))

    "Smart Decide kept it direct (#{reason}; needs_proxy #{needs}, " <>
      "mechanism #{mechanism}, leak_risk #{leak})"
  end

  defp format_number(value) when is_float(value), do: :erlang.float_to_binary(value, decimals: 3)
  defp format_number(value) when is_integer(value), do: Integer.to_string(value)
  defp format_number(_value), do: "n/a"
end
