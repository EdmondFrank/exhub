defmodule Exhub.MCP.WebTools.ProxyTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Desktop.ProxyEnv
  alias Exhub.MCP.WebTools.Proxy

  @proxy_url "https://hj.runjs.cn:31443"
  @cache_table :exhub_proxy_env_cache

  # Both baselines are restored after every test, so a failed assertion cannot
  # leak an override into the next one.
  #
  # `ProxyEnv` starts disabled and `:exhub, :proxy` is unset: nothing in this
  # file touches the network or the model unless the test injects a `:decider`
  # and/or a `:probe`, exactly as in proxy_env_test.exs.
  @web_baseline [enabled: true, shared_proxy_candidate: true]
  @env_baseline [enabled: false, target_probe: false]

  setup do
    Application.put_env(:exhub, Proxy, @web_baseline)
    Application.put_env(:exhub, ProxyEnv, @env_baseline)
    Application.delete_env(:exhub, :proxy)
    ensure_cache_table()
    ProxyEnv.clear_cache()
    on_exit(fn -> ProxyEnv.clear_cache() end)
    :ok
  end

  defp ensure_cache_table do
    if :ets.whereis(@cache_table) == :undefined do
      # Same shape as ProxyEnv's own table: the supervised owner is not running
      # under `mix test --no-start`, so the seeding tests create it here.
      :ets.new(@cache_table, [:set, :named_table, :public, read_concurrency: true])
    end

    :ok
  end

  # --- Fixtures ------------------------------------------------------------

  defp answers(needs, mechanism, leak) do
    %{
      "answers" => %{
        "needs_proxy" => %{"type" => "noul", "noul" => needs},
        "mechanism" => %{"type" => "choice", "choice" => mechanism},
        "leak_risk" => %{"type" => "score", "score" => leak}
      }
    }
  end

  defp decider(needs, mechanism, leak) do
    fn _state, _questions, _opts -> {:ok, answers(needs, mechanism, leak)} end
  end

  defp spy_decider(name) do
    fn _state, _questions, _opts ->
      send(name, :decided)
      {:ok, answers(0.95, "proxy_env", 0.4)}
    end
  end

  defp judge_opts(overrides \\ []) do
    Keyword.merge(
      [
        decider: decider(0.95, "proxy_env", 0.4),
        probe: fn _url, _timeout -> :reachable end,
        proxy_url: @proxy_url
      ],
      overrides
    )
  end

  defp with_web_config(overrides) do
    on_exit(fn -> Application.put_env(:exhub, Proxy, @web_baseline) end)
    Application.put_env(:exhub, Proxy, Keyword.merge(@web_baseline, overrides))
  end

  defp with_env_config(overrides) do
    on_exit(fn -> Application.put_env(:exhub, ProxyEnv, @env_baseline) end)
    Application.put_env(:exhub, ProxyEnv, Keyword.merge(@env_baseline, overrides))
  end

  defp seed_verdict(command, proxy_url) do
    expires_at = System.monotonic_time(:millisecond) + 60_000

    :ets.insert(
      @cache_table,
      {command, expires_at, {:inject, [{"HTTPS_PROXY", proxy_url}], %{}}}
    )
  end

  # --- Gate 1: is this failure even a transport failure? -------------------

  describe "transport_failure/1" do
    test "normalizes bare inet/hackney atoms into ProxyEnv's vocabulary" do
      assert Proxy.transport_failure(:timeout) =~ "connection timed out"
      assert Proxy.transport_failure(:connect_timeout) =~ "connection timed out"
      assert Proxy.transport_failure(:econnrefused) =~ "connection refused"
      assert Proxy.transport_failure(:econnreset) =~ "connection reset by peer"
      assert Proxy.transport_failure(:nxdomain) =~ "could not resolve host"
      assert Proxy.transport_failure(:enetunreach) =~ "network is unreachable"
    end

    test "reads hackney's {:failed_connect, …} shapes" do
      assert Proxy.transport_failure(
               {:failed_connect,
                [{:to_address, {{127, 0, 0, 1}, 443}}, {:inet, :inet, :econnrefused}]}
             ) =~
               "failed to connect"

      assert Proxy.transport_failure(
               {:failed_connect, [{:message, ~c"Name lookup failure"}, {:error, :nxdomain}]}
             ) =~
               "could not resolve host"
    end

    test "treats a broken TLS handshake as transport, a rejected certificate as not" do
      text = Proxy.transport_failure({:error, {:tls_alert, {:handshake_failure, :none}}})
      assert text =~ "unable to establish ssl connection"

      assert is_nil(Proxy.transport_failure({:bad_cert, :unknown_ca}))
      assert is_nil(Proxy.transport_failure(:certificate_expired))

      assert is_nil(
               Proxy.transport_failure({:failed_connect, [{:error, :hostname_check_failed}]})
             )
    end

    test "unwraps HTTPoison errors" do
      assert Proxy.transport_failure(%HTTPoison.Error{reason: :timeout}) =~ "connection timed out"
    end

    test "leaves application-layer and unparsable failures alone" do
      assert is_nil(Proxy.transport_failure(:unauthorized))
      assert is_nil(Proxy.transport_failure(:closed))
      assert is_nil(Proxy.transport_failure("invalid JSON response"))
      assert is_nil(Proxy.transport_failure("{}"))
      assert is_nil(Proxy.transport_failure({}))
      assert is_nil(Proxy.transport_failure(nil))
    end

    test "passes through text that already looks like a blocked route" do
      assert Proxy.transport_failure("curl: (28) Connection timed out") =~ "curl: (28)"
    end
  end

  # --- Evidence line -------------------------------------------------------

  describe "evidence_command/2" do
    test "renders an unquoted curl line ProxyEnv can read" do
      command = Proxy.evidence_command("get", "https://example.com/docs")
      assert command == "curl -fsSL -X GET https://example.com/docs"

      assert ProxyEnv.network_fetch?(command)
      assert ProxyEnv.url_in(command) == "https://example.com/docs"
      assert ProxyEnv.target_host(command) == {"example.com", 443}
    end

    test "a credential in the URL is visible to ProxyEnv's guard" do
      command = Proxy.evidence_command("GET", "https://user:pass@example.com/x")
      assert ProxyEnv.credentials_in_url?(command)
    end

    test "headers are deliberately not part of the evidence" do
      refute ProxyEnv.credentials_in_url?(Proxy.evidence_command("GET", "https://example.com/x"))
    end
  end

  # --- Where the first attempt goes ---------------------------------------

  describe "route/2" do
    test "disabled: the legacy static proxy, byte-identical to the old behaviour" do
      Application.put_env(:exhub, :proxy, @proxy_url)
      with_web_config(enabled: false)

      assert {:proxy, @proxy_url, %{"decision" => "static"}} =
               Proxy.route(Proxy.evidence_command("GET", "https://example.com/x"))
    end

    test "disabled without a configured proxy: direct" do
      with_web_config(enabled: false)

      assert {:direct, %{"decision" => "disabled"}} =
               Proxy.route(Proxy.evidence_command("GET", "https://example.com/x"))
    end

    test "enabled, :on_fail and no verdict yet: direct" do
      command = Proxy.evidence_command("GET", "https://example.com/x")

      assert {:direct, %{"decision" => "direct", "mode" => "on_fail"}} = Proxy.route(command)
    end

    test "enabled, :pre mode: the first reachable candidate, with no model call" do
      with_env_config(proxy_url: nil, fallback_proxies: [])

      assert {:proxy, @proxy_url, %{"decision" => "pre"}} =
               Proxy.route(
                 Proxy.evidence_command("GET", "https://example.com/x"),
                 mode: :pre,
                 proxy_url: @proxy_url,
                 probe: fn _url, _timeout -> :reachable end
               )
    end

    test "enabled, :pre mode with nothing reachable: direct" do
      with_env_config(proxy_url: nil, fallback_proxies: [])

      assert {:direct, %{"decision" => "direct", "reason" => "no_proxy_candidate"}} =
               Proxy.route(
                 Proxy.evidence_command("GET", "https://example.com/x"),
                 mode: :pre,
                 proxy_url: @proxy_url,
                 probe: fn _url, _timeout -> :unreachable end
               )
    end

    test "loopback is bypassed on the paths that make no model call" do
      with_env_config(proxy_url: nil, fallback_proxies: [])

      assert {:direct, %{"decision" => "direct", "reason" => "bypassed"}} =
               Proxy.route(
                 Proxy.evidence_command("GET", "http://127.0.0.1:9069/health"),
                 mode: :pre,
                 proxy_url: @proxy_url,
                 probe: fn _url, _timeout -> :reachable end
               )
    end

    test "a cached verdict for this exact request is reused" do
      with_env_config(enabled: true)
      command = Proxy.evidence_command("GET", "https://example.com/slow")
      seed_verdict(command, @proxy_url)

      assert {:proxy, @proxy_url, %{"decision" => "cached"}} = Proxy.route(command)
    end

    test "a cached verdict never drags a bypassed host through the proxy" do
      with_env_config(enabled: true)
      command = Proxy.evidence_command("GET", "http://localhost:9069/health")
      seed_verdict(command, @proxy_url)

      assert {:direct, %{"reason" => "bypassed"}} = Proxy.route(command)
    end

    test "a credential in the URL is never proxied without a judgment" do
      with_env_config(proxy_url: nil, fallback_proxies: [])

      assert {:direct, %{"reason" => "credentials_in_url"}} =
               Proxy.route(
                 Proxy.evidence_command("GET", "https://oauth2:tok@example.com/x"),
                 mode: :pre,
                 proxy_url: @proxy_url,
                 probe: fn _url, _timeout -> :reachable end
               )
    end

    test "a non-string request stays direct" do
      assert {:direct, %{"reason" => "no_request"}} = Proxy.route(nil)
    end
  end

  # --- Candidates ----------------------------------------------------------

  describe "candidates/1" do
    test "bridges the shared :exhub, :proxy into ProxyEnv's candidate order" do
      Application.put_env(:exhub, :proxy, "http://shared:1")
      with_env_config(proxy_url: "http://pc:2", fallback_proxies: ["http://fb:3"])

      got = Proxy.candidates(proxy_url: "http://explicit:4")

      assert Enum.at(got, 0) == "http://explicit:4"
      assert Enum.at(got, 1) == "http://shared:1"
      assert Enum.at(got, 2) == "http://pc:2"
      assert Enum.find_index(got, &(&1 == "http://fb:3")) >= 3
    end

    test "shared_proxy_candidate: false drops the bridged proxy" do
      Application.put_env(:exhub, :proxy, "http://shared:1")
      with_web_config(shared_proxy_candidate: false)

      refute "http://shared:1" in Proxy.candidates()
    end

    test "blanks, non-URLs and duplicates are dropped" do
      assert Proxy.candidates(proxy_url: "") == Proxy.candidates()
      assert Proxy.candidates(proxy_url: "not a url") == Proxy.candidates()

      assert Proxy.candidates(proxy_url: @proxy_url) |> Enum.uniq() ==
               Proxy.candidates(proxy_url: @proxy_url)

      assert "not a url" not in Proxy.candidates(proxy_url: "not a url")
    end

    test "candidate/1 picks the first reachable one" do
      with_env_config(proxy_url: nil, fallback_proxies: ["http://dead:1", @proxy_url])
      test_pid = self()

      probe = fn url, _timeout ->
        send(test_pid, {:probed, url})
        (url == @proxy_url && :reachable) || :unreachable
      end

      assert Proxy.candidate(probe: probe) == @proxy_url
      assert_received {:probed, "http://dead:1"}
    end

    test "candidate/1 survives a crashing probe" do
      with_env_config(proxy_url: nil, fallback_proxies: ["http://dead:1"])
      assert is_nil(Proxy.candidate(probe: fn _url, _timeout -> raise "boom" end))
    end
  end

  describe "bypassed?/1" do
    test "loopback defaults and configured entries exempt the host" do
      assert Proxy.bypassed?(Proxy.evidence_command("GET", "http://localhost:9069/x"))
      assert Proxy.bypassed?(Proxy.evidence_command("GET", "http://127.0.0.1:19999/x"))
      refute Proxy.bypassed?(Proxy.evidence_command("GET", "https://example.com/x"))
      refute Proxy.bypassed?(Proxy.evidence_command("GET", "https://notlocalhost.example/x"))
    end

    test "ProxyEnv's no_proxy list and a wildcard are honoured" do
      with_env_config(no_proxy: ["intranet.example", "*.corp.example"])
      assert Proxy.bypassed?(Proxy.evidence_command("GET", "https://intranet.example/x"))
      assert Proxy.bypassed?(Proxy.evidence_command("GET", "https://a.corp.example/x"))

      with_env_config(no_proxy: ["*"])
      assert Proxy.bypassed?(Proxy.evidence_command("GET", "https://example.com/x"))
    end
  end

  # --- The Smart Decide verdict -------------------------------------------

  describe "judge/4" do
    test "an approving verdict becomes the judged proxy URL" do
      assert {:proxy, @proxy_url, detail} =
               Proxy.judge("GET", "https://example.com/x", "connection timed out", judge_opts())

      assert detail["needs_proxy"] == 0.95
      assert detail["mechanism"] == "proxy_env"
      assert detail["proxy_url"] == @proxy_url
    end

    test "low confidence abstains" do
      assert {:noop, :low_confidence, detail} =
               Proxy.judge(
                 "GET",
                 "https://example.com/x",
                 "connection timed out",
                 judge_opts(decider: decider(0.31, "proxy_env", 0.1))
               )

      assert detail["needs_proxy"] == 0.31
    end

    test "another mechanism abstains, and direct advice is exposed" do
      opts = judge_opts(decider: decider(0.99, "direct_no_proxy", 0.0))

      assert {:noop, :other_mechanism, detail} =
               Proxy.judge("GET", "https://example.com/x", "connection refused", opts)

      assert Proxy.advises_direct?(:other_mechanism, detail)
      refute Proxy.advises_direct?(:other_mechanism, %{"mechanism" => "url_rewrite"})
    end

    test "a leak risk above the ceiling refuses injection" do
      assert {:noop, :leak_risk, _detail} =
               Proxy.judge(
                 "POST",
                 "https://example.com/upload",
                 "connection reset by peer",
                 judge_opts(decider: decider(0.99, "proxy_env", 2.9))
               )
    end

    test "a credential in the URL stops the decision before any API call" do
      opts = judge_opts(decider: spy_decider(self()))

      assert {:noop, :credentials_in_url, _} =
               Proxy.judge("GET", "https://user:pass@example.com/x", "connection timed out", opts)

      refute_received :decided
    end

    test "a bypassed target never reaches the model" do
      opts = judge_opts(decider: spy_decider(self()), proxy_url: nil, env: [])

      assert {:noop, :bypassed, _} =
               Proxy.judge("GET", "http://127.0.0.1:9069/health", "connection timed out", opts)

      refute_received :decided
    end

    test "respect_bypass: false hands the exempt host to the model anyway" do
      with_env_config(no_proxy: ["example.com"])

      assert {:proxy, @proxy_url, _detail} =
               Proxy.judge(
                 "GET",
                 "https://example.com/x",
                 "connection timed out",
                 judge_opts(respect_bypass: false)
               )
    end

    test "an unreachable candidate never gets injected" do
      with_env_config(proxy_url: nil, fallback_proxies: [])

      assert {:noop, :no_proxy_candidate, _} =
               Proxy.judge(
                 "GET",
                 "https://example.com/x",
                 "connection timed out",
                 judge_opts(probe: fn _url, _timeout -> :unreachable end)
               )
    end

    test "a model error fails closed" do
      assert {:noop, :decide_failed, nil} =
               Proxy.judge(
                 "GET",
                 "https://example.com/x",
                 "connection timed out",
                 judge_opts(decider: fn _s, _q, _o -> {:error, "System One is down"} end)
               )
    end

    test "the failure text travels to the model as the request's stderr" do
      test_pid = self()

      decider = fn state, _questions, _opts ->
        send(test_pid, {:state, state})
        {:ok, answers(0.95, "proxy_env", 0.4)}
      end

      Proxy.judge(
        "GET",
        "https://example.com/x",
        "connection timed out",
        judge_opts(decider: decider)
      )

      assert_received {:state, state}
      assert state =~ "curl -fsSL -X GET https://example.com/x"
      assert state =~ "connection timed out"
      assert state =~ @proxy_url
    end
  end

  # --- Options and reporting ----------------------------------------------

  describe "add_proxy/2" do
    test "adds the proxy to the existing hackney options" do
      options = [hackney: [follow_redirect: true, timeout: 30_000]]

      assert Proxy.add_proxy(options, @proxy_url) == [
               hackney: [proxy: @proxy_url, follow_redirect: true, timeout: 30_000]
             ]
    end

    test "leaves the options untouched without a proxy" do
      options = [hackney: [follow_redirect: true]]
      assert Proxy.add_proxy(options, nil) == options
      assert Proxy.add_proxy(options, "") == options
      assert Proxy.add_proxy(options, "nonsense") == options
    end

    test "creates the hackney keyword when absent" do
      assert Proxy.add_proxy([recv_timeout: 1_000], @proxy_url) == [
               recv_timeout: 1_000,
               hackney: [proxy: @proxy_url]
             ]
    end
  end

  describe "annotate/3" do
    test "reports the proxy and the verdict that chose it" do
      detail = %{
        "decision" => "smart_decide",
        "needs_proxy" => 0.95,
        "mechanism" => "proxy_env",
        "leak_risk" => 0.4
      }

      assert Proxy.annotate(@proxy_url, detail, 2) == %{
               "proxy_url" => @proxy_url,
               "decision" => "smart_decide",
               "needs_proxy" => 0.95,
               "mechanism" => "proxy_env",
               "leak_risk" => 0.4,
               "attempt" => 2
             }
    end

    test "tolerates a missing detail" do
      assert %{"needs_proxy" => nil, "attempt" => 1} = Proxy.annotate(@proxy_url)
    end
  end

  describe "failure_note/1" do
    test "says nothing when there is nothing to report" do
      assert Proxy.failure_note(proxy_url: nil, decision: "direct", verdict: nil, detail: nil) ==
               ""
    end

    test "names the transport in use" do
      note = Proxy.failure_note(proxy_url: @proxy_url, decision: "static")
      assert note =~ "the configured proxy #{@proxy_url} was used"

      assert Proxy.failure_note(proxy_url: @proxy_url, decision: "cached") =~
               "cached proxy verdict"

      assert Proxy.failure_note(proxy_url: @proxy_url, decision: "pre") =~ "pre-selected proxy"
    end

    test "explains an abstention with the numbers behind it" do
      note =
        Proxy.failure_note(
          verdict: :low_confidence,
          detail: %{"needs_proxy" => 0.42, "mechanism" => "proxy_env", "leak_risk" => 1.0}
        )

      assert note =~ "Smart Decide kept it direct (low_confidence"
      assert note =~ "needs_proxy 0.420"
      assert note =~ "mechanism proxy_env"
      assert note =~ "leak_risk 1.000"
    end

    test "reports a disabled decision and exhausted attempts" do
      assert Proxy.failure_note(verdict: :decision_disabled) =~ "proxy decision is disabled"
      assert Proxy.failure_note(verdict: :attempts_exhausted) =~ "also failed"
      assert Proxy.failure_note(verdict: :already_proxied) =~ "already in use"
    end

    test "names the pre-model guards instead of blaming the model" do
      assert Proxy.failure_note(verdict: :bypassed) =~ "exempt from any proxy"
      assert Proxy.failure_note(verdict: :credentials_in_url) =~ "never handed to a proxy"
      assert Proxy.failure_note(verdict: :no_proxy_candidate) =~ "no proxy candidate is reachable"
    end
  end

  describe "decision_enabled?/0" do
    test "follows both switches" do
      refute Proxy.decision_enabled?()

      with_env_config(enabled: true)
      assert Proxy.decision_enabled?()

      with_web_config(enabled: false)
      refute Proxy.decision_enabled?()
    end
  end
end
