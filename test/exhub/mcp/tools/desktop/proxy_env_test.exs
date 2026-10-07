defmodule Exhub.MCP.Tools.Desktop.ProxyEnvTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Desktop.ProxyEnv
  alias Exhub.MCP.Tools.Desktop.ExecuteCommand

  @tmp_dir System.tmp_dir!()

  @proxy_url "https://hj.runjs.cn:31443"
  # Two ports nothing ever listens on, so the probe is deterministic offline.
  @dead_proxy "http://127.0.0.1:1"

  # Every test starts and ends on the same known config, so an assertion
  # failure mid-test cannot leak an override into the next one.
  #
  # `target_probe: false` keeps the suite off the network: the evidence layer
  # would otherwise open a real TCP connection to each command's target host.
  # Tests that want the probe pass `target_probe: true` plus a `:target_probe_fn`
  # stub, or exercise `probe_host/3` against loopback.
  @test_baseline [enabled: false, target_probe: false]

  setup do
    Application.put_env(:exhub, ProxyEnv, @test_baseline)
    ProxyEnv.clear_cache()
    on_exit(&ProxyEnv.clear_cache/0)
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

  defp failing(command, opts \\ []) do
    {:ok,
     %{
       "exit_code" => Keyword.get(opts, :exit_code, 7),
       "stdout" => "",
       "stderr" =>
         Keyword.get(opts, :stderr, "curl: (7) Failed to connect to storage.googleapis.com"),
       "command" => command
     }}
  end

  defp judge(command, first, opts \\ []) do
    ProxyEnv.judge(
      command,
      first,
      Keyword.merge(
        [
          decider: decider(0.95, "proxy_env", 0.4),
          probe: fn _u, _t -> :reachable end,
          proxy_url: @proxy_url
        ],
        opts
      )
    )
  end

  # Overrides for one test; the baseline is restored by `on_exit`, so a failed
  # assertion cannot leak config into the next test.
  defp with_config(overrides) do
    on_exit(fn -> Application.put_env(:exhub, ProxyEnv, @test_baseline) end)
    Application.put_env(:exhub, ProxyEnv, Keyword.merge(@test_baseline, overrides))
  end

  # --- Gate 1: is it even a network command? -------------------------------

  describe "network_fetch?/1" do
    test "recognises clients that reach out over HTTP(S)" do
      assert ProxyEnv.network_fetch?("curl https://storage.googleapis.com/gvisor/runsc")
      assert ProxyEnv.network_fetch?("wget -qO- http://example.com/x")
      assert ProxyEnv.network_fetch?("git clone https://gitee.com/oscstudio/skyline.git")
      assert ProxyEnv.network_fetch?("go install golang.org/x/tools/gopls@latest")
      assert ProxyEnv.network_fetch?("mix deps.get")
      assert ProxyEnv.network_fetch?("docker pull hub.gitee.com/library/nginx:1.22.0")
    end

    test "passes over commands with no outbound request in them" do
      refute ProxyEnv.network_fetch?("ls -la")
      refute ProxyEnv.network_fetch?("echo hello")
      refute ProxyEnv.network_fetch?("kill -0 4242")
      refute ProxyEnv.network_fetch?(nil)
    end

    test "a URL inside quotes is text, not a request" do
      refute ProxyEnv.network_fetch?("echo 'see https://example.com'")
      refute ProxyEnv.network_fetch?("echo \"curl /tmp/x\"")
    end
  end

  # --- Gate 2: did it fail the way a blocked route fails? ------------------

  describe "proxy_failure?/1" do
    test "matches transport-shaped errors, including the vault's signatures" do
      assert ProxyEnv.proxy_failure?("HTTP/1.1 000")
      assert ProxyEnv.proxy_failure?(~s(exit_code: 0\nstdout: "HTTP 000\n"))
      assert ProxyEnv.proxy_failure?("LibreSSL SSL_connect: SSL_ERROR_SYSCALL in connection")
      assert ProxyEnv.proxy_failure?("curl: (6) Could not resolve host: hub.gitee.com")
      assert ProxyEnv.proxy_failure?("dial tcp: lookup hub.gitee.com: no such host")
      assert ProxyEnv.proxy_failure?("connect: connection refused")
    end

    test "ignores failures that are not transport-shaped" do
      refute ProxyEnv.proxy_failure?("404 Not Found")
      refute ProxyEnv.proxy_failure?("permission denied")
      refute ProxyEnv.proxy_failure?("")
      refute ProxyEnv.proxy_failure?(nil)
    end
  end

  describe "retry_candidate?/2" do
    test "a command that reported success is never re-run" do
      refute ProxyEnv.retry_candidate?(
               "curl https://x.dev",
               {:ok, %{"exit_code" => 0, "stderr" => "HTTP 000"}}
             )
    end

    test "a fetch that failed at the transport layer is a candidate" do
      assert ProxyEnv.retry_candidate?("curl https://x.dev", failing("curl https://x.dev"))
    end

    test "a non-fetch failing with a transport-shaped error is not a candidate" do
      refute ProxyEnv.retry_candidate?(
               "ls -la",
               {:ok, %{"exit_code" => 2, "stderr" => "connection refused"}}
             )
    end

    test "a fetch that failed for an unrelated reason is not a candidate" do
      refute ProxyEnv.retry_candidate?(
               "curl https://x.dev",
               failing("curl https://x.dev", exit_code: 22, stderr: "404 Not Found")
             )
    end

    test "a timed-out fetch with transport-shaped partial output is a candidate" do
      assert ProxyEnv.retry_candidate?(
               "curl https://x.dev",
               {:timeout, %{"stdout" => "", "stderr" => "i/o timeout"}}
             )
    end

    test "an error tag is not a candidate" do
      refute ProxyEnv.retry_candidate?("curl https://x.dev", {:error, :nodedown})
    end
  end

  # --- Hard guards: no model call, no env change ---------------------------

  describe "judge/3 guards" do
    test "disabled by config means no model call and no probe" do
      with_config(enabled: false)
      parent = self()

      assert {:noop, :disabled, nil} =
               ProxyEnv.judge("curl https://x.dev", failing("curl https://x.dev"),
                 probe: fn _u, _t ->
                   send(parent, :probed)
                   :reachable
                 end
               )

      refute_received :probed
    end

    test "not enabled means nothing is judged" do
      assert {:noop, :disabled, nil} =
               ProxyEnv.judge("curl https://x.dev", failing("curl https://x.dev"), enabled: false)
    end

    test "a credential in the URL stops the decision before the model" do
      parent = self()
      command = "git clone https://oauth2:***@gitee.com/oscstudio/skyline.git"

      assert {:noop, :credentials_in_url, nil} =
               judge(command, failing(command),
                 decider: fn _s, _q, _o ->
                   send(parent, :called)
                   {:ok, answers(0.99, "proxy_env", 0.0)}
                 end
               )

      refute_received :called
    end

    test "a credential in an auth header stops it too" do
      command = "wget --header 'Authorization: Bearer ***' https://x.dev/file"
      assert {:noop, :credentials_in_url, nil} = judge(command, failing(command))
    end

    test "a token query parameter stops it" do
      command = "curl https://x.dev/report?access_token=***"
      assert {:noop, :credentials_in_url, nil} = judge(command, failing(command))
    end

    test "an already-exported proxy is left alone" do
      command = "curl https://x.dev"

      assert {:noop, :proxy_already_set, nil} =
               judge(command, failing(command), env: [{"HTTPS_PROXY", "http://127.0.0.1:7890"}])
    end

    test "a proxy candidate that does not connect is never injected" do
      command = "curl https://x.dev"

      assert {:noop, :no_proxy_candidate, nil} =
               judge(command, failing(command),
                 proxy_url: @dead_proxy,
                 probe: fn _u, _t -> :unreachable end
               )
    end

    test "a failure that is not transport-shaped is not judged" do
      assert {:noop, :not_a_network_failure, nil} =
               judge(
                 "curl https://x.dev",
                 failing("curl https://x.dev", exit_code: 22, stderr: "404 Not Found")
               )
    end

    test "a non-fetch command is not judged" do
      assert {:noop, :not_a_fetch, nil} =
               judge("ls -la", {:ok, %{"exit_code" => 2, "stderr" => "connection refused"}})
    end

    test "a blank command is not judged" do
      assert {:noop, :blank_command, nil} = judge("   ", failing("   "))
    end

    test "a model error fails closed" do
      command = "curl https://x.dev"

      assert {:noop, :decide_failed, nil} =
               judge(command, failing(command), decider: fn _s, _q, _o -> {:error, :timeout} end)
    end

    test "a crashing model fails closed" do
      command = "curl https://x.dev"

      assert {:noop, :decide_failed, nil} =
               judge(command, failing(command), decider: fn _s, _q, _o -> raise "boom" end)
    end
  end

  # --- Verdict -------------------------------------------------------------

  describe "judge/3 verdict" do
    test "injects when the model asks for proxy env" do
      command = "curl https://storage.googleapis.com/gvisor/releases/runsc"

      assert {:inject, additions, detail} = judge(command, failing(command))
      assert detail["proxy_url"] == @proxy_url
      assert detail["needs_proxy"] == 0.95
      assert {"HTTPS_PROXY", @proxy_url} in additions
      assert {"https_proxy", @proxy_url} in additions
      assert {"NO_PROXY", _} = List.keyfind(additions, "NO_PROXY", 0)
    end

    test "a low-confidence verdict abstains and changes nothing" do
      command = "curl https://x.dev"

      assert {:noop, :low_confidence, detail} =
               judge(command, failing(command), decider: decider(0.35, "proxy_env", 0.4))

      assert detail["needs_proxy"] == 0.35
    end

    test "the abstention bar is configurable" do
      command = "curl https://x.dev"

      assert {:inject, _, _} =
               judge(command, failing(command),
                 decider: decider(0.35, "proxy_env", 0.4),
                 min_confidence: 0.3
               )
    end

    test "another mechanism is not applied: a rewrite is only advice" do
      command = "curl https://github.com/a/b/releases/download/v1/x.tar.gz"

      assert {:noop, :other_mechanism, detail} =
               judge(command, failing(command), decider: decider(0.99, "url_rewrite", 0.1))

      assert detail["mechanism"] == "url_rewrite"
    end

    test "the fix is the tool's own config, not the environment" do
      command = "apt-get install -y docker-ce"

      assert {:noop, :other_mechanism, _} =
               judge(command, failing(command, stderr: "W: Failed to fetch"),
                 decider: decider(0.9, "tool_own_config", 0.2)
               )
    end

    test "a severe credential-leak risk blocks injection even when a proxy is needed" do
      command = "curl https://x.dev"

      assert {:noop, :leak_risk, _} =
               judge(command, failing(command), decider: decider(0.95, "proxy_env", 3.0))
    end

    test "the leak bar is configurable" do
      command = "curl https://x.dev"

      assert {:inject, _, _} =
               judge(command, failing(command),
                 decider: decider(0.95, "proxy_env", 3.0),
                 max_leak_risk: 3.5
               )
    end
  end

  # --- Evidence given to the model ----------------------------------------

  describe "evidence" do
    test "the model is told only observed facts, and asked three questions" do
      parent = self()
      command = "curl https://storage.googleapis.com/gvisor/releases/runsc"

      assert {:inject, _, _} =
               judge(command, failing(command),
                 decider: fn state, questions, _o ->
                   send(parent, {:state, state, questions})
                   {:ok, answers(0.9, "proxy_env", 0.1)}
                 end,
                 env: [{"PATH", "/usr/bin"}]
               )

      assert_received {:state, state, questions}
      assert state =~ "Command: " <> command
      assert state =~ "Candidate proxy: #{@proxy_url}"
      assert state =~ "Proxy variables already exported: none"
      assert state =~ "Exit code: 7"
      assert state =~ "curl: (7) Failed to connect"

      assert Map.keys(questions) |> MapSet.new() ==
               MapSet.new(["needs_proxy", "mechanism", "leak_risk"])
    end

    test "credentials are masked before the prompt leaves this process" do
      parent = self()
      command = "curl https://dl.example.com/pkg.tar.gz"

      assert {:inject, _, _} =
               judge(
                 command,
                 {:ok,
                  %{
                    "exit_code" => 7,
                    "stderr" => "curl: (7) to https://pusher:***@dl.example.com",
                    "command" => command
                  }},
                 decider: fn state, _q, _o ->
                   send(parent, {:state, state})
                   {:ok, answers(0.9, "proxy_env", 0.1)}
                 end
               )

      assert_received {:state, state}
      refute state =~ "hunter2"
      assert state =~ "***:***@"
    end

    test "evidence is truncated so the prompt fits the 8K default model" do
      parent = self()
      command = "curl https://dl.example.com/pkg.tar.gz"
      huge = String.duplicate("x", 40_000)

      judge(
        command,
        {:ok, %{"exit_code" => 7, "stderr" => "curl: (7) Failed " <> huge, "command" => command}},
        decider: fn state, _q, _o ->
          send(parent, {:state, state})
          {:ok, answers(0.9, "proxy_env", 0.1)}
        end
      )

      assert_received {:state, state}
      assert byte_size(state) < 4_000
    end
  end

  # --- Setup ---------------------------------------------------------------

  describe "build_env/3" do
    test "NO_PROXY merges existing value, config, and loopback defaults" do
      additions =
        ProxyEnv.build_env(@proxy_url, [{"NO_PROXY", ".runjs.cn,api.gitee.com"}],
          no_proxy: ["ai.gitee.com"]
        )

      assert {"NO_PROXY", no_proxy} = List.keyfind(additions, "NO_PROXY", 0)
      assert no_proxy =~ ".runjs.cn"
      assert no_proxy =~ "api.gitee.com"
      assert no_proxy =~ "ai.gitee.com"
      assert no_proxy =~ "localhost"
    end

    test "NO_PROXY is de-duplicated case-insensitively" do
      first = ProxyEnv.no_proxy_value([{"no_proxy", "LocalHost,localhost,::1"}], no_proxy: [])
      assert first == "localhost,127.0.0.1,::1"
    end

    test "both cases of every variable are set" do
      keys = ProxyEnv.build_env(@proxy_url) |> Enum.map(fn {k, _} -> k end)
      assert Enum.all?(["HTTPS_PROXY", "HTTP_PROXY", "ALL_PROXY"], &(&1 in keys))
      assert Enum.all?(["https_proxy", "http_proxy", "all_proxy"], &(&1 in keys))
    end
  end

  describe "apply_to_env/2" do
    test "replaces case variants so a stale value cannot shadow the injection" do
      base = [{"PATH", "/usr/bin"}, {"https_proxy", "http://stale:1"}, {"NO_PROXY", "old"}]

      additions = [
        {"HTTPS_PROXY", @proxy_url},
        {~s(https_proxy), @proxy_url},
        {"NO_PROXY", "new"}
      ]

      merged = ProxyEnv.apply_to_env(base, additions)

      assert {"PATH", "/usr/bin"} in merged
      assert {"HTTPS_PROXY", @proxy_url} in merged
      assert {"https_proxy", @proxy_url} in merged
      assert {"NO_PROXY", "new"} in merged
      refute {"NO_PROXY", "old"} in merged
      refute Enum.any?(merged, fn {k, v} -> k == "https_proxy" and v == "http://stale:1" end)
    end

    test "non-proxy variables are untouched" do
      merged = ProxyEnv.apply_to_env([{"HOME", "/root"}], ProxyEnv.build_env(@proxy_url))
      assert {"HOME", "/root"} in merged
    end
  end

  describe "cached_additions/1" do
    test "returns nothing without a verdict" do
      assert ProxyEnv.cached_additions("curl https://never-ran.example") == []
    end

    test "reuses an injected verdict for the same command" do
      with_config(enabled: true)
      command = "curl https://storage.googleapis.com/gvisor/releases/runsc"
      assert {:inject, additions, _} = judge(command, failing(command), cache: true)
      assert ProxyEnv.cached_additions(command) == additions
    end

    test "does not reuse an abstention" do
      with_config(enabled: true)
      command = "curl https://x.dev"

      assert {:noop, _, _} =
               judge(command, failing(command),
                 decider: decider(0.2, "proxy_env", 0.1),
                 cache: true
               )

      assert ProxyEnv.cached_additions(command) == []
    end

    test "is empty while the module is disabled" do
      with_config(enabled: true)
      command = "curl https://x.dev"
      {:inject, _, _} = judge(command, failing(command), cache: true)
      with_config(enabled: false)
      assert ProxyEnv.cached_additions(command) == []
    end
  end

  # --- Pure answer interpretation -----------------------------------------

  describe "interpret/2" do
    test "accepts a whole result map" do
      assert {:inject, additions, _} =
               ProxyEnv.interpret(answers(0.9, "proxy_env", 0.1), proxy_url: @proxy_url)

      assert {"HTTPS_PROXY", @proxy_url} in additions
    end

    test "a missing answer abstains" do
      assert {:noop, :no_verdict, _} =
               ProxyEnv.interpret(%{"answers" => %{}}, proxy_url: @proxy_url)

      assert {:noop, :no_verdict, nil} = ProxyEnv.interpret(:garbage, [])
    end

    test "reads a probability out of the yes/no map when `noul` is absent" do
      answers = %{
        "needs_proxy" => %{"probabilities" => %{"yes" => 0.93}},
        "mechanism" => %{"choice" => "proxy_env"},
        "leak_risk" => %{"score" => 0.5}
      }

      assert {:inject, _, detail} = ProxyEnv.interpret(answers, proxy_url: @proxy_url)
      assert detail["needs_proxy"] == 0.93
    end

    test "without a proxy candidate there is nothing to inject" do
      assert {:noop, :no_proxy_candidate, _} =
               ProxyEnv.interpret(answers(0.9, "proxy_env", 0.1), [])
    end
  end

  # --- Config --------------------------------------------------------------

  describe "config/0" do
    test "off by default under config/test.exs so the shell tools stay offline" do
      refute ProxyEnv.enabled?()
      assert ProxyEnv.mode() == :on_fail
    end

    test "the abstention and leak bars are conservative" do
      cfg = ProxyEnv.config()
      assert cfg[:min_confidence] == 0.6
      assert cfg[:max_leak_risk] == 2.0
      assert cfg[:probe_timeout_ms] == 50
    end

    test "unknown modes fall back to :on_fail without creating atoms" do
      with_config(mode: "reboot")
      assert ProxyEnv.mode() == :on_fail

      with_config(mode: :pre)
      assert ProxyEnv.mode() == :pre
    end
  end

  # --- Probe ---------------------------------------------------------------

  describe "tcp_reachable?/2" do
    test "a closed port on loopback is unreachable" do
      assert ProxyEnv.tcp_reachable?(@dead_proxy, 50) == :unreachable
    end

    test "an unresolvable host is unreachable" do
      assert ProxyEnv.tcp_reachable?("http://no-such-host.invalid", 50) == :unreachable
    end

    test "a listening port is reachable" do
      {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false, ip: {127, 0, 0, 1}])
      {:ok, port} = :inet.port(listen)

      task =
        Task.async(fn ->
          {:ok, socket} = :gen_tcp.accept(listen, 2_000)
          :gen_tcp.close(socket)
        end)

      assert ProxyEnv.tcp_reachable?("http://127.0.0.1:#{port}", 500) == :reachable
      Task.await(task)
      :gen_tcp.close(listen)
    end

    test "non-string input is unreachable" do
      assert ProxyEnv.tcp_reachable?(nil, 50) == :unreachable
    end
  end

  describe "proxy_vars/0" do
    test "documents the variables this module may inject" do
      assert "HTTPS_PROXY" in ProxyEnv.proxy_vars()
      assert "NO_PROXY" in ProxyEnv.proxy_vars()
    end
  end

  # --- Evidence: measured network facts ----------------------------------

  describe "target_host/1" do
    test "takes host and port from an http(s) URL" do
      assert ProxyEnv.target_host("curl -sS https://www.google.com/search") ==
               {"www.google.com", 443}

      assert ProxyEnv.target_host("wget http://files.example.org/a.tar.gz") ==
               {"files.example.org", 80}

      assert ProxyEnv.target_host("curl http://127.0.0.1:8080/health") == {"127.0.0.1", 8080}
    end

    test "ignores userinfo and URL punctuation glued to the host" do
      assert ProxyEnv.target_host(~s|curl https://user:***@h.example.com/x|) ==
               {"h.example.com", 443}

      assert ProxyEnv.target_host(~s|curl -sS "https://example.com/x"|) == {"example.com", 443}
    end

    test "reads ssh, scp and git-remote hosts as port 22" do
      assert ProxyEnv.target_host("ssh -o StrictHostKeyChecking=no root@10.0.0.5 'uptime'") ==
               {"10.0.0.5", 22}

      assert ProxyEnv.target_host("scp user@jump.example.com:/tmp/x .") ==
               {"jump.example.com", 22}

      assert ProxyEnv.target_host("git clone git@gitee.com:team/repo.git") == {"gitee.com", 22}
    end

    test "returns nil when the command addresses no host" do
      assert ProxyEnv.target_host("npm install left-pad") == nil
      assert ProxyEnv.target_host("ls -la") == nil
      assert ProxyEnv.target_host(nil) == nil
    end
  end

  describe "probe_host/3" do
    test "a listening port is reachable, with the connect time" do
      {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false, ip: {127, 0, 0, 1}])
      {:ok, port} = :inet.port(listen)

      task =
        Task.async(fn ->
          {:ok, socket} = :gen_tcp.accept(listen, 2_000)
          :gen_tcp.close(socket)
        end)

      try do
        assert {:reachable, note} = ProxyEnv.probe_host("127.0.0.1", port, 500)
        assert note =~ "connect "
        assert note =~ " ms)"
      after
        Task.await(task)
        :gen_tcp.close(listen)
      end
    end

    test "a refused port is unreachable, and the reason is named" do
      assert {:unreachable, note} = ProxyEnv.probe_host("127.0.0.1", 1, 300)
      assert note =~ "refused"
    end

    test "invalid arguments are reported, never raised" do
      assert {:unreachable, note} = ProxyEnv.probe_host(nil, 443, 100)
      assert note =~ "invalid probe arguments"
    end
  end

  describe "network premise in the instructions" do
    test "the mainland-China premise is stated to every question by default" do
      parent = self()
      command = "curl https://www.google.com"

      assert {:inject, _, _} =
               judge(
                 command,
                 failing(command, exit_code: 28, stderr: "curl: (28) Connection timed out"),
                 # the production default; the suite baseline disables probing
                 target_probe: true,
                 target_probe_fn: fn _host, _port, _t -> {:unreachable, " (SYN timeout)"} end,
                 decider: fn _state, questions, _o ->
                   send(parent, {:questions, questions})
                   {:ok, answers(0.9, "proxy_env", 0.1)}
                 end
               )

      assert_received {:questions, questions}

      # The two questions whose answer depends on where this machine sits. The
      # leak-risk question is about what leaves the host, so it deliberately
      # carries no regional premise.
      assert questions["needs_proxy"]["instructions"] =~ "mainland China"
      assert questions["mechanism"]["instructions"] =~ "mainland China"
      assert questions["leak_risk"]["instructions"] =~ "personal local proxy"
      assert questions["needs_proxy"]["instructions"] =~ "decisive fact"
      assert questions["mechanism"]["instructions"] =~ "measured direct probe"
    end

    test "the premise is not binding on the proxy inventory" do
      parent = self()
      command = "curl https://www.google.com"

      judge(command, failing(command),
        decider: fn _state, questions, _o ->
          send(parent, {:questions, questions})
          {:ok, answers(0.9, "proxy_env", 0.1)}
        end
      )

      assert_received {:questions, questions}
      assert questions["needs_proxy"]["instructions"] =~ "NOT a rule you must obey"
    end

    test "a host with unrestricted egress can drop the premise" do
      parent = self()
      command = "curl https://www.google.com"
      with_config(network_premise: nil)

      judge(command, failing(command),
        decider: fn _state, questions, _o ->
          send(parent, {:questions, questions})
          {:ok, answers(0.9, "proxy_env", 0.1)}
        end
      )

      assert_received {:questions, questions}
      refute questions["needs_proxy"]["instructions"] =~ "Context premise"
      refute questions["mechanism"]["instructions"] =~ "mainland China"
      assert questions["mechanism"]["instructions"] =~ "no assumed regional restriction"
    end
  end

  describe "target probe in the evidence" do
    test "states the measured reachability of the target host" do
      parent = self()
      command = "curl https://www.google.com/search"

      assert {:inject, _, _} =
               judge(
                 command,
                 failing(command, exit_code: 28, stderr: "curl: (28) Connection timed out"),
                 target_probe: true,
                 target_probe_fn: fn "www.google.com", 443, _timeout ->
                   {:unreachable, " (SYN timeout after 1500 ms)"}
                 end,
                 decider: fn state, _questions, _o ->
                   send(parent, {:state, state})
                   {:ok, answers(0.9, "proxy_env", 0.1)}
                 end
               )

      assert_received {:state, state}

      assert state =~
               "Direct TCP probe of target host, no proxy: www.google.com:443 → unreachable (SYN timeout after 1500 ms)"
    end

    test "a directly reachable target is reported as such" do
      parent = self()
      command = "curl https://www.baidu.com"

      judge(command, failing(command, exit_code: 28, stderr: "curl: (28) Connection timed out"),
        target_probe: true,
        target_probe_fn: fn _host, _port, _timeout -> {:reachable, " (connect 12 ms)"} end,
        decider: fn state, _questions, _o ->
          send(parent, {:state, state})
          {:ok, answers(0.9, "proxy_env", 0.1)}
        end
      )

      assert_received {:state, state}
      assert state =~ "www.baidu.com:443 → reachable (connect 12 ms)"
    end

    test "says so when no target host can be identified" do
      parent = self()
      command = "go mod download github.com/x/y"

      judge(command, failing(command, stderr: "dial tcp: lookup proxy.golang.org: no such host"),
        target_probe: true,
        decider: fn state, _questions, _o ->
          send(parent, {:state, state})
          {:ok, answers(0.9, "proxy_env", 0.1)}
        end
      )

      assert_received {:state, state}
      assert state =~ "no target host detected in the command"
    end

    test "is skipped when target_probe is off" do
      parent = self()
      command = "curl https://www.google.com"

      judge(command, failing(command),
        target_probe: false,
        decider: fn state, questions, _o ->
          send(parent, {:state, state, questions})
          {:ok, answers(0.9, "proxy_env", 0.1)}
        end
      )

      assert_received {:state, state, questions}
      assert state =~ "not probed (target_probe disabled)"
      refute questions["needs_proxy"]["instructions"] =~ "decisive fact"
    end

    test "one probe per host is cached across decisions" do
      parent = self()
      command = "curl https://example.com/a"

      opts = [
        target_probe: true,
        target_probe_fn: fn _host, _port, _timeout ->
          send(parent, :probed)
          {:reachable, " (connect 5 ms)"}
        end,
        cache: true,
        decider: decider(0.9, "proxy_env", 0.1)
      ]

      assert {:inject, _, _} = judge(command, failing(command), opts)
      assert {:inject, _, _} = judge(command, failing(command), opts)

      assert_received :probed
      refute_received :probed
    end
  end

  describe "advisory network notes" do
    test "the operator's proxy inventory is shown as reference, not as a rule" do
      parent = self()
      command = "curl https://www.google.com"
      with_config(network_notes: "Clash listens on 127.0.0.1:7890; CI proxy hj.runjs.cn:31443")

      judge(command, failing(command),
        decider: fn state, _questions, _o ->
          send(parent, {:state, state})
          {:ok, answers(0.9, "proxy_env", 0.1)}
        end
      )

      assert_received {:state, state}
      assert state =~ "Advisory host network notes (reference only, not a rule): Clash listens"
      assert state =~ "hj.runjs.cn:31443"
    end

    test "no notes line when none are configured" do
      parent = self()
      command = "curl https://www.google.com"

      judge(command, failing(command),
        decider: fn state, _questions, _o ->
          send(parent, {:state, state})
          {:ok, answers(0.9, "proxy_env", 0.1)}
        end
      )

      assert_received {:state, state}
      refute state =~ "Advisory host network notes"
    end
  end

  # --- Tool wiring ---------------------------------------------------------

  describe "ExecuteCommand integration" do
    setup do
      {:ok, _} = Application.ensure_all_started(:exile)
      :ok
    end

    # A closed loopback port: a real transport failure (`Failed to connect`),
    # resolved offline.
    @dead_fetch ~s(curl --connect-timeout 1 -sS http://127.0.0.1:1/ )

    defp run_tool(command) do
      {:reply, resp, %{}} =
        ExecuteCommand.execute(%{command: command, timeout_ms: 5_000, working_dir: @tmp_dir}, %{})

      resp
    end

    defp tool_text(resp), do: Enum.map_join(resp.content, "\n", & &1["text"])

    test "leaves the result untouched while disabled" do
      resp = run_tool(@dead_fetch)
      text = tool_text(resp)
      assert text =~ "exit_code"
      refute text =~ "proxy_env"
    end

    test "does not annotate when no proxy candidate can connect" do
      with_config(enabled: true, proxy_url: @dead_proxy, fallback_proxies: [])
      resp = run_tool(@dead_fetch)
      refute tool_text(resp) =~ "proxy_env"
    end

    test "a successful command is reported once, never re-run" do
      with_config(enabled: true, proxy_url: @proxy_url, fallback_proxies: [])
      resp = run_tool("echo ok")
      text = tool_text(resp)
      assert text =~ "ok"
      refute text =~ "attempt"
    end

    test "credentials in the command line are never routed through a proxy" do
      with_config(enabled: true, proxy_url: @proxy_url, fallback_proxies: [])
      resp = run_tool(~s(curl --connect-timeout 1 -sS https://user:***@127.0.0.1:1/ ))
      refute tool_text(resp) =~ "proxy_env"
    end
  end
end
