defmodule Exhub.MCP.Tools.Desktop.SecretGuardTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Desktop.SecretGuard

  # All data here is synthetic — no real envless/vault values are read.
  @vault "/tmp/exhub_guard_test_vault"

  setup do
    SecretGuard.clear_cache()
    :ok
  end

  defp decider(probability) do
    fn _state, _questions, _opts ->
      {:ok, %{"answers" => %{"secret_risk" => %{"noul" => probability}}}}
    end
  end

  defp counting_decider(probability) do
    {:ok, agent} = Agent.start_link(fn -> 0 end)

    decider = fn state, questions, opts ->
      Agent.update(agent, &(&1 + 1))
      decider(probability).(state, questions, opts)
    end

    {decider, agent}
  end

  describe "envless CLI (deterministic fast path)" do
    test "refuses the value-printing subcommands" do
      for command <- [
            "envless get OPENAI_API_KEY",
            "ENVLESS_ROOT=~/GTD envless get COTP_PASS --confirm",
            "envless show prd_token",
            "envless export SENTRY_DSN"
          ] do
        assert {:refuse, message} =
                 SecretGuard.check(
                   "execute_command",
                   %{"command" => command},
                   enabled: false,
                   vault_roots: [@vault]
                 ),
               "expected #{inspect(command)} to be refused"

        assert message =~ "execute_command"
      end
    end

    test "refuses `envless exec` that dumps the injected environment" do
      for command <- [
            "envless exec -- env",
            "envless exec -- printenv",
            "ENVLESS_ROOT=~/GTD envless exec -- set"
          ] do
        assert {:refuse, _} =
                 SecretGuard.check("execute_command", %{"command" => command}, enabled: false)
      end
    end

    test "allows the value-free `envless list` and a child that consumes secrets" do
      for command <- [
            "ENVLESS_ROOT=~/GTD envless list",
            "envless list --env=prod",
            "envless exec -- node server.js"
          ] do
        assert :ok =
                 SecretGuard.check("execute_command", %{"command" => command}, enabled: false),
               "expected #{inspect(command)} to be allowed"
      end
    end
  end

  describe "environment dumps (deterministic fast path)" do
    test "refuses a bare dump or a credential-shaped variable" do
      for command <- ["env", "printenv", "printenv COTP_PASS", "env | grep SECRET"] do
        assert {:refuse, _} =
                 SecretGuard.check("execute_command", %{"command" => command}, enabled: false),
               "expected #{inspect(command)} to be refused"
      end
    end

    test "allows an env assignment that merely prefixes a command" do
      for command <- ["env FOO=1 ruby script.rb", "env PATH=/usr/bin:/bin mix test"] do
        assert :ok = SecretGuard.check("execute_command", %{"command" => command}, enabled: false)
      end
    end
  end

  describe "secret-manager commands (deterministic fast path)" do
    test "refuses keychain / secret-manager reads" do
      for command <- [
            "security find-generic-password -s my-login -w",
            "op read op://vault/item/credential",
            "op item get github --fields password",
            "pass show github/token",
            "gh auth token",
            "sops -d secrets.enc.yaml",
            "aws secretsmanager get-secret-value --secret-id prod/db",
            "kubectl get secret db -o jsonpath='{.data.password}'"
          ] do
        assert {:refuse, _} =
                 SecretGuard.check("execute_command", %{"command" => command}, enabled: false),
               "expected #{inspect(command)} to be refused"
      end
    end
  end

  describe "secret paths (deterministic fast path)" do
    test "refuses .env / private-key / credential files" do
      for path <- [
            "/Users/me/project/.env",
            "/Users/me/project/.env.production",
            "/Users/me/.ssh/id_rsa",
            "/Users/me/.aws/credentials",
            "/etc/ssl/private/server.pem",
            "/Users/me/.netrc"
          ] do
        assert SecretGuard.secret_path?(path, vault_roots: [@vault]),
               "expected #{inspect(path)} to be a secret path"
      end
    end

    test "refuses the envless store under the vault root but not plain notes" do
      assert SecretGuard.secret_path?("#{@vault}/.envless/identity.key", vault_roots: [@vault])
      assert SecretGuard.secret_path?("#{@vault}/secrets/dev.env.enc", vault_roots: [@vault])
      assert SecretGuard.secret_path?("#{@vault}/MANIFEST.json", vault_roots: [@vault])

      refute SecretGuard.secret_path?("#{@vault}/PKB/note.md", vault_roots: [@vault])
      refute SecretGuard.secret_path?(@vault, vault_roots: [@vault])
    end

    test "refuses production / release configuration files" do
      for path <- [
            "config/runtime.exs",
            "/app/config/prod.exs",
            "/app/_build/prod/rel/exhub/releases/0.1.0/sys.config",
            "/app/_build/prod/rel/exhub/releases/0.1.0/vm.args",
            "/app/_build/prod/rel/exhub/releases/0.1.0/env.sh"
          ] do
        assert SecretGuard.secret_path?(path), "expected #{inspect(path)} to be a secret path"
      end
    end

    test "allows plain source, docs, notes and template env files" do
      for path <- [
            "/Users/me/project/src/main.ex",
            "/Users/me/project/README.md",
            "/Users/me/notes/todo.md",
            "/Users/me/project/.env.example",
            "/Users/me/config/config.exs"
          ] do
        refute SecretGuard.secret_path?(path, vault_roots: [@vault]),
               "expected #{inspect(path)} to be allowed"
      end
    end

    test "reads_secret_file only fires on a path-like secret token" do
      assert SecretGuard.secret_command?("cat /Users/me/project/.env", vault_roots: [@vault])
      assert SecretGuard.secret_command?("grep -n TOKEN ~/.ssh/id_rsa", vault_roots: [@vault])
      # A bare word "secret" is not a path, so a scoped grep is allowed.
      refute SecretGuard.secret_command?("grep secret /Users/me/project/main.ex",
               vault_roots: [@vault]
             )
    end

    test "guards the path-based tools" do
      for tool <- ["read_file", "search_files"] do
        assert {:refuse, _} =
                 SecretGuard.check(tool, %{"path" => "/Users/me/.env"}, enabled: false)
      end

      assert {:refuse, _} =
               SecretGuard.check(
                 "read_multiple_files",
                 %{"paths" => ["/Users/me/ok.md", "/Users/me/.ssh/id_rsa"]},
                 enabled: false
               )

      assert :ok =
               SecretGuard.check(
                 "read_multiple_files",
                 %{"paths" => ["/Users/me/ok.md", "/Users/me/other.md"]},
                 enabled: false
               )
    end
  end

  describe "vault roots" do
    test "honours an explicit `:vault_roots`" do
      assert SecretGuard.vault_roots(vault_roots: ["/tmp/a", "~/b"]) == [
               "/tmp/a",
               Path.expand("~/b")
             ]
    end

    test "defaults to $ENVLESS_ROOT or ~/GTD" do
      roots = SecretGuard.vault_roots()
      assert is_list(roots) and roots != []
      assert Enum.all?(roots, &(Path.type(&1) == :absolute))
    end
  end

  describe "Smart Decide layer" do
    test "refuses an ambiguous call the model judges secret-risky" do
      assert {:refuse, _} =
               SecretGuard.check(
                 "read_file",
                 %{"path" => "/Users/me/notes.md"},
                 decider: decider(0.9)
               )
    end

    test "allows a call the model judges safe" do
      assert :ok =
               SecretGuard.check(
                 "read_file",
                 %{"path" => "/Users/me/notes.md"},
                 decider: decider(0.1)
               )
    end

    test "respects the threshold" do
      opts = [decider: decider(0.6)]

      assert {:refuse, _} =
               SecretGuard.check("read_file", %{"path" => "/Users/me/notes.md"}, opts)

      assert :ok =
               SecretGuard.check(
                 "read_file",
                 %{"path" => "/Users/me/notes.md"},
                 Keyword.put(opts, :threshold, 0.9)
               )
    end

    test "fails open when the model errors" do
      decider = fn _, _, _ -> {:error, :boom} end

      assert :ok =
               SecretGuard.check("read_file", %{"path" => "/Users/me/notes.md"}, decider: decider)
    end

    test "fails open on an unparsable answer" do
      decider = fn _, _, _ -> {:ok, %{"answers" => "nope"}} end

      assert :ok =
               SecretGuard.check("read_file", %{"path" => "/Users/me/notes.md"}, decider: decider)
    end

    test "enabled: false skips the model but keeps the fast path" do
      assert :ok =
               SecretGuard.check("read_file", %{"path" => "/Users/me/notes.md"}, enabled: false)

      assert {:refuse, _} =
               SecretGuard.check("read_file", %{"path" => "/Users/me/.env"}, enabled: false)
    end

    test "defaults to the SemIf-OpenJev-4B model (task-specific pick)" do
      assert Keyword.get(SecretGuard.config(), :model) == "SemIf-OpenJev-4B"
      assert Keyword.get(SecretGuard.config(), :threshold) == 0.3
    end

    test "passes the configured model to the decider, and honours an override" do
      {:ok, agent} = Agent.start_link(fn -> nil end)

      decider = fn _state, _questions, opts ->
        Agent.update(agent, fn _ -> Keyword.get(opts, :model) end)
        {:ok, %{"answers" => %{"secret_risk" => %{"noul" => 0.1}}}}
      end

      assert :ok = SecretGuard.check("read_file", %{"path" => "/Users/me/n.md"}, decider: decider)
      assert Agent.get(agent, & &1) == "SemIf-OpenJev-4B"

      assert :ok =
               SecretGuard.check("read_file", %{"path" => "/Users/me/n.md"},
                 decider: decider,
                 model: "laya-multilingual"
               )

      assert Agent.get(agent, & &1) == "laya-multilingual"
    end
  end

  describe "guards" do
    test "non-guarded tools are never judged" do
      assert :ok =
               SecretGuard.check("delete_file", %{"path" => "/Users/me/.env"},
                 decider: decider(0.99)
               )
    end

    test "always_allow bypasses the gate" do
      assert :ok =
               SecretGuard.check("read_file", %{"path" => "/Users/me/.env"},
                 always_allow: ["read_file"]
               )
    end

    test "a nil / non-binary tool fails open" do
      assert :ok = SecretGuard.check(nil, %{"path" => "/Users/me/.env"})
    end
  end

  describe "cache" do
    test "caches the verdict per tool and arguments" do
      {decider, agent} = counting_decider(0.9)
      opts = [decider: decider, cache: true]

      assert {:refuse, _} = SecretGuard.check("read_file", %{"path" => "/Users/me/a.md"}, opts)
      assert {:refuse, _} = SecretGuard.check("read_file", %{"path" => "/Users/me/a.md"}, opts)
      assert Agent.get(agent, & &1) == 1

      assert {:refuse, _} = SecretGuard.check("read_file", %{"path" => "/Users/me/b.md"}, opts)
      assert Agent.get(agent, & &1) == 2
    end
  end

  describe "supervision" do
    test "start_link creates the verdict cache table" do
      name = :"secret_guard_test_#{System.unique_integer([:positive])}"
      {:ok, pid} = SecretGuard.start_link(name: name)
      assert :ets.whereis(:exhub_secret_guard_cache) != :undefined
      GenServer.stop(pid)
    end
  end

  describe "check_request/2" do
    test "extracts the tool name and arguments from a tools/call request" do
      request = %{
        "method" => "tools/call",
        "params" => %{"name" => "read_file", "arguments" => %{"path" => "/Users/me/.env"}}
      }

      assert {:refuse, _} = SecretGuard.check_request(request)
    end

    test "allows anything that is not a well-formed tools/call" do
      assert :ok = SecretGuard.check_request(%{"method" => "tools/list"})
      assert :ok = SecretGuard.check_request(%{"params" => %{}})
      assert :ok = SecretGuard.check_request(nil)
    end
  end
end
