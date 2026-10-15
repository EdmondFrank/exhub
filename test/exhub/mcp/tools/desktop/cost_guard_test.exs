defmodule Exhub.MCP.Tools.Desktop.CostGuardTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Desktop.CostGuard

  setup do
    CostGuard.clear_cache()
    :ok
  end

  defp decider(probability) do
    fn _state, _questions, _opts ->
      {:ok, %{"answers" => %{"expensive" => %{"noul" => probability}}}}
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

  describe "deterministic fast path (no model call)" do
    test "refuses a search_files rooted at the home directory or disk" do
      home = System.user_home!()

      # `enabled: false` proves no model call is needed — the fast path refuses.
      assert {:refuse, message} =
               CostGuard.check("search_files", %{"path" => "~"}, enabled: false)

      assert message =~ "search_files"
      assert message =~ "~"

      assert {:refuse, _} = CostGuard.check("search_files", %{"path" => "/"}, enabled: false)
      assert {:refuse, _} = CostGuard.check("search_files", %{"path" => "/Users"}, enabled: false)
      assert {:refuse, _} = CostGuard.check("search_files", %{"path" => home}, enabled: false)
    end

    test "allows a search_files scoped to a project directory" do
      assert :ok = CostGuard.check("search_files", %{"path" => "~/projects/repo"}, enabled: false)
      assert :ok = CostGuard.check("search_files", %{"path" => "/code/thing"}, enabled: false)
    end

    test "list_directory only refuses a broad root once it recurses" do
      assert {:refuse, _} =
               CostGuard.check("list_directory", %{"path" => "~", "depth" => 3}, enabled: false)

      assert :ok =
               CostGuard.check("list_directory", %{"path" => "~", "depth" => 0}, enabled: false)

      assert :ok =
               CostGuard.check("list_directory", %{"path" => "~", "depth" => 1}, enabled: false)

      assert :ok =
               CostGuard.check("list_directory", %{"path" => "~/code", "depth" => 5},
                 enabled: false
               )
    end

    test "refuses a shell command that scans the whole home or disk" do
      for command <- [
            "find ~ -name '*.log'",
            "find / -type f",
            "grep -r secret /Users",
            "rg foo /",
            "du -sh ~",
            "mdfind kind:pdf",
            "locate passwd"
          ] do
        assert {:refuse, _} =
                 CostGuard.check("execute_command", %{"command" => command}, enabled: false),
               "expected #{inspect(command)} to be refused"
      end
    end

    test "allows a shell command scoped to a tree" do
      for command <- [
            "find /code/thing -name '*.ex'",
            "grep -r foo ~/projects/repo",
            "grep foo /Users/me/file.txt",
            "cat /etc/hosts",
            "ls -la ~/code",
            "du -sh ~/projects/repo",
            "git status"
          ] do
        assert :ok = CostGuard.check("execute_command", %{"command" => command}, enabled: false),
               "expected #{inspect(command)} to be allowed"
      end
    end

    test "start_process is gated like execute_command" do
      assert {:refuse, _} =
               CostGuard.check("start_process", %{"command" => "rg foo /"}, enabled: false)
    end

    test "broad_path? and broad_command? expose the pure heuristics" do
      assert CostGuard.broad_path?("~")
      assert CostGuard.broad_path?("/")
      assert CostGuard.broad_path?(System.user_home!())
      refute CostGuard.broad_path?("~/projects/repo")
      refute CostGuard.broad_path?("/code")
      refute CostGuard.broad_path?(nil)

      assert CostGuard.broad_command?("find ~ -type f")
      assert CostGuard.broad_command?("grep -R foo /Users")
      refute CostGuard.broad_command?("find /code -name x")
      refute CostGuard.broad_command?("cat /etc/hosts")
      refute CostGuard.broad_command?(nil)
    end
  end

  describe "Smart Decide layer" do
    test "refuses a scoped but huge call the model judges expensive" do
      assert {:refuse, _} =
               CostGuard.check("search_files", %{"path" => "~/data"}, decider: decider(0.9))
    end

    test "allows a call the model judges cheap" do
      assert :ok = CostGuard.check("search_files", %{"path" => "~/data"}, decider: decider(0.1))
    end

    test "respects the threshold" do
      opts = [decider: decider(0.6)]
      assert {:refuse, _} = CostGuard.check("search_files", %{"path" => "~/data"}, opts)

      assert :ok =
               CostGuard.check(
                 "search_files",
                 %{"path" => "~/data"},
                 Keyword.put(opts, :threshold, 0.9)
               )
    end

    test "fails open when the model errors" do
      decider = fn _, _, _ -> {:error, :boom} end
      assert :ok = CostGuard.check("search_files", %{"path" => "~/data"}, decider: decider)
    end

    test "fails open on an unparsable answer" do
      decider = fn _, _, _ -> {:ok, %{"answers" => "nope"}} end
      assert :ok = CostGuard.check("search_files", %{"path" => "~/data"}, decider: decider)
    end

    test "enabled: false skips the model but keeps the fast path" do
      assert :ok = CostGuard.check("search_files", %{"path" => "~/data"}, enabled: false)
      assert {:refuse, _} = CostGuard.check("search_files", %{"path" => "~"}, enabled: false)
    end

    test "defaults to the APUS-OpenJev-v1-9B model" do
      assert Keyword.get(CostGuard.config(), :model) == "APUS-OpenJev-v1-9B"
    end

    test "passes the configured model to the decider, and honours an override" do
      {:ok, agent} = Agent.start_link(fn -> nil end)

      decider = fn _state, _questions, opts ->
        Agent.update(agent, fn _ -> Keyword.get(opts, :model) end)
        {:ok, %{"answers" => %{"expensive" => %{"noul" => 0.1}}}}
      end

      assert :ok = CostGuard.check("search_files", %{"path" => "~/data"}, decider: decider)
      assert Agent.get(agent, & &1) == "APUS-OpenJev-v1-9B"

      assert :ok =
               CostGuard.check("search_files", %{"path" => "~/data"},
                 decider: decider,
                 model: "laya-multilingual"
               )

      assert Agent.get(agent, & &1) == "laya-multilingual"
    end
  end

  describe "guards" do
    test "non-guarded tools are never judged" do
      assert :ok = CostGuard.check("read_file", %{"path" => "~"}, decider: decider(0.99))
    end

    test "always_allow bypasses the gate" do
      assert :ok =
               CostGuard.check("search_files", %{"path" => "~"}, always_allow: ["search_files"])
    end

    test "a nil / non-binary tool fails open" do
      assert :ok = CostGuard.check(nil, %{"path" => "~"})
    end
  end

  describe "cache" do
    test "caches the verdict per tool and arguments" do
      {decider, agent} = counting_decider(0.9)
      opts = [decider: decider, cache: true]

      assert {:refuse, _} = CostGuard.check("search_files", %{"path" => "~/data"}, opts)
      assert {:refuse, _} = CostGuard.check("search_files", %{"path" => "~/data"}, opts)
      assert Agent.get(agent, & &1) == 1

      # A different argument set is a different cache key.
      assert {:refuse, _} = CostGuard.check("search_files", %{"path" => "~/other"}, opts)
      assert Agent.get(agent, & &1) == 2
    end
  end

  describe "supervision" do
    test "start_link creates the verdict cache table" do
      name = :"cost_guard_test_#{System.unique_integer([:positive])}"
      {:ok, pid} = CostGuard.start_link(name: name)
      assert :ets.whereis(:exhub_cost_guard_cache) != :undefined
      GenServer.stop(pid)
    end
  end

  describe "check_request/2" do
    test "extracts the tool name and arguments from a tools/call request" do
      request = %{
        "method" => "tools/call",
        "params" => %{"name" => "search_files", "arguments" => %{"path" => "~"}}
      }

      assert {:refuse, _} = CostGuard.check_request(request)
    end

    test "allows anything that is not a well-formed tools/call" do
      assert :ok = CostGuard.check_request(%{"method" => "tools/list"})
      assert :ok = CostGuard.check_request(%{"params" => %{}})
      assert :ok = CostGuard.check_request(nil)
    end
  end
end
