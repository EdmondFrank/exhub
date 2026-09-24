defmodule Exhub.MCP.Tools.Desktop.WorkingDirTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Desktop.WorkingDir

  setup do
    WorkingDir.clear_cache()
    :ok
  end

  defp noul_result(probability) do
    %{"answers" => %{"needs_working_dir" => %{"type" => "noul", "noul" => probability}}}
  end

  defp noul(probability), do: {:ok, noul_result(probability)}

  describe "needs_working_dir?/1 anchored fast path" do
    test "absolute paths and cd never need a working_dir, without asking the model" do
      parent = self()

      decider = fn _state, _questions, _opts ->
        send(parent, :decider_called)
        noul(0.9)
      end

      refute WorkingDir.needs_working_dir?("cd /some/path", decider: decider)
      refute WorkingDir.needs_working_dir?("cd ~/projects", decider: decider)
      refute WorkingDir.needs_working_dir?("foo && cd /some/path", decider: decider)
      refute WorkingDir.needs_working_dir?("cat /etc/passwd", decider: decider)
      refute WorkingDir.needs_working_dir?("echo hi > /tmp/out.txt", decider: decider)
      refute WorkingDir.needs_working_dir?("~/bin/script arg1", decider: decider)

      refute_received :decider_called
    end

    test "a quoted path does not make a command anchored" do
      decider = fn _state, _questions, _opts -> noul(0.9) end

      # `echo "ls /tmp"` only prints text; it is not anchored by the path in the
      # quotes, so the model is consulted.
      assert WorkingDir.needs_working_dir?(~s(echo "ls /tmp"), decider: decider)
    end

    test "a relative cd is not anchored, so the model decides" do
      decider = fn _s, _q, _o -> noul(0.99) end

      assert WorkingDir.needs_working_dir?("cd build && make", decider: decider)
      assert WorkingDir.needs_working_dir?("cd .. && ls", decider: decider)
      assert WorkingDir.needs_working_dir?("cd sub/dir; ls", decider: decider)
    end

    test "a bare cd or an absolute/~ cd target is anchored" do
      decider = fn _s, _q, _o -> noul(0.99) end

      refute WorkingDir.needs_working_dir?("cd", decider: decider)
      refute WorkingDir.needs_working_dir?("cd && ls", decider: decider)
      refute WorkingDir.needs_working_dir?("cd /tmp", decider: decider)
      refute WorkingDir.needs_working_dir?("cd ~/src", decider: decider)
    end
  end

  describe "needs_working_dir?/1 Smart Decide" do
    test "requires a working_dir when the model says the command depends on cwd" do
      decider = fn _state, _questions, _opts -> noul(0.9) end
      assert WorkingDir.needs_working_dir?("git status", decider: decider)
    end

    test "does not require a working_dir when the model says it is independent" do
      decider = fn _state, _questions, _opts -> noul(0.05) end
      refute WorkingDir.needs_working_dir?("curl https://example.com", decider: decider)
    end

    test "honours the threshold" do
      decider = fn _state, _questions, _opts -> noul(0.4) end

      refute WorkingDir.needs_working_dir?("git status", decider: decider, threshold: 0.5)
      assert WorkingDir.needs_working_dir?("git status", decider: decider, threshold: 0.3)
    end

    test "passes the command as state and asks a noul question" do
      parent = self()

      decider = fn state, questions, _opts ->
        send(parent, {:call, state, questions})
        noul(0.9)
      end

      WorkingDir.needs_working_dir?("git status", decider: decider)

      assert_received {:call, "git status", questions}

      assert %{"needs_working_dir" => %{"type" => "noul", "instructions" => instructions}} =
               questions

      assert is_binary(instructions)
    end

    test "the instruction names downloads/clones as cwd-dependent" do
      parent = self()

      decider = fn _s, questions, _o ->
        send(parent, {:q, questions})
        noul(0.9)
      end

      WorkingDir.needs_working_dir?("wget https://example.com/f.gz", decider: decider)

      assert_received {:q, %{"needs_working_dir" => %{"instructions" => i}}}
      assert i =~ "wget"
      assert i =~ "git clone"
      assert i =~ "without writing files"
    end
  end

  describe "needs_working_dir?/1 fallback" do
    test "falls back to the pure heuristic when the model errors" do
      decider = fn _state, _questions, _opts -> {:error, "boom"} end

      # Unanchored and cwd-sensitive: the heuristic requires a working_dir.
      assert WorkingDir.needs_working_dir?("git status", decider: decider)

      # Anchored commands never reach the model.
      refute WorkingDir.needs_working_dir?("cat /etc/passwd", decider: decider)
    end

    test "uses the pure heuristic when disabled" do
      assert WorkingDir.needs_working_dir?("git status", enabled: false)
      refute WorkingDir.needs_working_dir?("cat /etc/passwd", enabled: false)
    end

    test "fails closed on an unparsable answer" do
      decider = fn _state, _questions, _opts -> {:ok, %{}} end
      assert WorkingDir.needs_working_dir?("git status", decider: decider)
    end

    test "fails closed on a blank command" do
      assert WorkingDir.needs_working_dir?("")
      assert WorkingDir.needs_working_dir?("   ")
    end
  end

  describe "needs_working_dir_result?/2" do
    test "reads the noul probability" do
      refute WorkingDir.needs_working_dir_result?(noul_result(0.1), 0.5)
      assert WorkingDir.needs_working_dir_result?(noul_result(0.9), 0.5)
    end

    test "fails closed on unparsable results" do
      assert WorkingDir.needs_working_dir_result?(%{}, 0.5)
      assert WorkingDir.needs_working_dir_result?(%{"answers" => %{}}, 0.5)
    end
  end

  describe "cache" do
    test "does not call the decider twice for the same command" do
      parent = self()

      decider = fn _state, _questions, _opts ->
        send(parent, :called)
        noul(0.9)
      end

      assert WorkingDir.needs_working_dir?("make build", decider: decider, cache: true)
      assert WorkingDir.needs_working_dir?("make build", decider: decider, cache: true)

      assert_received :called
      refute_received :called
    end
  end
end
