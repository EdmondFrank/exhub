defmodule Exhub.MCP.Tools.Desktop.HelpersTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Desktop.Helpers

  describe "needs_working_dir?/1" do
    test "returns false for commands starting with cd" do
      refute Helpers.needs_working_dir?("cd /some/path")
      refute Helpers.needs_working_dir?("cd ~/projects")
    end

    test "returns false for commands containing cd" do
      refute Helpers.needs_working_dir?("foo && cd /some/path")
      refute Helpers.needs_working_dir?("foo ; cd /some/path")
    end

    test "returns false for commands starting with ls" do
      refute Helpers.needs_working_dir?("ls /some/path")
      refute Helpers.needs_working_dir?("ls ~/projects")
    end

    test "returns false for commands containing ls" do
      refute Helpers.needs_working_dir?("foo && ls /some/path")
    end

    test "returns false for commands with absolute paths in arguments" do
      refute Helpers.needs_working_dir?(
               "rtk grep -n 'pattern' /Users/edmondfrank/Code/skyline/utils/time.go"
             )

      refute Helpers.needs_working_dir?("cat /etc/passwd")
      refute Helpers.needs_working_dir?("echo hello > /tmp/output.txt")
      refute Helpers.needs_working_dir?("~/bin/script arg1 arg2")
    end

    test "returns false for commands starting with absolute path" do
      refute Helpers.needs_working_dir?("/usr/bin/python script.py")
      refute Helpers.needs_working_dir?("~/bin/my-script")
    end

    test "returns true for commands without absolute paths or cd/ls" do
      assert Helpers.needs_working_dir?("echo hello")
      assert Helpers.needs_working_dir?("python script.py")
      assert Helpers.needs_working_dir?("make build")
      assert Helpers.needs_working_dir?("git status")
    end

    test "returns true for relative paths" do
      assert Helpers.needs_working_dir?("cat relative/path/file.txt")
      assert Helpers.needs_working_dir?("echo hello > output.txt")
    end

    test "returns true for a cd into a relative directory" do
      assert Helpers.needs_working_dir?("cd build && make")
      assert Helpers.needs_working_dir?("cd .. && ls")
      assert Helpers.needs_working_dir?("cd sub/dir; ls")
      assert Helpers.needs_working_dir?("cd -")
    end

    test "returns false for an absolute/~ cd target or a bare cd" do
      refute Helpers.needs_working_dir?("cd /tmp")
      refute Helpers.needs_working_dir?("cd ~/src")
      refute Helpers.needs_working_dir?("cd")
      refute Helpers.needs_working_dir?("cd;")
      refute Helpers.needs_working_dir?("cd && ls")
      refute Helpers.needs_working_dir?("(cd /tmp && make)")
    end

    test "quoted cd/path text does not anchor, so it fails closed" do
      assert Helpers.needs_working_dir?(~s(git commit -m "cd fix"))
      assert Helpers.needs_working_dir?("echo 'a | cd /x'")
      assert Helpers.needs_working_dir?(~s(echo "ls /tmp"))
    end

    test "a trailing cd argument or a quoted relative target does not anchor" do
      assert Helpers.needs_working_dir?("grep foo cd")
      assert Helpers.needs_working_dir?("cat cd")
      assert Helpers.needs_working_dir?(~s(cd "build"))
    end
  end

  describe "anchored?/1" do
    test "is true for absolute/~ paths and an absolute or bare cd" do
      assert Helpers.anchored?("cat /etc/hosts")
      assert Helpers.anchored?("~/bin/x")
      assert Helpers.anchored?("cd /tmp")
      assert Helpers.anchored?("cd")
      assert Helpers.anchored?("cd && ls")
    end

    test "is false for a relative cd or an unanchored command" do
      refute Helpers.anchored?("cd build && make")
      refute Helpers.anchored?("cd ..")
      refute Helpers.anchored?("git status")
      refute Helpers.anchored?("make")
    end

    test "ignores quoted text" do
      refute Helpers.anchored?(~s(git commit -m "cd fix"))
      refute Helpers.anchored?("echo 'a | cd /x'")
    end

    test "only counts a cd that is a command, not a trailing argument" do
      refute Helpers.anchored?("grep foo cd")
      refute Helpers.anchored?("cat cd")
      refute Helpers.anchored?(~s(cd "build"))
      assert Helpers.anchored?("ls; cd /tmp")
    end
  end

  describe "expand_path/1" do
    test "expand_path(nil) returns nil" do
      assert Helpers.expand_path(nil) == nil
    end

    test "expand_path(~) returns System.user_home!()" do
      assert Helpers.expand_path("~") == System.user_home!()
    end

    test "expand_path(~/foo/bar) returns Path.join(System.user_home!(), \"foo/bar\")" do
      expected = Path.join(System.user_home!(), "foo/bar")
      assert Helpers.expand_path("~/foo/bar") == expected
    end

    test "expand_path(/absolute/path) returns the path unchanged" do
      assert Helpers.expand_path("/absolute/path") == "/absolute/path"
    end

    test "expand_path(relative/path) returns the path unchanged" do
      assert Helpers.expand_path("relative/path") == "relative/path"
    end
  end
end
