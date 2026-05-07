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
