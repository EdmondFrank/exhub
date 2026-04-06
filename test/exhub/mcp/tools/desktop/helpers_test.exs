defmodule Exhub.MCP.Tools.Desktop.HelpersTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Desktop.Helpers

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
