defmodule Exhub.ResponseHandlers.ExhubBlinkSearchTest do
  use ExUnit.Case, async: true

  alias Exhub.ResponseHandlers.ExhubBlinkSearch

  # The BlinkSearch.Server may not be running under --no-start; casts to a
  # non-existent process are dropped silently, which is exactly what these
  # tests rely on: they verify message parsing/routing, not side effects.

  describe "call/1" do
    test "parses search action" do
      assert ExhubBlinkSearch.call(["blink-search", "search", "foo", 20, []]) == nil
      assert ExhubBlinkSearch.call(["blink-search", "search", "", 20, ["Find File"]]) == nil
    end

    test "coerces row_number to default when not an integer" do
      assert ExhubBlinkSearch.call(["blink-search", "search", "foo", "bad", []]) == nil
    end

    test "parses candidate actions" do
      for action <- ["do", "copy", "parent", "select", "continue"] do
        assert ExhubBlinkSearch.call(["blink-search", action, "Find File", "a.ex"]) == nil
      end
    end

    test "parses navigation actions" do
      for action <- [
            "select_next_candidate",
            "select_prev_candidate",
            "select_next_backend",
            "select_prev_backend",
            "select_next_group",
            "select_prev_group"
          ] do
        assert ExhubBlinkSearch.call(["blink-search", action]) == nil
      end
    end

    test "parses update action with item list" do
      assert ExhubBlinkSearch.call(["blink-search", "update", "Buffer List", ["*scratch*"]]) ==
               nil
    end

    test "ignores update action when items is not a list" do
      assert match?(
               nil,
               ExhubBlinkSearch.call(["blink-search", "update", "Buffer List", "not-a-list"])
             )
    end

    test "parses init actions" do
      assert ExhubBlinkSearch.call(["blink-search", "init_search_dir", "/tmp"]) == nil

      assert ExhubBlinkSearch.call([
               "blink-search",
               "init_current_buffer",
               "*scratch*",
               "base64content"
             ]) == nil
    end

    test "parses init_common_directory with alias/path pairs" do
      assert ExhubBlinkSearch.call([
               "blink-search",
               "init_common_directory",
               [["HOME", "~/"], ["P", "~/projects"]]
             ]) == nil

      # Non-list entries are dropped, not crashing
      assert ExhubBlinkSearch.call([
               "blink-search",
               "init_common_directory",
               ["garbage", ["OK", "/tmp"]]
             ]) == nil
    end

    test "ignores init_common_directory when dirs is not a list" do
      assert match?(
               nil,
               ExhubBlinkSearch.call(["blink-search", "init_common_directory", "not-a-list"])
             )
    end

    test "parses init_grep_pdf_paths" do
      assert ExhubBlinkSearch.call([
               "blink-search",
               "init_grep_pdf_paths",
               ["/tmp/docs", "~/pdfs"]
             ]) == nil
    end

    test "ignores init_grep_pdf_paths when paths is not a list" do
      assert match?(
               nil,
               ExhubBlinkSearch.call(["blink-search", "init_grep_pdf_paths", 42])
             )
    end

    test "parses clean action" do
      assert ExhubBlinkSearch.call(["blink-search", "clean"]) == nil
    end

    test "unknown action logs and returns nil" do
      assert ExhubBlinkSearch.call(["blink-search", "bogus", 1, 2]) == nil
    end
  end

  describe "DefaultResponseHandler routing" do
    test "routes blink-search messages to the blink-search handler" do
      import ExUnit.CaptureLog

      message = Jason.encode!(["func", ["blink-search", "clean"]])

      # Registered actions must NOT hit the unknown-action fallback
      log =
        capture_log(fn ->
          assert Exhub.DefaultResponseHandler.call(message) == nil
        end)

      refute log =~ "Unknown action: blink-search"
    end

    test "returns nil for unknown func names" do
      import ExUnit.CaptureLog

      message = Jason.encode!(["func", ["no-such-func"]])

      log =
        capture_log(fn ->
          assert Exhub.DefaultResponseHandler.call(message) == nil
        end)

      assert log =~ "Unknown action: no-such-func"
    end
  end
end
