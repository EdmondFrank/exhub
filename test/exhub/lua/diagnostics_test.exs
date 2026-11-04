defmodule Exhub.Lua.DiagnosticsTest do
  use ExUnit.Case, async: true

  alias Exhub.Lua.Diagnostics

  defp classify(code) do
    assert {:error, error} = Lua.parse_chunk(code)
    Diagnostics.classify(error)
  end

  describe "SyntaxError" do
    test "a JSON-style table key gets a hint naming the mistake" do
      message = classify(~s|return desktop.execute_command({command:cmd, working_dir="/x"})|)

      assert message =~ "SyntaxError: Expected '(', string, or table after method name"
      assert message =~ "hint:"
      assert message =~ "{path = \"/x\"}"
      assert message =~ "method call"
      # The parser's inspected wrapper ("Parse error: \"…\"") is noise to the caller.
      refute message =~ "Parse error:"
    end

    test "a colon key with a string value gets the same hint" do
      message = classify(~s|return desktop.execute_command({working_dir: "/x"})|)

      assert message =~ "SyntaxError: Expected :identifier, got :string"
      assert message =~ "hint:"
      assert message =~ "{path = "
    end

    test "a positioned error carries the line, source excerpt, caret and suggestion" do
      message = classify("local x = 1\nreturn 2 +\n")

      assert message =~ "SyntaxError: Expected expression (line 2)"
      assert message =~ "2 | return 2 +"
      assert Regex.match?(~r/\|\s+\^/, message)
      assert message =~ "suggestion: Reached end of file unexpectedly."
    end

    test "the caret points at the offending token" do
      message = classify(~s|local t = {"a" = 1}|)

      assert message =~ "Expected ',' or '}' in table (line 1)"
      assert message =~ "1 | local t = {\"a\" = 1}"
      assert message =~ "suggestion:"
    end

    test "a failure without structured diagnostics still classifies" do
      assert Diagnostics.classify(%Lua.CompilerException{errors: ["odd failure"]}) ==
               "SyntaxError: odd failure"
    end

    test "an empty report falls back to a plain message" do
      assert Diagnostics.classify(%Lua.CompilerException{}) ==
               "SyntaxError: code could not be parsed"
    end
  end

  describe "runtime errors" do
    test "other exceptions pass through unchanged" do
      assert Diagnostics.classify(%ArgumentError{message: "boom"}) == "boom"
    end

    test "an unknown tool names the retrieve_tools escape hatch" do
      message =
        Diagnostics.classify(%Lua.RuntimeException{
          original: "attempt to index a nil value (global 'nosuch')"
        })

      assert message =~ "UnknownTool: 'nosuch'"
      assert message =~ "retrieve_tools"
    end
  end
end
