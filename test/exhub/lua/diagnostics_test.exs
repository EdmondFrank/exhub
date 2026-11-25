defmodule Exhub.Lua.DiagnosticsTest do
  use ExUnit.Case, async: true

  alias Exhub.Lua.Diagnostics

  defp classify(code) do
    assert {:error, error} = Lua.parse_chunk(code)
    Diagnostics.classify(error)
  end

  # The engine compiles through `Lua.eval!/3`, not `parse_chunk/1`, and the two
  # report failures differently — this is the path a real call takes.
  defp classify_eval(code) do
    lua =
      Exhub.Lua.Bridge.build(
        [],
        fn _server, _tool, _args -> {:ok, nil} end,
        Exhub.Lua.Engine.config()
      )

    try do
      Lua.eval!(lua, code, source: "code_mode")
      flunk("expected #{inspect(code)} to fail compilation")
    rescue
      error -> Diagnostics.classify(error, code)
    end
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

  describe "located diagnostics" do
    test "a colon key is reported at its own line, not where the parser gave up" do
      code =
        "local r = parallel({\n  {server = \"desktop\", args = {path: \"/x\"}},\n})\nreturn r"

      message = classify_eval(code)

      assert message =~ "SyntaxError: line 2"
      assert message =~ "table keys use '=', not ':'"
      assert message =~ "parser:"
    end

    test "a positionless parser failure still gets its hint" do
      message =
        Diagnostics.classify(
          %Lua.CompilerException{errors: ["Unclosed long string"]},
          "local s = [[\nx"
        )

      assert message =~ "hint:"
      assert message =~ "[==["
    end

    test "a [[ literal that closed early names its closing line" do
      message = classify_eval("local s = [[\nassert p[\"x\"] == []]]\nreturn s")

      assert message =~ "closes at line 2"
      assert message =~ "`[[` at line 1"
    end

    test "a backtick string is named as such" do
      message = classify_eval("local s = `hi`\nreturn s")

      assert message =~ "backticks are not Lua string delimiters"
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

    test "a call on a result field is named as such, not as a missing tool" do
      message =
        Diagnostics.classify(%Lua.RuntimeException{
          original: "attempt to call a nil value (global 'content')"
        })

      assert message =~ "UnknownTool: 'content'"
      assert message =~ "result field, not a tool"
      assert message =~ "`r.content[1].text`"
      refute message =~ "is not available in this sandbox"
    end

    test "the same hint applies to an indexed field" do
      message =
        Diagnostics.classify(%Lua.RuntimeException{
          original: "attempt to index a nil value (field 'text')"
        })

      assert message =~ "UnknownTool: 'text'"
      assert message =~ "result field"
    end
  end
end
