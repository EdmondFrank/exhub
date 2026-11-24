defmodule Exhub.Lua.LintTest do
  use ExUnit.Case, async: true

  alias Exhub.Lua.Lint

  describe "findings/1" do
    test "a JSON-style key in a nested table is located on its own line" do
      code = """
      local r = parallel({
        {server = "desktop", tool = "read_file", args = {path: "/x", offset: 0}},
      })
      return r
      """

      assert [first, second] = Lint.findings(code)
      assert %{kind: :json_key, line: 2, name: "path"} = first
      assert %{kind: :json_key, line: 2, name: "offset"} = second
      assert first.snippet =~ ~s(args = {path: "/x", offset: 0})
      assert first.fix =~ ~s(path = "/x")
    end

    test "a method call is not a table key" do
      assert Lint.findings(~s|local s = "a:b"\nreturn s:sub(1, 1)|) == []
    end

    test "colons in strings, comments and long strings are ignored" do
      code = """
      local s = -- {a: 1} in a comment
      [[ {b: 2} in text ]]
      return s
      """

      assert Lint.findings(code) == []
    end

    test "backticks are found once per string" do
      assert [%{kind: :backtick, line: 1, snippet: "ls -la"}] =
               Lint.findings("local r = desktop.execute_command({command = `ls -la`})")
    end

    test "a backtick inside a string is not reported" do
      assert Lint.findings(~s|local s = "a ` b"\nreturn s|) == []
    end

    test "findings are capped and ordered by source position" do
      code = "local t = {\n a: 1,\n b: 2,\n c: 3,\n d: 4,\n}\nreturn t"

      assert [%{line: 2}, %{line: 3}, %{line: 4}] = Lint.findings(code)
    end
  end

  describe "render/1" do
    test "names the mistake and the replacement" do
      rendered = "local t = {a: 1}" |> Lint.findings() |> Lint.render()

      assert rendered =~ "line 1: table keys use '=', not ':'"
      assert rendered =~ "write `a = 1`"
    end
  end

  describe "repair/1" do
    test "rewrites a key without disturbing the rest of the line" do
      assert {"local t = {a = 1, b = \"x\"}\nreturn t", [_, _]} =
               Lint.repair(~s|local t = {a: 1, b: "x"}\nreturn t|)
    end

    test "rewrites a backtick string as a long bracket" do
      assert {~s|local s = [==[ls -la]==]\nreturn s|, [%{kind: :backtick}]} =
               Lint.repair("local s = `ls -la`\nreturn s")
    end

    test "leaves clean code untouched" do
      code = ~s|return desktop.read_file({path = "/x"})|
      assert {^code, []} = Lint.repair(code)
    end

    test "an unpaired backtick is reported but never rewritten" do
      code = "local s = `oops\nreturn s"
      assert {^code, []} = Lint.repair(code)
      assert [%{kind: :backtick}] = Lint.findings(code)
    end

    test "a body containing the closing delimiter is left alone" do
      code = "local s = `a ]==] b`\nreturn s"
      assert {^code, []} = Lint.repair(code)
    end

    test "the repair note names the line and the transformation" do
      note = Lint.repair_note([%{kind: :json_key, line: 3}])

      assert note =~ "auto-repaired"
      assert note =~ "line 3"
      assert note =~ "`key:` → `key =`"
    end
  end

  describe "long_string_note/1" do
    test "reports where a [[ literal really closes" do
      code = "local s = [[\nassert p[\"x\"] == []]]\nreturn s"

      assert note = Lint.long_string_note(code)
      assert note =~ "`[[` at line 1 closes at line 2"
      assert note =~ "[==["
    end

    test "reports an unterminated literal" do
      assert Lint.long_string_note("local s = [[\nno close") =~ "never closed"
    end

    test "is silent when a stronger delimiter is already used" do
      assert Lint.long_string_note("local s = [==[ a ]] b ]==]\nreturn s") == nil
    end

    test "is silent when there is no long string" do
      assert Lint.long_string_note("return 1") == nil
    end
  end
end
