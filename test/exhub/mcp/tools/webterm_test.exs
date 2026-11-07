defmodule Exhub.MCP.Tools.WebTermTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.WebTerm

  describe "insert_js/1" do
    test "embeds the payload as base64 and keeps the JS quote-free" do
      js = WebTerm.insert_js(Base.encode64("echo \"hi there\" | awk '{print $1}'"))

      assert js =~ "atob('"
      assert js =~ ".xterm-helper-textarea"
      # The whole point: no double quote anywhere, so the chrome `type`/`eval`
      # DSL cannot mis-parse it.
      refute js =~ "\""
    end

    test "round-trips the payload" do
      payload = "ls -la /data/docker-image && echo done"
      js = WebTerm.insert_js(Base.encode64(payload))
      [_, b64] = Regex.run(~r/atob\('([^']+)'\)/, js)

      assert Base.decode64!(b64) == payload
    end
  end

  describe "chunk/2" do
    test "splits on character boundaries" do
      assert WebTerm.chunk("abcdef", 2) == ["ab", "cd", "ef"]
      assert WebTerm.chunk("abcde", 2) == ["ab", "cd", "e"]
    end

    test "keeps multibyte characters intact" do
      assert WebTerm.chunk("攻击分析主机", 2) == ["攻击", "分析", "主机"]
    end

    test "falls back to a single chunk for a bad size" do
      assert WebTerm.chunk("abc", 0) == ["abc"]
      assert WebTerm.chunk("abc", nil) == ["abc"]
    end
  end

  describe "find_tab/2" do
    @listing """
    ✓ tab list
      931804176 "AI 队友仪表盘" — https://gitee.com/ai-teammates/dashboard
      931804396 "连接资产 - JumpServer 开源堡垒机" — https://jmsv5.autom.studio/ui/
    * 931804463 "攻击分析主机" — https://jmsv5.autom.studio/luna/session/abc
    """

    test "matches by title substring" do
      assert WebTerm.find_tab(@listing, "攻击分析主机") == "931804463"
    end

    test "matches by id" do
      assert WebTerm.find_tab(@listing, "931804396") == "931804396"
    end

    test "returns nil when nothing matches" do
      assert WebTerm.find_tab(@listing, "nope") == nil
      assert WebTerm.find_tab(@listing, "") == nil
    end
  end

  describe "wrap_command/1" do
    test "appends a printf that puts the exit code on its own line" do
      assert WebTerm.wrap_command("ls") == "ls; printf '\\nWTDONE %s\\n' $?"
    end

    test "trims trailing whitespace before appending" do
      assert WebTerm.wrap_command("ls   \n") == "ls; printf '\\nWTDONE %s\\n' $?"
    end
  end

  describe "extract_result/1" do
    test "returns the exit code and the text before the marker" do
      screen = "file.txt\nWTDONE 0\nroot@host:~#"

      assert {:ok, 0, output} = WebTerm.extract_result(screen)
      assert output == "file.txt"
    end

    test "parses non-zero exit codes" do
      assert {:ok, 127, _} = WebTerm.extract_result("bash: nope: not found\nWTDONE 127")
    end

    test "is pending while the command is still running" do
      assert WebTerm.extract_result("still running...\n") == :pending
    end

    test "never matches the echoed command (echo-proof marker)" do
      echo = "root@host:~# ls; printf '\\nWTDONE %s\\n' $?\n"
      assert WebTerm.extract_result(echo) == :pending
    end

    test "ignores the marker when it is not alone on its line" do
      assert WebTerm.extract_result("WTDONE 0 something else") == :pending
    end
  end

  describe "clean_output/2" do
    @command "cat /data/docker-image/list.txt"

    test "drops the echoed command line and surrounding blank lines" do
      screen = "\nroot@host:/# cat /data/docker-image/list.txt\ncopycat.tgz\nums-admin.tar\n\n"

      assert WebTerm.clean_output(screen, @command) == "copycat.tgz\nums-admin.tar"
    end

    test "drops a wrapped echo when at least the first line matches" do
      screen = "root@host:/# cat /data/docker-image/list.txt\ncopycat.tgz"
      assert WebTerm.clean_output(screen, @command) == "copycat.tgz"
    end

    test "leaves output untouched when the echo is not present" do
      assert WebTerm.clean_output("a\nb", @command) == "a\nb"
    end

    test "does not try to strip very short commands" do
      assert WebTerm.clean_output("root@h:~# ls\nfile", "ls") == "root@h:~# ls\nfile"
    end
  end

  describe "eval_result/1" do
    test "takes the last line and strips the JSON quotes" do
      reply = "✓ eval (function(){ ... 'no-input' ... })\n\"ok:27\""
      assert WebTerm.eval_result(reply) == "ok:27"
    end

    test "reports the error sentinels" do
      assert WebTerm.eval_result("✓ eval (...)\n\"no-input\"") == "no-input"
      assert WebTerm.eval_result("✓ eval (...)\n\"fail:0\"") == "fail:0"
    end

    test "tolerates an empty reply" do
      assert WebTerm.eval_result("") == ""
      assert WebTerm.eval_result(nil) == ""
    end
  end

  describe "format_run/3" do
    test "renders exit code, poll count and output" do
      text = WebTerm.format_run(0, "hello\n", 2)

      assert text =~ "exit_code: 0"
      assert text =~ "polls: 2"
      assert text =~ "--- output ---\nhello"
    end
  end
end
