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

    test "decodes the base64 as UTF-8 so non-ASCII commands survive" do
      js = WebTerm.insert_js(Base.encode64("printf '攻击分析主机 OK'"))

      # `atob` yields a Latin-1 code point per byte; without an explicit UTF-8
      # decode the terminal receives double-encoded bytes.
      assert js =~ "new TextDecoder('utf-8')"
      assert js =~ "u[i]=b.charCodeAt(i)"
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

    test "tolerates the OCR misreads seen on live screens" do
      # deepseek-v4.1-flash dropped the T on a mid-paint frame
      assert {:ok, 0, _} = WebTerm.extract_result("BETA\nWDONE 0\nroot@host:~#")
      # and swapped a 0 for a capital O
      assert {:ok, 0, _} = WebTerm.extract_result("root\nWTDONE O\nroot@host:~#")
      assert {:ok, 0, _} = WebTerm.extract_result("WTD0NE 0")
      assert {:ok, -1, _} = WebTerm.extract_result("WTDONF -1")
    end

    test "still rejects unrelated words and bad exit codes" do
      assert WebTerm.extract_result("WARNING 0") == :pending
      assert WebTerm.extract_result("WTDONE nope") == :pending
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

  describe "format_run/4" do
    test "reports which readback channel produced the output" do
      assert WebTerm.format_run(0, "hi", 1, "buffer") =~ "source: buffer"
      assert WebTerm.format_run(0, "hi", 1, "ocr") =~ "source: ocr"
    end
  end

  describe "terminal_js/1" do
    test "is quote-free so the chrome eval DSL cannot mis-parse it" do
      for opts <- [[], [tail: 6], [from: 3, to: 9]] do
        refute WebTerm.terminal_js(opts) =~ "\""
      end
    end

    test "defaults to a tail read and honours explicit bounds" do
      assert WebTerm.terminal_js(tail: 6) =~ "Math.max(0,n-6)"
      assert WebTerm.terminal_js(from: 10, to: 20) =~ "for(var p=10;p<end;p++)"
      assert WebTerm.terminal_js(from: 10, to: 20) =~ "Math.min(n,20)+1"
    end

    test "cursor mode reads a window around the cursor row" do
      js = WebTerm.terminal_js(cursor: true)

      assert js =~ "Math.max(0,cy-8)"
      assert js =~ "cy+1"
      assert js =~ "cy=b.baseY+b.cursorY"
    end

    test "walks the Vue app for the live Terminal instance" do
      js = WebTerm.terminal_js()

      assert js =~ "__vue_app__"
      assert js =~ "buffer.active.getLine"
      assert js =~ "isWrapped"
    end
  end

  describe "decode_payload/1" do
    test "decodes the double-encoded chrome reply" do
      reply = "✓ eval \"...\"\n" <> Jason.encode!(Jason.encode!(%{"found" => true, "len" => 42}))

      assert {:ok, %{"found" => true, "len" => 42}} = WebTerm.decode_payload(reply)
    end

    test "accepts a single-encoded reply" do
      assert {:ok, %{"found" => false}} =
               WebTerm.decode_payload(Jason.encode!(%{"found" => false}))
    end

    test "reports extension truncation instead of returning half a payload" do
      reply =
        "line\n… cut at 10000 of 62671 characters: --max all reads all of it, or return less"

      assert {:error, message} = WebTerm.decode_payload(reply)
      assert message =~ "truncated"
    end

    test "reports an empty reply" do
      assert {:error, _} = WebTerm.decode_payload("")
      assert {:error, _} = WebTerm.decode_payload(nil)
    end
  end

  describe "marker_index/2" do
    @nonce "ABC123"

    test "finds the marker and its absolute buffer index" do
      lines = [
        [10, "root@h:~# ls", 0],
        [11, "file.txt", 0],
        [12, "WTDONEABC123 0", 0],
        [13, "root@h:~# ", 0]
      ]

      assert {:ok, 0, 12} = WebTerm.marker_index(lines, @nonce)
    end

    test "ignores markers left by an earlier run" do
      lines = [[5, "WTDONEZZZ999 0", 0], [7, "WTDONEABC123 7", 0]]

      assert {:ok, 7, 7} = WebTerm.marker_index(lines, @nonce)
    end

    test "takes the last marker when several are visible" do
      lines = [[2, "WTDONEABC123 3", 0], [8, "WTDONEABC123 0", 0]]

      assert {:ok, 0, 8} = WebTerm.marker_index(lines, @nonce)
    end

    test "never matches the echoed command or a partial line" do
      lines = [
        [1, "root@h:~# printf '\\nWTDONEABC123 %s\\n' $?", 0],
        [2, "WTDONEABC123 0 something", 0],
        [3, "WARNING 0", 0]
      ]

      assert :pending = WebTerm.marker_index(lines, @nonce)
    end

    test "parses negative exit codes" do
      assert {:ok, -1, 2} = WebTerm.marker_index([[2, "WTDONEABC123 -1", 0]], @nonce)
    end

    test "is pending while the command is still running" do
      assert :pending = WebTerm.marker_index([[2, "still running", 0]], @nonce)
      assert :pending = WebTerm.marker_index([], @nonce)
    end
  end

  describe "strip_echo/2" do
    test "drops the echoed command and keeps the output" do
      lines = [
        [1, "root@h:~# seq 1 3; printf '\\nWTDONEABC123 %s\\n' $?", 0],
        [2, "1", 0],
        [3, "2", 0],
        [4, "3", 0]
      ]

      assert WebTerm.strip_echo(lines, "ABC123") == ["1", "2", "3"]
    end

    test "finds an echo that xterm wrapped across visual lines" do
      lines = [
        [1, "root@h:~# echo aaaa", 0],
        [2, "bbbb; printf '\\nWTDONEABC123 %s\\n' $?", 1],
        [3, "aaaabbbb", 0]
      ]

      assert WebTerm.strip_echo(lines, "ABC123") == ["aaaabbbb"]
    end

    test "returns everything when the echo scrolled out of the window" do
      lines = [[1, "1", 0], [2, "2", 0]]

      assert WebTerm.strip_echo(lines, "ABC123") == ["1", "2"]
    end
  end

  describe "wrap_command/2" do
    test "embeds the nonce in the marker and keeps the literal %s in the echo" do
      assert WebTerm.wrap_command("ls", "ABC123") == "ls; printf '\\nWTDONEABC123 %s\\n' $?"
    end
  end

  describe "join_wrapped/1" do
    test "merges wrapped continuations into one logical line" do
      lines = [[1, "aaaaaaaaaa", 0], [2, "bbbb", 1], [3, "cccc", 1], [4, "next", 0]]

      assert WebTerm.join_wrapped(lines) == ["aaaaaaaaaabbbbcccc", "next"]
    end

    test "tolerates empty input" do
      assert WebTerm.join_wrapped([]) == []
    end
  end
end
