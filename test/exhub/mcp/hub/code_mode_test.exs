defmodule Exhub.MCP.Hub.CodeModeTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Hub.CodeMode

  @tools [
    %{"server" => "demo", "name" => "echo", "description" => "echo", "inputSchema" => %{}},
    %{"server" => "web-tools", "name" => "fetch", "description" => "", "inputSchema" => %{}}
  ]

  @ok %{"content" => [%{"type" => "text", "text" => "hello"}]}

  @mcp_error %{"content" => [%{"type" => "text", "text" => "nope"}], "isError" => true}

  @big_output ~s|local t = {} for i = 1, 5000 do t[i] = "abcdefghij" end return table.concat(t)|

  defp runner(fun), do: fn server, tool, args -> fun.(server, tool, args) end

  defp echo_runner do
    runner(fn _s, _t, args ->
      Process.sleep(120)
      {:ok, %{"content" => [%{"type" => "text", "text" => args["tag"] || ""}]}}
    end)
  end

  defp spilled_path(text) do
    [_, path] = Regex.run(~r/Full output saved to: (\S+)/, text)
    path
  end

  describe "configuration" do
    test "default timeout aligns with the MCP server request timeout" do
      # The Hub server's `request_timeout` (application.ex / router.ex) is 600s;
      # the sandbox must time out first so it can return a graceful error
      # instead of being hard-killed by ConcurrentToolDispatcher.
      assert CodeMode.config()[:timeout_ms] == 600_000
    end

    test "excludes meta servers and bounds concurrency by default" do
      cfg = CodeMode.config()
      assert cfg[:exclude_servers] == ["mcp-hub"]
      assert cfg[:max_concurrency] == 8
    end
  end

  describe "pure Lua evaluation" do
    test "returns a scalar" do
      assert {:ok, "4"} = CodeMode.run("return 2 + 2", [])
    end

    test "reports when nothing is returned" do
      assert {:ok, text} = CodeMode.run("local x = 1", [])
      assert text =~ "no return value"
    end

    test "encodes tables as JSON (arrays vs objects)" do
      code = ~s[return {total = 3, names = {"a", "b", "c"}}]
      assert {:ok, json} = CodeMode.run(code, [])
      assert Jason.decode!(json) == %{"total" => 3, "names" => ["a", "b", "c"]}
    end

    test "surfaces syntax errors instead of crashing" do
      assert {:error, message} = CodeMode.run("return 2 +", [])
      assert message != ""
    end
  end

  describe "invalid UTF-8 in results" do
    # Regression: a byte-offset slice (`string.sub`) can end inside a multibyte
    # codepoint. One invalid byte made Jason reject the whole value, and the
    # `inspect/2` fallback then dumped every byte of the affected binary
    # numerically (`<<231, 148, 169, ...>>`) — an unreadable wall of numbers at
    # roughly 4x the size of the text it replaced, which also tripped the output
    # cap. The lua VM rejects non-ASCII *source* literals, so these snippets build
    # their CJK text with `string.char/0` instead.
    @cjk <<0xE7, 0x94, 0xA9>>
    @replacement "\uFFFD"
    @repaired String.duplicate(@cjk, 299) <> @replacement <> @replacement
    @cjk_lua "string.char(0xE7, 0x94, 0xA9)"
    @cut_table "local s = string.rep(" <>
                 @cjk_lua <> ", 300) return {a = s:sub(1, 899), b = \"ascii\"}"
    @cut_string "local s = string.rep(" <> @cjk_lua <> ", 300) return s:sub(1, 899)"
    @cut_print "local s = string.rep(" <>
                 @cjk_lua <> ", 300) print({a = s:sub(1, 899)}); return \"done\""

    test "a table whose value ends mid-codepoint still renders as JSON" do
      assert {:ok, text} = CodeMode.run(@cut_table, [])
      assert String.valid?(text)
      refute text =~ "<<"
      assert Jason.decode!(text) == %{"a" => @repaired, "b" => "ascii"}
    end

    test "a bare string ending mid-codepoint is repaired, not re-encoded" do
      assert {:ok, text} = CodeMode.run(@cut_string, [])
      assert text == @repaired
    end

    test "print output keeps text as text" do
      assert {:ok, text} = CodeMode.run(@cut_print, [])
      assert text =~ "print output:"
      assert text =~ "done"
      refute text =~ "<<"
      assert String.valid?(text)
    end

    test "tool-error text is sanitized before it reaches a lua error message" do
      call =
        runner(fn _s, _t, _a ->
          {:ok,
           %{
             "isError" => true,
             "content" => [%{"type" => "text", "text" => "bad " <> <<0xE7, 0x94>>}]
           }}
        end)

      assert {:error, message} = CodeMode.run("return demo.echo({x = 1})", @tools, call_fun: call)
      assert String.valid?(message)
      refute message =~ "<<"
      assert message =~ "bad " <> @replacement <> @replacement
    end

    test "valid results are unchanged" do
      assert {:ok, json} = CodeMode.run(~s|return {a = "ok", b = {1, 2}}|, [])
      assert Jason.decode!(json) == %{"a" => "ok", "b" => [1, 2]}
    end
  end

  describe "tool bridging" do
    test "calls a tool nested by server and decodes args to a JSON object" do
      parent = self()

      call =
        runner(fn server, tool, args ->
          send(parent, {:called, server, tool, args})
          {:ok, @ok}
        end)

      code = ~s|local r = demo.echo({msg = "hi", nested = {n = 1}}); return r.content[1].text|

      assert {:ok, "hello"} = CodeMode.run(code, @tools, call_fun: call)
      assert_received {:called, "demo", "echo", args}
      assert args == %{"msg" => "hi", "nested" => %{"n" => 1}}
    end

    test "sanitizes hyphenated server names and exposes the flat tools table" do
      call = runner(fn _s, _t, _a -> {:ok, @ok} end)

      assert {:ok, "hello"} =
               CodeMode.run(~s|return web_tools.fetch({url = "x"}).content[1].text|, @tools,
                 call_fun: call
               )

      assert {:ok, "hello"} =
               CodeMode.run(
                 ~s|return tools["web-tools__fetch"]({url = "x"}).content[1].text|,
                 @tools,
                 call_fun: call
               )
    end

    test "passes an empty object when a tool is called without a table arg" do
      parent = self()
      call = runner(fn _s, _t, args -> send(parent, {:args, args}) && {:ok, @ok} end)

      assert {:ok, _} = CodeMode.run("return demo.echo()", @tools, call_fun: call)
      assert_received {:args, %{}}
    end

    test "rejects array-shaped arguments with a clear error" do
      call = runner(fn _s, _t, _a -> {:ok, @ok} end)

      code = """
      local ok, err = pcall(demo.echo, {"a", "b"})
      if ok then return "unexpected" end
      return err
      """

      assert {:ok, error} = CodeMode.run(code, @tools, call_fun: call)
      assert error =~ "must be a table with named keys"
    end

    test "a failing tool raises a catchable Lua error" do
      call = runner(fn _s, _t, _a -> {:error, :boom} end)

      code = """
      local ok, err = pcall(demo.echo, {})
      if ok then return "unexpected" end
      return err
      """

      assert {:ok, error} = CodeMode.run(code, @tools, call_fun: call)
      assert error =~ "demo.echo failed"
      assert error =~ "boom"
    end
  end

  describe "sandbox limits" do
    test "captures print output alongside the return value" do
      assert {:ok, text} = CodeMode.run(~s[print("debug", 42); return "done"], [])
      assert text =~ "print output:"
      assert text =~ "debug\t42"
      assert text =~ "done"
    end

    test "blocks unsafe globals" do
      assert {:error, message} = CodeMode.run(~s[return os.getenv("HOME")], [])
      assert message =~ "sandboxed"
    end

    test "stops a runaway loop on the instruction budget" do
      assert {:error, message} = CodeMode.run("while true do end", [], timeout_ms: 5_000)
      assert message =~ "instruction budget exceeded"
    end

    test "kills an evaluation that blocks in a tool call on the wall-clock timeout" do
      call = runner(fn _s, _t, _a -> Process.sleep(5_000) && {:ok, @ok} end)

      assert {:error, message} =
               CodeMode.run(~s|return demo.echo({})|, @tools, call_fun: call, timeout_ms: 200)

      assert message =~ "timed out"
    end

    test "truncates oversized output and spills the full text to a temp file" do
      assert {:ok, text} = CodeMode.run(@big_output, [], max_output_chars: 100)
      assert String.starts_with?(text, "abcdefghij")
      assert text =~ "(truncated"

      path = spilled_path(text)
      on_exit(fn -> File.rm(path) end)

      assert File.exists?(path)
      full = File.read!(path)
      assert byte_size(full) == 50_000
      assert String.starts_with?(full, "abcdefghij")
    end

    test "does not spill when the output fits within the limit" do
      assert {:ok, text} = CodeMode.run(~s|return "small"|, [], max_output_chars: 100)
      assert text == "small"
      refute text =~ "Full output saved"
    end

    test "spill_truncated: false keeps the plain truncation notice" do
      assert {:ok, text} =
               CodeMode.run(@big_output, [], max_output_chars: 100, spill_truncated: false)

      assert text =~ "(truncated"
      refute text =~ "Full output saved"
    end

    test "successive spills use distinct files" do
      assert {:ok, first} = CodeMode.run(@big_output, [], max_output_chars: 100)
      assert {:ok, second} = CodeMode.run(@big_output, [], max_output_chars: 100)

      first_path = spilled_path(first)
      second_path = spilled_path(second)
      on_exit(fn -> File.rm(first_path) end)
      on_exit(fn -> File.rm(second_path) end)

      refute first_path == second_path
      assert File.exists?(first_path)
      assert File.exists?(second_path)
    end
  end

  describe "MCP-level failure semantics" do
    test "an isError result raises by default and is catchable via pcall" do
      call = runner(fn _s, _t, _a -> {:ok, @mcp_error} end)

      code = """
      local ok, err = pcall(demo.echo, {})
      if ok then return "unexpected" end
      return err
      """

      assert {:ok, error} = CodeMode.run(code, @tools, call_fun: call)
      assert error =~ "demo.echo failed"
      assert error =~ "nope"
    end

    test "raise_on_tool_error: false returns the isError payload as data" do
      call = runner(fn _s, _t, _a -> {:ok, @mcp_error} end)

      code =
        ~s|local r = demo.echo({}); if r.isError then return r.content[1].text end return "no"|

      assert {:ok, "nope"} =
               CodeMode.run(code, @tools, call_fun: call, raise_on_tool_error: false)
    end
  end

  describe "parallel calls" do
    test "returns index-aligned results and runs calls concurrently" do
      call = echo_runner()

      code = """
      local r = parallel({
        {server = "demo", tool = "echo", args = {tag = "a"}},
        {server = "demo", tool = "echo", args = {tag = "b"}},
        {server = "demo", tool = "echo", args = {tag = "c"}},
        {server = "demo", tool = "echo", args = {tag = "d"}}
      })
      return table.concat({
        r[1].content[1].text, r[2].content[1].text,
        r[3].content[1].text, r[4].content[1].text
      }, ",")
      """

      {elapsed_us, result} =
        :timer.tc(fn ->
          CodeMode.run(code, @tools, call_fun: call, timeout_ms: 5_000)
        end)

      assert {:ok, "a,b,c,d"} = result
      # Four sequential 120 ms calls would take >= 480 ms.
      assert elapsed_us < 400_000
    end

    test "parallel raises on the first failing call" do
      call =
        runner(fn
          _s, _t, %{"fail" => true} -> {:error, :boom}
          _s, _t, args -> {:ok, %{"content" => [%{"type" => "text", "text" => args["tag"]}]}}
        end)

      code = """
      local r = parallel({
        {server = "demo", tool = "echo", args = {tag = "ok"}},
        {server = "demo", tool = "echo", args = {fail = true}}
      })
      return "unexpected"
      """

      assert {:error, message} = CodeMode.run(code, @tools, call_fun: call)
      assert message =~ "parallel call 2 failed"
      assert message =~ "boom"
    end

    test "parallel_all returns per-call ok/error without raising" do
      call =
        runner(fn
          _s, _t, %{"fail" => true} -> {:error, :boom}
          _s, _t, args -> {:ok, %{"content" => [%{"type" => "text", "text" => args["tag"]}]}}
        end)

      code = """
      local r = parallel_all({
        {server = "demo", tool = "echo", args = {tag = "ok"}},
        {name = "demo__echo", args = {fail = true}}
      })
      return tostring(r[1].ok) .. ":" .. r[1].result.content[1].text
        .. ":" .. tostring(r[2].ok) .. ":" .. r[2].error
      """

      assert {:ok, text} = CodeMode.run(code, @tools, call_fun: call)
      assert text =~ "true:ok:false"
      assert text =~ "boom"
    end

    test "a server colliding with a reserved global is still reachable via the flat table" do
      tools = [
        %{"server" => "parallel", "name" => "echo", "description" => "", "inputSchema" => %{}}
      ]

      call = runner(fn _s, _t, _a -> {:ok, @ok} end)

      assert {:ok, "hello"} =
               CodeMode.run(
                 ~s|return tools["parallel__echo"]({}).content[1].text|,
                 tools,
                 call_fun: call
               )

      assert {:ok, "function"} = CodeMode.run(~s|return type(parallel)|, tools, call_fun: call)
    end
  end
end
