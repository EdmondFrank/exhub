defmodule Exhub.LspBridge.SessionTest do
  use ExUnit.Case, async: false

  alias Exhub.LspBridge.{Config, Session}

  @tmp_dir System.tmp_dir!() |> Path.join("exhub_lsp_sess_#{:erlang.unique_integer([:positive])}")
  @script Path.join(@tmp_dir, "fake_lsp.exs")

  # A configurable stdio JSON-RPC server. `push` advertises incremental sync and
  # emits `publishDiagnostics` on didOpen/didChange; `pull` advertises a
  # diagnostic provider and answers `textDocument/diagnostic`. Either way it
  # answers a server-initiated `workspace/configuration`. Run as a script (no
  # `--no-halt`) so EOF on close exits the child instead of orphaning a beam.
  @fake ~S'''
  defmodule Fake do
    def mode, do: List.first(System.argv()) || "push"

    def read_frame do
      case IO.read(:stdio, :line) do
        :eof -> nil
        {:error, _} -> nil
        line ->
          case Integer.parse(String.replace(line, "Content-Length: ", "")) do
            {len, _} ->
              _blank = IO.read(:stdio, :line)
              IO.read(:stdio, len)
            _ ->
              read_frame()
          end
      end
    end

    def write(body), do: IO.write(:stdio, "Content-Length: #{byte_size(body)}\r\n\r\n" <> body)

    defp field(body, key) do
      case Regex.run(~r/"#{key}"\s*:\s*"([^"]+)"/, body) do
        [_, v] -> v
        _ -> nil
      end
    end

    defp id(body) do
      case Regex.run(~r/"id"\s*:\s*(\d+)/, body) do
        [_, i] -> i
        _ -> nil
      end
    end

    defp method(body) do
      case Regex.run(~r/"method"\s*:\s*"([^"]+)"/, body) do
        [_, m] -> m
        _ -> nil
      end
    end

    defp capabilities do
      base =
        ~s("textDocumentSync":{"change":2},"hoverProvider":true,"definitionProvider":true,) <>
          ~s("typeDefinitionProvider":true,"implementationProvider":true,"referencesProvider":true,) <>
          ~s("documentSymbolProvider":true,"workspaceSymbolProvider":true,) <>
          ~s("completionProvider":{"resolveProvider":true,"triggerCharacters":["."]},) <>
          ~s("renameProvider":{"prepareProvider":true},"documentFormattingProvider":true,) <>
          ~s("codeActionProvider":{"codeActionKinds":["quickfix"]},"callHierarchyProvider":true,) <>
          ~s("inlayHintProvider":true,) <>
          ~s("semanticTokensProvider":{"legend":{"tokenTypes":["namespace","variable","function"],"tokenModifiers":["declaration","readonly"]},"full":true})

      if mode() == "pull", do: ~s({#{base},"diagnosticProvider":{"identifier":"fake"}}), else: ~s({#{base}})
    end

    defp publish(uri, diags) do
      write(~s({"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{"uri":"#{uri}","diagnostics":#{diags}}}))
    end

    def loop do
      case read_frame() do
        nil -> :ok
        body -> handle(body); loop()
      end
    end

    defp handle(body) do
      m = method(body)
      i = id(body)

      cond do
        m == "initialize" ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":{"capabilities":#{capabilities()}}}))
          write(~s({"jsonrpc":"2.0","id":98,"method":"workspace/configuration","params":{"items":[{"section":"x"}]}}))

        m == "textDocument/didOpen" and mode() == "push" ->
          publish(field(body, "uri"), ~s([{"severity":1,"message":"open-error","range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}}}]))

        m == "textDocument/didChange" and mode() == "push" ->
          publish(field(body, "uri"), ~s([{"severity":2,"message":"change-warning","range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}}}]))

        m == "textDocument/diagnostic" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":{"kind":"full","items":[{"severity":1,"message":"pull-error","range":{"start":{"line":1,"character":0},"end":{"line":1,"character":1}}}]}}))

        m == "textDocument/definition" and i != nil ->
          if mode() == "slow", do: Process.sleep(500)

          write(~s({"jsonrpc":"2.0","id":#{i},"result":[{"uri":"file:///tmp/target.ex","range":{"start":{"line":2,"character":3},"end":{"line":2,"character":5}}}]}))

        m == "textDocument/hover" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":{"contents":{"kind":"markdown","value":"# Doc"}}}))

        m == "textDocument/documentSymbol" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":[{"name":"X","kind":5,"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}},"selectionRange":{"start":{"line":0,"character":0},"end":{"line":0,"character":1}}}]}))

        m == "workspace/symbol" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":[{"name":"Y","kind":12,"location":{"uri":"file:///tmp/y.ex","range":{"start":{"line":1,"character":0},"end":{"line":1,"character":1}}}}]}))

        m == "textDocument/completion" and i != nil ->
          write(~s|{"jsonrpc":"2.0","id":#{i},"result":{"items":[{"label":"hello","kind":3,"detail":"def hello()"},{"label":"world","kind":6,"detail":"var"}]}}|)

        m == "completionItem/resolve" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":{"documentation":{"kind":"markdown","value":"# Resolved"}}}))

        m == "textDocument/prepareRename" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":{"range":{"start":{"line":0,"character":4},"end":{"line":0,"character":9}},"placeholder":"hello"}}))

        m == "textDocument/rename" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":{"changes":{"file:///tmp/renamed.ex":[{"range":{"start":{"line":0,"character":4},"end":{"line":0,"character":9}},"newText":"greet"}]}}}))

        m == "textDocument/formatting" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":""}]}))

        m == "textDocument/codeAction" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":[{"title":"Fix it","kind":"quickfix","edit":{"changes":{}}}]}))

        m == "workspace/executeCommand" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":null}))

        m == "textDocument/prepareCallHierarchy" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":[{"name":"hello","kind":12,"uri":"file:///tmp/target.ex","range":{"start":{"line":2,"character":0},"end":{"line":2,"character":5}},"selectionRange":{"start":{"line":2,"character":0},"end":{"line":2,"character":5}},"data":{"x":1}}]}))

        m == "callHierarchy/incomingCalls" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":[{"from":{"name":"caller","kind":12,"uri":"file:///tmp/caller.ex","range":{"start":{"line":0,"character":0},"end":{"line":0,"character":3}},"selectionRange":{"start":{"line":0,"character":0},"end":{"line":0,"character":3}}},"fromRanges":[{"start":{"line":0,"character":0},"end":{"line":0,"character":3}}]}]}))

        m == "callHierarchy/outgoingCalls" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":[{"to":{"name":"callee","kind":12,"uri":"file:///tmp/callee.ex","range":{"start":{"line":1,"character":0},"end":{"line":1,"character":3}},"selectionRange":{"start":{"line":1,"character":0},"end":{"line":1,"character":3}}},"fromRanges":[]}]}))

        m == "textDocument/inlayHint" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":[{"position":{"line":1,"character":3},"label":":ok"}]}))

        m == "textDocument/semanticTokens/full" and i != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":{"data":[0,0,4,2,1]}}))

        i != nil and m != nil ->
          write(~s({"jsonrpc":"2.0","id":#{i},"result":{"contents":"fake-hover"}}))

        true -> :ok
      end
    end
  end

  Fake.loop()
  '''

  setup_all do
    File.mkdir_p!(@tmp_dir)
    File.write!(@script, @fake)
    {:ok, _registry} = start_supervised({Registry, keys: :unique, name: Exhub.LspBridge.Registry})

    {:ok, _sup} =
      start_supervised(
        {DynamicSupervisor, strategy: :one_for_one, name: Exhub.LspBridge.Supervisor}
      )

    {:ok, _sessions} =
      start_supervised(
        {DynamicSupervisor, strategy: :one_for_one, name: Exhub.LspBridge.SessionSupervisor}
      )

    on_exit(fn -> File.rm_rf(@tmp_dir) end)
    :ok
  end

  defp start_session(mode, extra \\ %{}) do
    elixir = System.find_executable("elixir") || flunk("elixir not on PATH")
    name = "fake-#{mode}-#{:erlang.unique_integer([:positive])}"

    info = %Config{name: name, command: elixir, args: [@script, mode], settings: %{}}

    opts =
      Map.merge(
        %{multi: false, server_infos: [info], owner: self(), diag_idle: 0},
        extra
      )

    {:ok, pid} = Session.start_link(@tmp_dir, {:single, name}, opts)
    {pid, name}
  end

  test "open-file delivers pushed diagnostics; change-file refreshes them" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "x.ex")

    assert {:ok, [_server]} =
             Session.open_file(pid, path, "defmodule X do\nend\n", %{"language-id" => "elixir"})

    assert_receive {:lsp_diagnostics_update, ^path, [diag], 1}, 10_000
    assert diag["message"] == "open-error"
    assert diag["server-name"] =~ "fake-push"

    change = %{
      "range" => %{
        "start" => %{"line" => 0, "character" => 0},
        "end" => %{"line" => 0, "character" => 0}
      },
      "rangeLength" => 0,
      "text" => "x"
    }

    assert :ok = Session.change_file(pid, path, change)
    assert_receive {:lsp_diagnostics_update, ^path, [updated], 1}, 10_000
    assert updated["message"] == "change-warning"

    Session.shutdown(pid)
  end

  test "pull diagnostics are requested on open and recorded" do
    {pid, _name} = start_session("pull", %{diag_idle: 50})
    path = Path.join(@tmp_dir, "y.ex")

    assert {:ok, [_server]} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})

    assert_receive {:lsp_diagnostics_update, ^path, [diag], 1}, 10_000
    assert diag["message"] == "pull-error"

    Session.shutdown(pid)
  end

  test "close-file sends didClose and stops the session when the last document closes" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "z.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})

    ref = Process.monitor(pid)
    assert :ok = Session.close_file(pid, path)
    assert_receive {:DOWN, ^ref, :process, ^pid, _reason}, 5_000
  end

  test "diagnostics/3 returns the merged cache for an open document" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "w.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _diagnostics, _count}, 10_000

    assert {:ok, [diag]} = Session.diagnostics(pid, path, [])
    assert diag["message"] == "open-error"

    Session.shutdown(pid)
  end

  test "nil optional options fall back to defaults (the ClientManager call shape)" do
    # `ClientManager.open_in_session/4` always emits `:diag_idle` and
    # `:hide_severities`, passing `nil` when Emacs omits them. A nil diag_idle
    # used to crash `schedule_pull/2` (Process.send_after/3) and a nil
    # hide_severities used to crash `Diagnostics.merge/2` — so exercise the
    # exact option map it builds.
    {pid, _name} = start_session("push", %{diag_idle: nil, hide_severities: nil})
    path = Path.join(@tmp_dir, "nil_opts.ex")

    assert {:ok, [_server]} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, [diag], 1}, 10_000
    assert diag["message"] == "open-error"

    assert {:ok, [merged]} = Session.diagnostics(pid, path, [])
    assert merged["message"] == "open-error"

    Session.shutdown(pid)
  end

  test "perform find-define returns normalised locations" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "def.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "find-define", %{
               "position" => %{"line" => 0, "character" => 0}
             })

    assert_receive {:lsp_handler_result, {:locations, ^path, "definition", [loc]}}, 10_000
    assert loc["path"] == "/tmp/target.ex"
    assert loc["range"]["start"]["line"] == 2

    Session.shutdown(pid)
  end

  test "perform hover returns markdown" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "hover.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "hover", %{"position" => %{"line" => 0, "character" => 0}})

    assert_receive {:lsp_handler_result, {:hover, ^path, markdown}}, 10_000
    assert markdown == "# Doc"

    Session.shutdown(pid)
  end

  test "perform document-symbol passes the tree through" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "sym.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000
    assert :ok = Session.perform(pid, path, "document-symbol", %{})

    assert_receive {:lsp_handler_result, {:symbols, ^path, [%{"name" => "X"}]}}, 10_000

    Session.shutdown(pid)
  end

  test "a command whose provider is unsupported yields a handler error" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "nope.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "signature-help", %{
               "position" => %{"line" => 0, "character" => 0}
             })

    assert_receive {:lsp_handler_error, msg}, 10_000
    assert msg =~ "signature-help"

    Session.shutdown(pid)
  end

  test "cancel_on_change drops a stale definition response" do
    {pid, _name} = start_session("slow")
    path = Path.join(@tmp_dir, "stale.ex")

    assert {:ok, _} = Session.open_file(pid, path, "abc", %{"language-id" => "elixir"})

    assert :ok =
             Session.perform(pid, path, "find-define", %{
               "position" => %{"line" => 0, "character" => 0}
             })

    # Edit before the (delayed) definition response lands, so the document's
    # `last_change` moves past the request time and the response is discarded.
    change = %{
      "range" => %{
        "start" => %{"line" => 0, "character" => 0},
        "end" => %{"line" => 0, "character" => 0}
      },
      "rangeLength" => 0,
      "text" => "z"
    }

    assert :ok = Session.change_file(pid, path, change)

    refute_receive {:lsp_handler_result, _}, 1_500
    Session.shutdown(pid)
  end

  test "perform completion returns the server's candidates and item map" do
    {pid, name} = start_session("push")
    path = Path.join(@tmp_dir, "comp.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "completion", %{
               "position" => %{"line" => 0, "character" => 0},
               "char" => ".",
               "prefix" => "he",
               "match-mode" => "prefix",
               "case-mode" => "ignore"
             })

    assert_receive {:lsp_handler_result, {:completion, ^path, ^name, candidates, items, meta}},
                   10_000

    assert Enum.map(candidates, & &1["label"]) == ["hello"]
    assert map_size(items) == 1
    assert meta["server-names"] == [name]
    assert meta["trigger-characters"] == ["."]

    Session.shutdown(pid)
  end

  test "completion-item-resolve targets the server that produced the candidate" do
    {pid, name} = start_session("push")
    path = Path.join(@tmp_dir, "resolve.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "completion-item-resolve", %{
               "key" => "hello_def hello()",
               "server" => name,
               "item" => %{"label" => "hello", "kind" => 3}
             })

    assert_receive {:lsp_handler_result, {:completion_doc, ^path, ^name, _key, "# Resolved", []}},
                   10_000

    Session.shutdown(pid)
  end

  test "perform prepare-rename returns the range" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "prep.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "prepare-rename", %{
               "position" => %{"line" => 0, "character" => 6}
             })

    assert_receive {:lsp_handler_result,
                    {:rename_range, ^path, %{"start" => %{"character" => 4}}}},
                   10_000

    Session.shutdown(pid)
  end

  test "perform rename returns a workspace edit" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "rename.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "rename", %{
               "position" => %{"line" => 0, "character" => 6},
               "newName" => "greet"
             })

    assert_receive {:lsp_handler_result,
                    {:workspace_edit, %{"changes" => changes}, "Rename done."}},
                   10_000

    assert Map.has_key?(changes, "file:///tmp/renamed.ex")

    Session.shutdown(pid)
  end

  test "perform format returns text edits" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "fmt.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "format", %{"tabSize" => 2, "insertSpaces" => true})

    assert_receive {:lsp_handler_result, {:format, ^path, [_edit]}}, 10_000

    Session.shutdown(pid)
  end

  test "perform code-action returns actions" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "ca.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "code-action", %{
               "range" => %{
                 "start" => %{"line" => 0, "character" => 0},
                 "end" => %{"line" => 0, "character" => 1}
               }
             })

    assert_receive {:lsp_handler_result, {:code_actions, ^path, [%{"title" => "Fix it"}]}},
                   10_000

    Session.shutdown(pid)
  end

  test "perform execute-command is not capability-gated" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "exec.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "execute-command", %{"command" => "c", "arguments" => []})

    assert_receive {:lsp_handler_result, {:message, "Command executed."}}, 10_000

    Session.shutdown(pid)
  end

  test "update-file replaces content and notifies the server" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "upd.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, [open_diag], 1}, 10_000
    assert open_diag["message"] == "open-error"

    assert :ok = Session.update_file(pid, path, "greet()\n")

    # The full-text didChange reaches the server, which pushes a fresh diagnostic.
    assert_receive {:lsp_diagnostics_update, ^path, [diag], 1}, 10_000
    assert diag["message"] == "change-warning"

    Session.shutdown(pid)
  end

  test "perform call-hierarchy-prepare returns the items" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "chp.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "call-hierarchy-prepare", %{
               "position" => %{"line" => 0, "character" => 0}
             })

    assert_receive {:lsp_handler_result, {:call_hierarchy_items, ^path, [%{"name" => "hello"}]}},
                   10_000

    Session.shutdown(pid)
  end

  test "perform call-hierarchy-incoming returns the `from' calls" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "chi.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "call-hierarchy-incoming", %{
               "item" => %{"name" => "hello"}
             })

    assert_receive {:lsp_handler_result,
                    {:call_hierarchy, ^path, "incoming", [%{"from" => %{"name" => "caller"}}]}},
                   10_000

    Session.shutdown(pid)
  end

  test "perform call-hierarchy-outgoing returns the `to' calls" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "cho.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "call-hierarchy-outgoing", %{
               "item" => %{"name" => "hello"}
             })

    assert_receive {:lsp_handler_result,
                    {:call_hierarchy, ^path, "outgoing", [%{"to" => %{"name" => "callee"}}]}},
                   10_000

    Session.shutdown(pid)
  end

  test "perform inlay-hint returns the hints" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "inlay.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok =
             Session.perform(pid, path, "inlay-hint", %{
               "range" => %{
                 "start" => %{"line" => 0, "character" => 0},
                 "end" => %{"line" => 0, "character" => 1}
               }
             })

    assert_receive {:lsp_handler_result, {:inlay_hints, ^path, [%{"label" => ":ok"}]}}, 10_000

    Session.shutdown(pid)
  end

  test "perform semantic-tokens decodes the legend" do
    {pid, _name} = start_session("push")
    path = Path.join(@tmp_dir, "sem.ex")

    assert {:ok, _} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000

    assert :ok = Session.perform(pid, path, "semantic-tokens", %{})

    assert_receive {:lsp_handler_result, {:semantic_tokens, ^path, [token]}}, 10_000
    assert token["line"] == 0
    assert token["character"] == 0
    assert token["type"] == "function"
    assert token["modifiers"] == ["declaration"]

    Session.shutdown(pid)
  end

  test "idle servers stop after the timeout and restart lazily on the next edit" do
    {pid, name} = start_session("push", %{idle_stop: 100})
    path = Path.join(@tmp_dir, "idle.ex")
    key = Exhub.LspBridge.Server.key(@tmp_dir, name)
    registry = Exhub.LspBridge.Registry

    assert {:ok, [^name]} = Session.open_file(pid, path, "x", %{"language-id" => "elixir"})
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000
    assert [_] = Registry.lookup(registry, key)

    # After the idle timeout the server is reaped, but the session and its open
    # document survive.
    Process.sleep(400)
    assert [] = Registry.lookup(registry, key)
    assert Process.alive?(pid)

    # The next edit restarts the server lazily and re-opens the document.
    change = %{
      "range" => %{
        "start" => %{"line" => 0, "character" => 0},
        "end" => %{"line" => 0, "character" => 0}
      },
      "rangeLength" => 0,
      "text" => "z"
    }

    assert :ok = Session.change_file(pid, path, change)
    assert_receive {:lsp_diagnostics_update, ^path, _, _}, 10_000
    assert [_] = Registry.lookup(registry, key)

    Session.shutdown(pid)
  end
end
