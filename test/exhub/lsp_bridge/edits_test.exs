defmodule Exhub.LspBridge.EditsTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.Handlers.{
    CodeAction,
    ExecuteCommand,
    Formatting,
    PrepareRename,
    RangeFormatting,
    Rename
  }

  @ctx %{path: "/proj/lib/a.ex", language_id: "elixir", args: %{}}

  defp ctx(args), do: Map.put(@ctx, :args, args)

  defp range do
    %{
      "start" => %{"line" => 1, "character" => 0},
      "end" => %{"line" => 1, "character" => 3}
    }
  end

  defp diagnostic(line) do
    %{
      "range" => %{
        "start" => %{"line" => line, "character" => 0},
        "end" => %{"line" => line, "character" => 5}
      },
      "severity" => 1,
      "message" => "boom"
    }
  end

  describe "PrepareRename" do
    test "metadata" do
      assert PrepareRename.name() == "prepare-rename"
      assert PrepareRename.method() == "textDocument/prepareRename"
      assert PrepareRename.provider() == "prepare_rename"
      refute PrepareRename.cancel_on_change?()
    end

    test "request params carry the position" do
      pos = %{"line" => 4, "character" => 2}
      assert PrepareRename.request_params(%{"position" => pos}, @ctx) == %{"position" => pos}
    end

    test "a bare Range is normalised" do
      r = range()
      assert {:rename_range, "/proj/lib/a.ex", ^r} = PrepareRename.process_response(r, @ctx)
    end

    test "gopls {range, placeholder} uses the inner range" do
      r = range()

      assert {:rename_range, _, ^r} =
               PrepareRename.process_response(%{"range" => r, "placeholder" => "x"}, @ctx)
    end

    test "nil yields a message" do
      assert PrepareRename.process_response(nil, @ctx) == {:message, "No rename range."}
    end
  end

  describe "Rename" do
    test "request params carry position and newName" do
      args = %{"position" => %{"line" => 0, "character" => 1}, "newName" => "bar"}

      assert Rename.request_params(args, @ctx) == %{
               "position" => %{"line" => 0, "character" => 1},
               "newName" => "bar"
             }
    end

    test "a WorkspaceEdit becomes a workspace_edit payload" do
      edit = %{"changes" => %{"file:///proj/lib/a.ex" => []}}

      assert Rename.process_response(edit, @ctx) == {:workspace_edit, edit, "Rename done."}
    end

    test "nil yields a message" do
      assert Rename.process_response(nil, @ctx) == {:message, "No rename found."}
    end
  end

  describe "Formatting" do
    test "options use lsp-bridge defaults" do
      assert Formatting.request_params(%{}, @ctx) == %{
               "options" => %{
                 "tabSize" => 4,
                 "insertSpaces" => true,
                 "trimTrailingWhitespace" => true,
                 "insertFinalNewline" => false,
                 "trimFinalNewlines" => true
               }
             }
    end

    test "options honour tabSize/insertSpaces" do
      assert %{"options" => %{"tabSize" => 2, "insertSpaces" => false}} =
               Formatting.request_params(%{"tabSize" => 2, "insertSpaces" => false}, @ctx)
    end

    test "edits become a format payload; empty yields a message" do
      edits = [%{"range" => range(), "newText" => "x"}]
      assert Formatting.process_response(edits, @ctx) == {:format, "/proj/lib/a.ex", edits}
      assert Formatting.process_response([], @ctx) == {:message, "Nothing to format."}
    end
  end

  describe "RangeFormatting" do
    test "request params carry range and options" do
      r = range()

      assert %{"range" => ^r, "options" => %{"tabSize" => 4}} =
               RangeFormatting.request_params(%{"range" => r}, @ctx)
    end

    test "edits become a format payload" do
      edits = [%{"range" => range(), "newText" => "x"}]
      assert RangeFormatting.process_response(edits, @ctx) == {:format, "/proj/lib/a.ex", edits}
    end
  end

  describe "CodeAction" do
    test "request params carry the range and an empty context by default" do
      assert CodeAction.request_params(%{"range" => range()}, @ctx) == %{
               "range" => range(),
               "context" => %{"diagnostics" => []}
             }
    end

    test "context.diagnostics only includes diagnostics overlapping the range" do
      ctx = Map.put(@ctx, :diagnostics, [diagnostic(1), diagnostic(9)])

      assert %{"context" => %{"diagnostics" => [included]}} =
               CodeAction.request_params(%{"range" => range()}, ctx)

      assert included["message"] == "boom"

      # The line-9 diagnostic is not part of the line-1 range.
      refute Enum.any?(
               CodeAction.request_params(%{"range" => range()}, ctx)["context"]["diagnostics"],
               &(&1["range"]["start"]["line"] == 9)
             )
    end

    test "an `only` kind is passed through" do
      assert %{"context" => %{"only" => ["quickfix"]}} =
               CodeAction.request_params(%{"range" => range(), "only" => "quickfix"}, @ctx)
    end

    test "actions become a code_actions payload; empty yields a message" do
      actions = [%{"title" => "Fix it", "kind" => "quickfix"}]

      assert CodeAction.process_response(actions, @ctx) ==
               {:code_actions, "/proj/lib/a.ex", actions}

      assert CodeAction.process_response([], @ctx) == {:message, "No code actions available."}
    end
  end

  describe "ExecuteCommand" do
    test "is not capability-gated" do
      assert ExecuteCommand.provider() == nil
    end

    test "request params carry command and arguments" do
      assert ExecuteCommand.request_params(%{"command" => "c", "arguments" => [1]}, @ctx) ==
               %{"command" => "c", "arguments" => [1]}

      assert ExecuteCommand.request_params(%{"command" => "c"}, @ctx) ==
               %{"command" => "c", "arguments" => []}
    end

    test "nil and plain results yield a message" do
      assert ExecuteCommand.process_response(nil, @ctx) == {:message, "Command executed."}

      assert ExecuteCommand.process_response(%{"ok" => true}, @ctx) ==
               {:message, "Command executed."}
    end

    test "a WorkspaceEdit result is applied" do
      edit = %{"changes" => %{}}

      assert ExecuteCommand.process_response(edit, @ctx) ==
               {:workspace_edit, edit, "Command executed."}
    end
  end
end
