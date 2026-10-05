defmodule Exhub.LspBridge.CompletionTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.Handlers.{Completion, CompletionItem}

  defp ctx(args, opts \\ []) do
    server = Keyword.get(opts, :server, "fake")

    %{
      path: "/proj/lib/a.ex",
      language_id: "elixir",
      args: args,
      server: server,
      trigger_characters: Keyword.get(opts, :trigger_characters, []),
      server_names: [server]
    }
  end

  defp completion(result, args, opts \\ []) do
    Completion.process_response(result, ctx(args, opts))
  end

  test "builds candidates, display labels and the resolve item map" do
    result = %{
      "items" => [
        %{"label" => "hello", "kind" => 3, "detail" => "def hello()"},
        %{"label" => "world", "kind" => 6, "detail" => "variable"}
      ]
    }

    assert {:completion, "/proj/lib/a.ex", "fake", [candidate], items, meta} =
             completion(result, %{"prefix" => "h", "match-mode" => "prefix"})

    assert candidate["label"] == "hello"
    assert candidate["icon"] == "function"
    assert candidate["displayLabel"] == "hello => def hello()"
    assert candidate["backend"] == "lsp"
    assert candidate["server"] == "fake"
    assert candidate["score"] == 1000

    assert map_size(items) == 1
    assert items[candidate["key"]]["label"] == "hello"
    assert meta["server-names"] == ["fake"]
  end

  test "fuzzy match keeps subsequences" do
    result = %{"items" => [%{"label" => "hello_world", "kind" => 3}]}

    assert {:completion, _, _, [candidate], _, _} =
             completion(result, %{"prefix" => "hlo", "match-mode" => "fuzzy"})

    assert candidate["label"] == "hello_world"
  end

  test "substring and case-sensitive modes" do
    result = %{"items" => [%{"label" => "Canonical", "kind" => 3}]}

    assert {:completion, _, _, [_], _, _} =
             completion(result, %{"prefix" => "can", "match-mode" => "substring"})

    assert {:completion, _, _, [], _, _} =
             completion(result, %{
               "prefix" => "can",
               "match-mode" => "prefix",
               "case-mode" => "sensitive"
             })
  end

  test "block kind list drops candidate kinds" do
    result = %{
      "items" => [
        %{"label" => "hello", "kind" => 3},
        %{"label" => "helio", "kind" => 6}
      ]
    }

    assert {:completion, _, _, [candidate], _, _} =
             completion(result, %{"prefix" => "hel", "block-kind-list" => ["variable"]})

    assert candidate["label"] == "hello"
  end

  test "converts LSP snippet placeholders to yasnippet form" do
    result = %{
      "items" => [
        %{"label" => "foo", "kind" => 15, "insertText" => "${1:foo} ${1:foo} ${2:bar}"}
      ]
    }

    assert {:completion, _, _, [candidate], _, _} = completion(result, %{"prefix" => "f"})
    assert candidate["insertText"] == "${1:foo} ${1} ${2:bar}"
    assert candidate["icon"] == "snippet"
  end

  test "prefix matches sort before non-prefix matches" do
    result = %{
      "items" => [
        %{"label" => "xx_hello", "kind" => 3},
        %{"label" => "hello_world", "kind" => 3}
      ]
    }

    assert {:completion, _, _, candidates, _, _} = completion(result, %{"prefix" => "hello"})
    assert Enum.map(candidates, & &1["label"]) == ["hello_world", "xx_hello"]
  end

  test "higher server scores sort first, then length" do
    result = %{
      "items" => [
        %{"label" => "abc", "kind" => 3, "score" => 10},
        %{"label" => "abcd", "kind" => 3, "score" => 20}
      ]
    }

    assert {:completion, _, _, candidates, _, _} =
             completion(result, %{"prefix" => "", "match-mode" => "fuzzy"})

    assert Enum.map(candidates, & &1["label"]) == ["abcd", "abc"]
  end

  test "auto-import attaches edits and appends a hash to the key" do
    result = %{
      "items" => [
        %{
          "label" => "hello",
          "kind" => 3,
          "additionalTextEdits" => [%{"newText" => "import Foo", "range" => %{}}]
        }
      ]
    }

    assert {:completion, _, _, [candidate], items, _} = completion(result, %{"prefix" => "h"})
    assert candidate["additionalTextEdits"] == [%{"newText" => "import Foo", "range" => %{}}]
    # hash suffix differs from the plain label_detail key
    refute candidate["key"] == "hello_"
    assert Map.has_key?(items, candidate["key"])
  end

  test "items-limit caps the candidate list" do
    items = for i <- 1..20, do: %{"label" => "item#{i}", "kind" => 3}

    assert {:completion, _, _, candidates, _, _} =
             completion(%{"items" => items}, %{"prefix" => "item", "items-limit" => 5})

    assert length(candidates) == 5
  end

  test "trigger character produces a TriggerCharacter completion context" do
    params =
      Completion.request_params(
        %{"position" => %{"line" => 1, "character" => 2}, "char" => "."},
        ctx(%{}, trigger_characters: ["."])
      )

    assert params["context"] == %{"triggerCharacter" => ".", "triggerKind" => 2}
  end

  test "an untriggered completion uses Invoked context" do
    params =
      Completion.request_params(
        %{"position" => %{"line" => 1, "character" => 2}, "char" => "a"},
        ctx(%{}, trigger_characters: ["."])
      )

    assert params["context"] == %{"triggerKind" => 1}
  end

  test "metadata advertises the handler" do
    assert Completion.name() == "completion"
    assert Completion.method() == "textDocument/completion"
    assert Completion.provider() == "completion"
    assert Completion.cancel_on_change?()
  end

  describe "completion-item-resolve" do
    test "returns markdown documentation" do
      assert {:completion_doc, "/proj/lib/a.ex", "fake", "k", "# Resolved", []} =
               CompletionItem.process_response(
                 %{"documentation" => %{"kind" => "markdown", "value" => "# Resolved"}},
                 ctx(%{"key" => "k"})
               )
    end

    test "falls back to detail and carries additionalTextEdits" do
      result = %{"detail" => "def hello()", "additionalTextEdits" => [%{"newText" => "x"}]}

      assert {:completion_doc, _, _, "k", "def hello()", [%{"newText" => "x"}]} =
               CompletionItem.process_response(result, ctx(%{"key" => "k"}))
    end

    test "nil result resolves to an empty documentation" do
      assert {:completion_doc, _, _, "k", "", []} =
               CompletionItem.process_response(nil, ctx(%{"key" => "k"}))
    end

    test "request params are the item itself" do
      assert CompletionItem.request_params(%{"item" => %{"label" => "x"}}, ctx(%{})) == %{
               "label" => "x"
             }

      assert CompletionItem.name() == "completion-item-resolve"
      assert CompletionItem.method() == "completionItem/resolve"
      refute CompletionItem.cancel_on_change?()
    end
  end
end
