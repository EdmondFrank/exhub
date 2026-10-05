defmodule Exhub.LspBridge.HandlersTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.Handlers

  alias Exhub.LspBridge.Handlers.{
    Definition,
    DocumentSymbol,
    Hover,
    Locations,
    References,
    SignatureHelp,
    WorkspaceSymbol
  }

  @ctx %{path: "/proj/lib/a.ex", language_id: "elixir", args: %{}}

  defp ctx(args), do: Map.put(@ctx, :args, args)

  defp location do
    %{
      "uri" => "file:///proj/lib/a.ex",
      "range" => %{
        "start" => %{"line" => 1, "character" => 2},
        "end" => %{"line" => 1, "character" => 5}
      }
    }
  end

  describe "registry" do
    test "exposes the registered command set" do
      assert Handlers.commands() == [
               "hover",
               "find-define",
               "find-type-define",
               "find-implementation",
               "find-references",
               "document-symbol",
               "workspace-symbol",
               "signature-help",
               "completion",
               "completion-item-resolve",
               "prepare-rename",
               "rename",
               "format",
               "range-format",
               "code-action",
               "execute-command",
               "call-hierarchy-prepare",
               "call-hierarchy-incoming",
               "call-hierarchy-outgoing",
               "inlay-hint",
               "semantic-tokens"
             ]
    end

    test "fetch/1 resolves by command and is nil otherwise" do
      assert Handlers.fetch("hover") == Hover
      assert Handlers.fetch("nope") == nil
    end

    test "every handler advertises consistent metadata" do
      for mod <- Handlers.all() do
        assert is_binary(mod.name())
        assert is_binary(mod.method())
        assert is_binary(mod.provider()) or is_nil(mod.provider())
        assert is_boolean(mod.cancel_on_change?())
      end
    end
  end

  describe "Locations" do
    test "normalises a single Location" do
      loc = location()
      assert [normalised] = Locations.normalize(loc)
      assert normalised["path"] == "/proj/lib/a.ex"
      assert normalised["range"] == loc["range"]
      assert normalised["selectionRange"] == loc["range"]
    end

    test "normalises a list and percent-decodes the URI" do
      assert [normalised] =
               Locations.normalize([%{"uri" => "file:///proj/lib/a%20b.ex", "range" => %{}}])

      assert normalised["path"] == "/proj/lib/a b.ex"
    end

    test "normalises LocationLink target* fields" do
      link = %{
        "targetUri" => "file:///proj/x.ex",
        "targetRange" => %{"start" => %{"line" => 3, "character" => 0}},
        "targetSelectionRange" => %{"start" => %{"line" => 3, "character" => 4}}
      }

      assert [normalised] = Locations.normalize(link)
      assert normalised["path"] == "/proj/x.ex"
      assert normalised["range"] == link["targetRange"]
      assert normalised["selectionRange"] == link["targetSelectionRange"]
    end

    test "nil and junk normalise to []" do
      assert Locations.normalize(nil) == []
      assert Locations.normalize("x") == []
    end
  end

  describe "Hover" do
    test "markdown and plaintext contents pass through" do
      assert Hover.process_response(
               %{"contents" => %{"kind" => "markdown", "value" => "# H"}},
               @ctx
             ) == {:hover, "/proj/lib/a.ex", "# H"}

      assert {:hover, _, "text"} =
               Hover.process_response(
                 %{"contents" => %{"kind" => "plaintext", "value" => "text"}},
                 @ctx
               )
    end

    test "a bare string becomes a text code block" do
      assert {:hover, _, "```text\nhi\n```"} =
               Hover.process_response(%{"contents" => "hi"}, @ctx)
    end

    test "a MarkedString language wraps a code block" do
      assert {:hover, _, "```elixir\ndef x\n```"} =
               Hover.process_response(
                 %{"contents" => [%{"language" => "elixir", "value" => "def x"}]},
                 @ctx
               )
    end

    test "absent or empty contents yields a message" do
      assert Hover.process_response(nil, @ctx) == {:message, "No documentation available."}
      assert Hover.process_response(%{}, @ctx) == {:message, "No documentation available."}

      assert Hover.process_response(%{"contents" => ""}, @ctx) ==
               {:message, "No documentation available."}
    end
  end

  describe "Definition" do
    test "normalises the result into a locations payload" do
      assert {:locations, "/proj/lib/a.ex", "definition", [normalised]} =
               Definition.process_response([location()], @ctx)

      assert normalised["path"] == "/proj/lib/a.ex"
    end

    test "no result yields a message" do
      assert Definition.process_response(nil, @ctx) == {:message, "No definition found."}
    end

    test "request params carry the position" do
      pos = %{"line" => 4, "character" => 2}
      assert Definition.request_params(%{"position" => pos}, @ctx) == %{"position" => pos}
    end
  end

  describe "References" do
    test "request params include the context" do
      assert References.request_params(%{"position" => %{}}, @ctx) ==
               %{"position" => %{}, "context" => %{"includeDeclaration" => false}}
    end

    test "normalises results into a references payload" do
      assert {:locations, _, "references", [_]} = References.process_response([location()], @ctx)
    end
  end

  describe "DocumentSymbol" do
    test "passes the tree through" do
      tree = [%{"name" => "X", "kind" => 5}]
      assert DocumentSymbol.process_response(tree, @ctx) == {:symbols, "/proj/lib/a.ex", tree}
      assert DocumentSymbol.process_response([], @ctx) == {:message, "No symbols found."}
    end
  end

  describe "WorkspaceSymbol" do
    test "strips whitespace from the query" do
      assert WorkspaceSymbol.request_params(%{"query" => "  foo bar "}, @ctx) == %{
               "query" => "foobar"
             }
    end

    test "passes results through with the original query" do
      result = [%{"name" => "Y", "location" => %{"uri" => "file:///p/y.ex", "range" => %{}}}]

      assert {:workspace_symbols, "foo bar", ^result} =
               WorkspaceSymbol.process_response(result, ctx(%{"query" => "foo bar"}))
    end

    test "no result yields a message" do
      assert WorkspaceSymbol.process_response([], @ctx) == {:message, "No symbols found."}
    end
  end

  describe "SignatureHelp" do
    test "passes results through when signatures exist" do
      result = %{"signatures" => [%{"label" => "f(a, b)"}], "activeSignature" => 0}

      assert SignatureHelp.process_response(result, @ctx) ==
               {:signature_help, "/proj/lib/a.ex", result}
    end

    test "empty or absent signatures yield a message" do
      assert SignatureHelp.process_response(%{"signatures" => []}, @ctx) ==
               {:message, "No signature help available."}

      assert SignatureHelp.process_response(nil, @ctx) ==
               {:message, "No signature help available."}
    end
  end
end
