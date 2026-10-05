defmodule Exhub.LspBridge.SemanticTokensTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.Handlers.SemanticTokens

  @legend %{
    "tokenTypes" => ["namespace", "variable", "function"],
    "tokenModifiers" => ["declaration", "readonly"]
  }

  @ctx %{
    path: "/proj/lib/a.ex",
    language_id: "elixir",
    args: %{},
    semantic_tokens_legend: @legend
  }

  describe "SemanticTokens" do
    test "metadata" do
      assert SemanticTokens.name() == "semantic-tokens"
      assert SemanticTokens.method() == "textDocument/semanticTokens/full"
      assert SemanticTokens.provider() == "semantic_tokens"
      refute SemanticTokens.cancel_on_change?()
    end

    test "full requests carry no params" do
      assert SemanticTokens.request_params(%{}, @ctx) == %{}
    end

    test "decode expands deltas to absolute positions and resolves type names" do
      data = [0, 0, 3, 0, 0, 0, 5, 2, 1, 0, 1, 2, 4, 2, 1]

      assert SemanticTokens.decode(data, @legend) == [
               %{
                 "line" => 0,
                 "character" => 0,
                 "length" => 3,
                 "type" => "namespace",
                 "modifiers" => []
               },
               %{
                 "line" => 0,
                 "character" => 5,
                 "length" => 2,
                 "type" => "variable",
                 "modifiers" => []
               },
               %{
                 "line" => 1,
                 "character" => 2,
                 "length" => 4,
                 "type" => "function",
                 "modifiers" => ["declaration"]
               }
             ]
    end

    test "decode maps a modifier bitmask to modifier names" do
      # mask 3 = declaration (bit 0) | readonly (bit 1)
      assert [%{"type" => "variable", "modifiers" => ["declaration", "readonly"]}] =
               SemanticTokens.decode([0, 0, 1, 1, 3], @legend)
    end

    test "decode degrades gracefully without a legend" do
      assert [%{"type" => nil, "modifiers" => []}] =
               SemanticTokens.decode([0, 0, 1, 0, 0], nil)
    end

    test "process_response decodes using the context legend" do
      assert {:semantic_tokens, "/proj/lib/a.ex", [token]} =
               SemanticTokens.process_response(%{"data" => [0, 0, 3, 0, 0]}, @ctx)

      assert token["type"] == "namespace"
      assert token["line"] == 0
      assert token["character"] == 0
    end

    test "an absent result yields an empty list" do
      assert SemanticTokens.process_response(nil, @ctx) ==
               {:semantic_tokens, "/proj/lib/a.ex", []}

      assert SemanticTokens.process_response(%{}, @ctx) ==
               {:semantic_tokens, "/proj/lib/a.ex", []}
    end
  end
end
