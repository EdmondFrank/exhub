defmodule Exhub.LspBridge.CapabilitiesTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.Capabilities

  @result %{
    "capabilities" => %{
      "textDocumentSync" => %{"change" => 2, "save" => %{"includeText" => true}},
      "hoverProvider" => true,
      "completionProvider" => %{"triggerCharacters" => [".", ":"], "resolveProvider" => true},
      "definitionProvider" => true,
      "renameProvider" => %{"prepareProvider" => true},
      "codeActionProvider" => %{"codeActionKinds" => ["quickfix"]},
      "documentFormattingProvider" => true,
      "inlayHintProvider" => %{},
      "semanticTokensProvider" => %{"legend" => %{"tokenTypes" => ["keyword"]}},
      "diagnosticProvider" => %{"identifier" => "diag-id", "interFileDependencies" => false},
      "referencesProvider" => false
    }
  }

  test "extracts sync kind and the save includeText flag" do
    caps = Capabilities.from_initialize(@result)
    assert caps.sync_kind == 2
    assert caps.save_include_text == true
  end

  test "provider gating follows lsp-bridge truthiness (present and not false)" do
    caps = Capabilities.from_initialize(@result)

    for name <- ~w(completion completion_resolve hover definition prepare_rename code_action
                   formatting inlay_hint semantic_tokens diagnostic) do
      assert Capabilities.supports?(caps, name), "expected support for #{name}"
    end

    refute Capabilities.supports?(caps, "references")
    refute Capabilities.supports?(caps, "implementation")
  end

  test "extracts trigger chars, code action kinds, semantic legend and diagnostic id" do
    caps = Capabilities.from_initialize(@result)
    assert caps.trigger_characters == [".", ":"]
    assert caps.code_action_kinds == ["quickfix"]
    assert caps.semantic_tokens["legend"]["tokenTypes"] == ["keyword"]
    assert caps.diagnostic_identifier == "diag-id"
  end

  test "sync kind from an integer, an explicit 0, and the incremental default" do
    assert Capabilities.from_initialize(%{"capabilities" => %{"textDocumentSync" => 1}}).sync_kind ==
             1

    assert Capabilities.from_initialize(%{"capabilities" => %{"textDocumentSync" => 0}}).sync_kind ==
             0

    assert Capabilities.from_initialize(%{"capabilities" => %{}}).sync_kind == 2
  end

  test "forceInlayHint opts in without a capability" do
    caps = Capabilities.from_initialize(%{"capabilities" => %{}}, %{"forceInlayHint" => true})
    assert Capabilities.supports?(caps, "inlay_hint")
  end
end
