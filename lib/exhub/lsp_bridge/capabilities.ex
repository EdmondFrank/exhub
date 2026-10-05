defmodule Exhub.LspBridge.Capabilities do
  @moduledoc """
  Derivation and gating of a language server's capabilities.

  Elixir port of `core/lspserver.py::LspServer.save_attribute_from_message`.
  The `initialize` result's `capabilities` object is flattened into a set of
  boolean provider flags plus a few structured extras (sync kind, completion
  trigger characters, semantic-token legend, diagnostic identifier).

  Providers follow lsp-bridge's truthiness rule: a capability is *supported*
  when its value is present and not literally `false`, so a provider object or
  `true` both count. This is the cache every later handler (`hover`,
  `completion`, `rename`, …) gates on.
  """

  defstruct sync_kind: 2,
            save_include_text: false,
            providers: %{},
            trigger_characters: [],
            code_action_kinds: [],
            semantic_tokens: nil,
            diagnostic_identifier: nil,
            raw: %{}

  @type t :: %__MODULE__{}

  # Provider name => nested path into the capabilities object.
  @provider_paths %{
    "completion" => ["completionProvider"],
    "completion_resolve" => ["completionProvider", "resolveProvider"],
    "hover" => ["hoverProvider"],
    "definition" => ["definitionProvider"],
    "type_definition" => ["typeDefinitionProvider"],
    "implementation" => ["implementationProvider"],
    "references" => ["referencesProvider"],
    "document_symbol" => ["documentSymbolProvider"],
    "workspace_symbol" => ["workspaceSymbolProvider"],
    "signature_help" => ["signatureHelpProvider"],
    "rename" => ["renameProvider"],
    "prepare_rename" => ["renameProvider", "prepareProvider"],
    "code_action" => ["codeActionProvider"],
    "formatting" => ["documentFormattingProvider"],
    "range_formatting" => ["documentRangeFormattingProvider"],
    "inlay_hint" => ["inlayHintProvider"],
    "semantic_tokens" => ["semanticTokensProvider"],
    "call_hierarchy" => ["callHierarchyProvider"],
    "execute_command" => ["executeCommandProvider"],
    "diagnostic" => ["diagnosticProvider"]
  }

  @doc """
  Derive capabilities from an `initialize` result.

  `settings` is the server's configured `settings` map, consulted for
  `forceInlayHint` (some servers support inlay hints without advertising the
  capability — lsp-bridge honors an explicit opt-in).
  """
  @spec from_initialize(map(), map()) :: t()
  def from_initialize(result, settings \\ %{}) when is_map(result) do
    caps = result["capabilities"] || %{}

    providers =
      Map.new(@provider_paths, fn {name, path} -> {name, present?(dig(caps, path))} end)
      |> Map.put("inlay_hint", inlay_hint?(caps, settings))

    %__MODULE__{
      sync_kind: normalize_sync_kind(dig(caps, ["textDocumentSync"])),
      save_include_text: save_include_text(caps),
      providers: providers,
      trigger_characters: List.wrap(dig(caps, ["completionProvider", "triggerCharacters"])),
      code_action_kinds: List.wrap(dig(caps, ["codeActionProvider", "codeActionKinds"])),
      semantic_tokens: dig(caps, ["semanticTokensProvider"]),
      diagnostic_identifier: dig(caps, ["diagnosticProvider", "identifier"]),
      raw: caps
    }
  end

  @doc "True when the server advertises the provider `name` (e.g. `\"hover\"`)."
  @spec supports?(t(), String.t()) :: boolean()
  def supports?(%__MODULE__{providers: providers}, name), do: Map.get(providers, name, false)

  @doc "Text document sync kind: 0 none, 1 full, 2 incremental."
  @spec sync_kind(t()) :: non_neg_integer()
  def sync_kind(%__MODULE__{sync_kind: k}), do: k

  defp inlay_hint?(caps, settings) do
    present?(dig(caps, ["inlayHintProvider"])) or
      present?(dig(caps, ["clangdInlayHintsProvider"])) or
      Map.get(settings || %{}, "forceInlayHint") == true
  end

  # A missing capability leaves lsp-bridge's default (incremental sync) intact;
  # an explicit `0` (None) is honored.
  defp normalize_sync_kind(nil), do: 2
  defp normalize_sync_kind(n) when is_integer(n), do: n

  defp normalize_sync_kind(%{} = m) do
    case Map.get(m, "change") do
      c when is_integer(c) -> c
      _ -> 2
    end
  end

  defp normalize_sync_kind(_other), do: 2

  defp save_include_text(caps) do
    case dig(caps, ["textDocumentSync"]) do
      %{} = sync -> dig(sync, ["save", "includeText"]) == true
      _ -> false
    end
  end

  defp present?(nil), do: false
  defp present?(false), do: false
  defp present?(_), do: true

  # Path-aware dig that tolerates non-map intermediates instead of raising.
  defp dig(value, []), do: value
  defp dig(map, [key | rest]) when is_map(map), do: dig(Map.get(map, key), rest)
  defp dig(_other, _path), do: nil
end
