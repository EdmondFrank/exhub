defmodule Exhub.LspBridge.MultiServer do
  @moduledoc """
  Multi-server profile resolution — the Elixir port of lsp-bridge's
  `get_method_server_names` / `pick_multi_server_names`.

  A multi-server profile (a vendored `multiserver/*.json` file) maps a feature
  to an ordered list of language-server names, with an optional `"default"` used
  for any feature without an explicit entry. Values may be a single string or a
  list:

      {
        "default":     "pyright",
        "diagnostics": ["pyright", "ruff"],
        "formatting":  "ruff"
      }

  lsp-bridge keys these by the *handler name* it exposes to Emacs
  (`"completion"`, `"find_define"`, `"code_action"`, …). `servers/2` accepts
  either that spelling or the raw LSP method string and normalizes via
  `@aliases`.
  """

  alias Exhub.LspBridge.Config

  @doc """
  Ordered server names to use for `method` in profile `name`.

  Falls back to the profile's `"default"` when the method has no entry, and
  returns `[]` when the profile is unknown or resolves nothing.
  """
  @spec servers(String.t(), String.t()) :: [String.t()]
  def servers(name, method) when is_binary(name) and is_binary(method) do
    case Config.multi(name) do
      {:ok, profile} -> pick(profile, method)
      {:error, :not_found} -> []
    end
  end

  @doc "All server names referenced by a profile, de-duplicated."
  @spec all_servers(map()) :: [String.t()]
  def all_servers(profile) when is_map(profile) do
    profile
    |> Map.values()
    |> Enum.flat_map(&List.wrap/1)
    |> Enum.filter(&is_binary/1)
    |> Enum.uniq()
  end

  defp pick(profile, method) do
    case Enum.find_value([method, handler_alias(method)], &Map.get(profile, &1)) do
      nil -> normalize(Map.get(profile, "default"))
      names -> normalize(names)
    end
  end

  defp normalize(nil), do: []
  defp normalize(names), do: names |> List.wrap() |> Enum.filter(&is_binary/1)

  # Map common raw LSP methods onto lsp-bridge's multiserver handler keys so
  # callers can pass either spelling.
  @aliases %{
    "textDocument/completion" => "completion",
    "completionItem/resolve" => "completion_item_resolve",
    "textDocument/hover" => "hover",
    "textDocument/definition" => "find_define",
    "textDocument/typeDefinition" => "find_type_define",
    "textDocument/implementation" => "find_implementation",
    "textDocument/references" => "find_references",
    "textDocument/signatureHelp" => "signature_help",
    "textDocument/prepareRename" => "prepare_rename",
    "textDocument/rename" => "rename",
    "textDocument/codeAction" => "code_action",
    "textDocument/formatting" => "formatting",
    "textDocument/rangeFormatting" => "range_formatting",
    "textDocument/documentSymbol" => "document_symbol",
    "workspace/symbol" => "workspace_symbol",
    "textDocument/inlayHint" => "inlay_hint",
    "textDocument/semanticTokens/full" => "semantic_tokens",
    "textDocument/prepareCallHierarchy" => "call_hierarchy",
    "callHierarchy/incomingCalls" => "call_hierarchy",
    "callHierarchy/outgoingCalls" => "call_hierarchy",
    "textDocument/diagnostic" => "diagnostics"
  }

  defp handler_alias(method), do: Map.get(@aliases, method, method)
end
