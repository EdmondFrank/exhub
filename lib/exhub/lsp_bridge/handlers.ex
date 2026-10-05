defmodule Exhub.LspBridge.Handlers do
  @moduledoc """
  Compile-time registry of the read-only feature handlers.

  Elixir counterpart of lsp-bridge's `Handler.__subclasses__()` discovery in
  `core/handler/__init__.py`, but explicit: the list is declared here, so
  lookup is a plain scan and a hot reload sees the current set. Each module
  implements `Exhub.LspBridge.Handler`.
  """

  @handlers [
    Exhub.LspBridge.Handlers.Hover,
    Exhub.LspBridge.Handlers.Definition,
    Exhub.LspBridge.Handlers.TypeDefinition,
    Exhub.LspBridge.Handlers.Implementation,
    Exhub.LspBridge.Handlers.References,
    Exhub.LspBridge.Handlers.DocumentSymbol,
    Exhub.LspBridge.Handlers.WorkspaceSymbol,
    Exhub.LspBridge.Handlers.SignatureHelp,
    Exhub.LspBridge.Handlers.Completion,
    Exhub.LspBridge.Handlers.CompletionItem,
    Exhub.LspBridge.Handlers.PrepareRename,
    Exhub.LspBridge.Handlers.Rename,
    Exhub.LspBridge.Handlers.Formatting,
    Exhub.LspBridge.Handlers.RangeFormatting,
    Exhub.LspBridge.Handlers.CodeAction,
    Exhub.LspBridge.Handlers.ExecuteCommand,
    Exhub.LspBridge.Handlers.CallHierarchyPrepare,
    Exhub.LspBridge.Handlers.CallHierarchyIncoming,
    Exhub.LspBridge.Handlers.CallHierarchyOutgoing,
    Exhub.LspBridge.Handlers.InlayHint,
    Exhub.LspBridge.Handlers.SemanticTokens
  ]

  @doc "Every registered handler module."
  @spec all() :: [module()]
  def all, do: @handlers

  @doc "Find a handler by its command string, or `nil`."
  @spec fetch(String.t()) :: module() | nil
  def fetch(command) when is_binary(command) do
    Enum.find(@handlers, fn mod -> mod.name() == command end)
  end

  @doc "The command strings every registered handler answers to."
  @spec commands() :: [String.t()]
  def commands, do: Enum.map(@handlers, & &1.name())
end
