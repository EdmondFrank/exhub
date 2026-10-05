defmodule Exhub.LspBridge.Handler do
  @moduledoc """
  Behaviour for one read-only LSP feature.

  Elixir port of lsp-bridge's `core/handler/__init__.py::Handler`. A handler
  is a *pure* module (no process state): it knows the LSP `method/0` it
  implements, the capability `provider/0` that gates it, whether a response
  must be discarded when the document changed since the request
  (`cancel_on_change?/0`), how to build the request params from the decoded
  command arguments (`request_params/2`), and how to turn the raw LSP result
  into a payload (`process_response/2`).

  Unlike the Python original, `process_response/2` does not call
  `eval_in_emacs` directly: it returns a plain term and the transport layer
  (`Exhub.LspBridge.ClientManager`) renders it as an elisp form, so feature
  logic is testable without Emacs.

  Handlers are registered in `Exhub.LspBridge.Handlers` and dispatched by
  `Exhub.LspBridge.Session.perform/4`.
  """

  @type args :: map()
  @type ctx :: %{path: String.t(), language_id: String.t(), args: args()}

  @typedoc "A normalised location: filesystem path plus LSP range."
  @type location :: map()

  @typedoc """
  A renderable result. `{:message, text}` and `{:error, text}` are
  informational; the rest map to feature callbacks in the elisp front end.
  `nil` pushes nothing.
  """
  @type payload ::
          {:hover, String.t(), String.t()}
          | {:locations, String.t(), String.t(), [location()]}
          | {:symbols, String.t(), term()}
          | {:workspace_symbols, String.t(), term()}
          | {:signature_help, String.t(), term()}
          | {:completion, String.t(), String.t(), [map()], map(), map()}
          | {:completion_doc, String.t(), String.t(), String.t(), String.t(), [map()]}
          | {:rename_range, String.t(), map()}
          | {:workspace_edit, map(), String.t()}
          | {:format, String.t(), [map()]}
          | {:code_actions, String.t(), [map()]}
          | {:call_hierarchy, String.t(), String.t(), [map()]}
          | {:call_hierarchy_items, String.t(), [map()]}
          | {:inlay_hints, String.t(), [map()]}
          | {:semantic_tokens, String.t(), [map()]}
          | {:message, String.t()}
          | {:error, String.t()}
          | nil

  @doc "Command string Emacs uses (kebab-case), e.g. \"find-define\"."
  @callback name() :: String.t()

  @doc "LSP method, e.g. \"textDocument/definition\"."
  @callback method() :: String.t()

  @doc "Discard a response when the document changed since the request was sent."
  @callback cancel_on_change?() :: boolean()

  @doc "`Exhub.LspBridge.Capabilities` provider key gating this feature, or `nil` to skip capability gating."
  @callback provider() :: String.t() | nil

  @doc "LSP request params built from the decoded command arguments."
  @callback request_params(args(), ctx()) :: map()

  @doc "Turn the raw LSP result into a renderable payload."
  @callback process_response(result :: term(), ctx()) :: payload()
end
