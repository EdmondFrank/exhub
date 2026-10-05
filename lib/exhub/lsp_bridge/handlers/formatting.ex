defmodule Exhub.LspBridge.Handlers.Formatting do
  @moduledoc """
  `textDocument/formatting` — port of `core/handler/formatting.py`.

  The server returns a list of `TextEdit`; the elisp front end applies them to
  the buffer (descending). An empty list means nothing needed formatting.
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "format"

  @impl true
  def method, do: "textDocument/formatting"

  @impl true
  def cancel_on_change?, do: true

  @impl true
  def provider, do: "formatting"

  @impl true
  def request_params(args, _ctx), do: %{"options" => options(args)}

  @impl true
  def process_response(edits, ctx) when is_list(edits) and edits != [] do
    {:format, ctx.path, edits}
  end

  def process_response(_other, _ctx), do: {:message, "Nothing to format."}

  @doc "LSP `FormattingOptions` from the command arguments (with lsp-bridge's defaults)."
  @spec options(map()) :: map()
  def options(args) do
    %{
      "tabSize" => Map.get(args, "tabSize", 4),
      "insertSpaces" => Map.get(args, "insertSpaces", true),
      "trimTrailingWhitespace" => true,
      "insertFinalNewline" => false,
      "trimFinalNewlines" => true
    }
  end
end
