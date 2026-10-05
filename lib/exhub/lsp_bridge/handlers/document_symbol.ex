defmodule Exhub.LspBridge.Handlers.DocumentSymbol do
  @moduledoc "`textDocument/documentSymbol` — port of `core/handler/document_symbol.py`."

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "document-symbol"

  @impl true
  def method, do: "textDocument/documentSymbol"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "document_symbol"

  @impl true
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response([], _ctx), do: {:message, "No symbols found."}

  def process_response(result, ctx) when is_list(result) do
    {:symbols, ctx.path, result}
  end

  def process_response(_result, _ctx), do: {:message, "No symbols found."}
end
