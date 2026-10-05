defmodule Exhub.LspBridge.Handlers.SignatureHelp do
  @moduledoc "`textDocument/signatureHelp` — port of `core/handler/signature_help.py`."

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "signature-help"

  @impl true
  def method, do: "textDocument/signatureHelp"

  @impl true
  def cancel_on_change?, do: true

  @impl true
  def provider, do: "signature_help"

  @impl true
  def request_params(%{"position" => position}, _ctx), do: %{"position" => position}
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(%{"signatures" => [_ | _]} = result, ctx) do
    {:signature_help, ctx.path, result}
  end

  def process_response(_result, _ctx), do: {:message, "No signature help available."}
end
