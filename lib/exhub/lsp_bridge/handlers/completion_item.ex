defmodule Exhub.LspBridge.Handlers.CompletionItem do
  @moduledoc """
  `completionItem/resolve` — port of `core/handler/completion_item.py`.

  Emacs asks for the documentation (and any late `additionalTextEdits`) of a
  completion candidate chosen in the menu. The request params *are* the item
  itself, so no `textDocument` is attached; the response carries the
  documentation string and the edits back to the front end, which stores them
  on the cached candidate for the doc frame and auto-import.

  lsp-bridge's `send_document_uri = False` maps to `completionItem/resolve`
  not being a `textDocument/*` method, so `Session` never injects a document.
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "completion-item-resolve"

  @impl true
  def method, do: "completionItem/resolve"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "completion_resolve"

  @impl true
  def request_params(args, _ctx), do: Map.get(args, "item") || %{}

  @impl true
  def process_response(result, ctx) do
    {documentation, edits} = resolved(result)

    {:completion_doc, ctx.path, Map.get(ctx, :server) || "", Map.get(ctx.args || %{}, "key", ""),
     documentation, edits}
  end

  defp resolved(nil), do: {"", []}

  defp resolved(result) when is_map(result) do
    documentation =
      case result["documentation"] do
        %{"value" => value} -> to_string(value)
        value when is_binary(value) -> value
        _ -> ""
      end

    documentation = if String.trim(documentation) == "", do: detail(result), else: documentation
    {String.trim(documentation), List.wrap(result["additionalTextEdits"])}
  end

  defp resolved(_other), do: {"", []}

  defp detail(result) do
    case result["detail"] do
      value when is_binary(value) -> value
      _ -> ""
    end
  end
end
