defmodule Exhub.LspBridge.Handlers.InlayHint do
  @moduledoc """
  `textDocument/inlayHint` — port of `core/handler/inlay_hint.py`.

  Returns the server's `InlayHint[]` for a range (the elisp front end requests
  the whole buffer on an idle timer after each change). `cancel_on_change?` is
  true so a hint response computed against an older revision is discarded when
  the document has since changed. An empty/absent result still emits an empty
  list so the front end clears stale overlays.
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "inlay-hint"

  @impl true
  def method, do: "textDocument/inlayHint"

  @impl true
  def cancel_on_change?, do: true

  @impl true
  def provider, do: "inlay_hint"

  @impl true
  def request_params(%{"range" => range}, _ctx), do: %{"range" => range}
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(hints, ctx) when is_list(hints) do
    {:inlay_hints, ctx.path, hints}
  end

  def process_response(_other, ctx), do: {:inlay_hints, ctx.path, []}
end
