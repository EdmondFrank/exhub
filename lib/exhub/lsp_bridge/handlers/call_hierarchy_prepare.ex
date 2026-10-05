defmodule Exhub.LspBridge.Handlers.CallHierarchyPrepare do
  @moduledoc """
  `textDocument/prepareCallHierarchy` — the Elixir port of lsp-bridge's
  `PrepareCallHierarchy` (`core/handler/call_hierarchy.py`).

  Returns the `CallHierarchyItem`s at a position (usually one). The elisp front
  end prompts for one and sends it back verbatim to
  `CallHierarchy/incomingCalls` or `CallHierarchy/outgoingCalls` (the item can
  carry a server-private `data` field, so it must be round-tripped unmodified).
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "call-hierarchy-prepare"

  @impl true
  def method, do: "textDocument/prepareCallHierarchy"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "call_hierarchy"

  @impl true
  def request_params(%{"position" => position}, _ctx), do: %{"position" => position}
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(items, ctx) when is_list(items) and items != [] do
    {:call_hierarchy_items, ctx.path, items}
  end

  def process_response(_other, _ctx), do: {:message, "No call hierarchy items."}
end
