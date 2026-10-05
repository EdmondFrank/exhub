defmodule Exhub.LspBridge.Handlers.CallHierarchyOutgoing do
  @moduledoc """
  `callHierarchy/outgoingCalls` — port of lsp-bridge's `CallHierarchyOutgoingCalls`.

  The `item` is the `CallHierarchyItem` returned by
  `textDocument/prepareCallHierarchy`, passed back unchanged. The result is the
  raw `CallHierarchyOutgoingCall[]` (`{to, fromRanges}`); the elisp front end
  extracts the `to` items.
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "call-hierarchy-outgoing"

  @impl true
  def method, do: "callHierarchy/outgoingCalls"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "call_hierarchy"

  @impl true
  def request_params(%{"item" => item}, _ctx), do: %{"item" => item}
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(calls, ctx) when is_list(calls) and calls != [] do
    {:call_hierarchy, ctx.path, "outgoing", calls}
  end

  def process_response(_other, _ctx), do: {:message, "No outgoing calls."}
end
