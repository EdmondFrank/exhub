defmodule Exhub.LspBridge.Handlers.CallHierarchyIncoming do
  @moduledoc """
  `callHierarchy/incomingCalls` — port of lsp-bridge's `CallHierarchyIncomingCalls`.

  The `item` is the `CallHierarchyItem` returned by
  `textDocument/prepareCallHierarchy`, passed back unchanged. The result is the
  raw `CallHierarchyIncomingCall[]` (`{from, fromRanges}`); the elisp front end
  extracts the `from` items.
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "call-hierarchy-incoming"

  @impl true
  def method, do: "callHierarchy/incomingCalls"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "call_hierarchy"

  @impl true
  def request_params(%{"item" => item}, _ctx), do: %{"item" => item}
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(calls, ctx) when is_list(calls) and calls != [] do
    {:call_hierarchy, ctx.path, "incoming", calls}
  end

  def process_response(_other, _ctx), do: {:message, "No incoming calls."}
end
