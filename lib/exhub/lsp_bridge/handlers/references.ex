defmodule Exhub.LspBridge.Handlers.References do
  @moduledoc "`textDocument/references` — port of `core/handler/find_references.py`."

  @behaviour Exhub.LspBridge.Handler

  alias Exhub.LspBridge.Handlers.Locations

  @impl true
  def name, do: "find-references"

  @impl true
  def method, do: "textDocument/references"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "references"

  @impl true
  def request_params(%{"position" => position}, _ctx) do
    %{"position" => position, "context" => %{"includeDeclaration" => false}}
  end

  def request_params(_args, _ctx), do: %{"context" => %{"includeDeclaration" => false}}

  @impl true
  def process_response(result, ctx) do
    case Locations.normalize(result) do
      [] -> {:message, "No references found."}
      locations -> {:locations, ctx.path, "references", locations}
    end
  end
end
