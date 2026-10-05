defmodule Exhub.LspBridge.Handlers.TypeDefinition do
  @moduledoc "`textDocument/typeDefinition` — port of `core/handler/find_type_define.py`."

  @behaviour Exhub.LspBridge.Handler

  alias Exhub.LspBridge.Handlers.Locations

  @impl true
  def name, do: "find-type-define"

  @impl true
  def method, do: "textDocument/typeDefinition"

  @impl true
  def cancel_on_change?, do: true

  @impl true
  def provider, do: "type_definition"

  @impl true
  def request_params(%{"position" => position}, _ctx), do: %{"position" => position}
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(result, ctx) do
    case Locations.normalize(result) do
      [] -> {:message, "No type definition found."}
      locations -> {:locations, ctx.path, "type-definition", locations}
    end
  end
end
