defmodule Exhub.LspBridge.Handlers.Implementation do
  @moduledoc "`textDocument/implementation` — port of `core/handler/find_implementation.py`."

  @behaviour Exhub.LspBridge.Handler

  alias Exhub.LspBridge.Handlers.Locations

  @impl true
  def name, do: "find-implementation"

  @impl true
  def method, do: "textDocument/implementation"

  @impl true
  def cancel_on_change?, do: true

  @impl true
  def provider, do: "implementation"

  @impl true
  def request_params(%{"position" => position}, _ctx), do: %{"position" => position}
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(result, ctx) do
    case Locations.normalize(result) do
      [] -> {:message, "No implementation found."}
      locations -> {:locations, ctx.path, "implementation", locations}
    end
  end
end
