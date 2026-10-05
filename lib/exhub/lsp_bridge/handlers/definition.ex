defmodule Exhub.LspBridge.Handlers.Definition do
  @moduledoc "`textDocument/definition` — port of `core/handler/find_define.py`."

  @behaviour Exhub.LspBridge.Handler

  alias Exhub.LspBridge.Handlers.Locations

  @impl true
  def name, do: "find-define"

  @impl true
  def method, do: "textDocument/definition"

  @impl true
  def cancel_on_change?, do: true

  @impl true
  def provider, do: "definition"

  @impl true
  def request_params(%{"position" => position}, _ctx), do: %{"position" => position}
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(result, ctx) do
    case Locations.normalize(result) do
      [] -> {:message, "No definition found."}
      locations -> {:locations, ctx.path, "definition", locations}
    end
  end
end
