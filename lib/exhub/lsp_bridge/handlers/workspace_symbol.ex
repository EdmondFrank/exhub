defmodule Exhub.LspBridge.Handlers.WorkspaceSymbol do
  @moduledoc "`workspace/symbol` — port of `core/handler/workspace_symbol.py`."

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "workspace-symbol"

  @impl true
  def method, do: "workspace/symbol"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "workspace_symbol"

  @impl true
  def request_params(args, _ctx) do
    query = args |> Map.get("query", "") |> to_string() |> String.split() |> Enum.join()
    %{"query" => query}
  end

  @impl true
  def process_response([], _ctx), do: {:message, "No symbols found."}

  def process_response(result, ctx) when is_list(result) do
    query = ctx.args |> Map.get("query", "") |> to_string()
    {:workspace_symbols, query, result}
  end

  def process_response(_result, _ctx), do: {:message, "No symbols found."}
end
