defmodule Exhub.LspBridge.Diagnostics do
  @moduledoc """
  Diagnostics aggregation — push (`textDocument/publishDiagnostics`) and pull
  (`textDocument/diagnostic`).

  Elixir port of `core/fileaction.py::record_diagnostics` / `get_diagnostics`
  and `core/handler/diagnostic.py`. Diagnostics are cached per language server
  on each `Exhub.LspBridge.Document`; `merge/2` flattens the per-server caches
  into the single list Emacs renders, tagging each entry with its originating
  server (lsp-bridge's `server-name`) and honoring the max/hide-severity knobs.
  """

  alias Exhub.LspBridge.Document

  @default_max 100

  @doc "Store one server's diagnostics for a document, sorted by range."
  @spec record(Document.t(), String.t(), [map()]) :: Document.t()
  def record(%Document{} = d, server, diagnostics) when is_list(diagnostics) do
    %{d | diagnostics: Map.put(d.diagnostics, server, sort(diagnostics))}
  end

  @doc "Total number of cached diagnostics across all servers."
  @spec count(Document.t()) :: non_neg_integer()
  def count(%Document{diagnostics: diagnostics}) do
    Enum.reduce(diagnostics, 0, fn {_server, list}, acc -> acc + length(list) end)
  end

  @doc """
  All cached diagnostics for a document, flattened across servers.

  Unlike `merge/2` this applies no max/severity filtering and adds no
  `server-name` tag — callers (e.g. `codeAction`'s `context.diagnostics`) only
  need the raw ranges.
  """
  @spec all(Document.t()) :: [map()]
  def all(%Document{diagnostics: diagnostics}) do
    diagnostics |> Map.values() |> List.flatten()
  end

  @doc """
  Flatten a document's per-server diagnostics into one list.

  Options: `:max` (default #{@default_max}) and `:hide_severities` (a list of
  LSP severity integers to drop; 1=error, 2=warning, 3=information, 4=hint).
  """
  @spec merge(Document.t(), keyword()) :: [map()]
  def merge(%Document{} = d, opts \\ []) do
    hide = MapSet.new(Keyword.get(opts, :hide_severities) || [])
    max = Keyword.get(opts, :max, @default_max)

    d.diagnostics
    |> Enum.flat_map(fn {server, list} ->
      Enum.map(list, &Map.put(&1, "server-name", server))
    end)
    |> Enum.reject(&MapSet.member?(hide, Map.get(&1, "severity", 1)))
    |> sort()
    |> Enum.take(max)
  end

  @doc "Sort diagnostics by start, then end, position."
  @spec sort([map()]) :: [map()]
  def sort(diagnostics) do
    Enum.sort_by(diagnostics, fn diagnostic ->
      range = Map.get(diagnostic, "range", %{})
      start = Map.get(range, "start", %{})
      end_ = Map.get(range, "end", %{})

      {
        Map.get(start, "line", 0),
        Map.get(start, "character", 0),
        Map.get(end_, "line", 0),
        Map.get(end_, "character", 0)
      }
    end)
  end

  @doc "Params for a pull-diagnostic `textDocument/diagnostic` request."
  @spec pull_params(String.t() | nil, String.t() | nil) :: map()
  def pull_params(identifier, previous_result_id) do
    %{}
    |> maybe_put("identifier", identifier)
    |> maybe_put("previousResultId", previous_result_id)
  end

  @doc """
  Extract items from a `textDocument/diagnostic` result.

  Returns `nil` for an `unchanged` result (nothing new to render).
  """
  @spec from_pull_result(map() | nil) :: [map()] | nil
  def from_pull_result(%{"items" => items}) when is_list(items), do: items
  def from_pull_result(_other), do: nil

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)
end
