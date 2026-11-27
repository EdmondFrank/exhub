defmodule Exhub.MCP.Tools.Lsp.Symbols do
  @moduledoc """
  MCP Tool: `lsp_symbols` — outline of one file, or workspace symbol search.

  With only `file_path` it returns the file's document symbols (an outline).
  With `query` as well it runs `workspace/symbol` from that file's project, so
  you can find a definition by name without knowing the file.
  """

  alias Exhub.LspBridge.Headless
  alias Exhub.MCP.Tools.Lsp.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "lsp_symbols"

  @impl true
  def description do
    """
    Symbols from the language server of a file in any project on this machine.

    - `file_path` only → the file's outline (`textDocument/documentSymbol`):
      functions, types, fields, with their 1-based positions.
    - `file_path` + `query` → workspace symbol search
      (`workspace/symbol`) rooted at that file's project: find a definition by
      name without knowing which file holds it.

    Positions in the output are 1-based, so they can be fed straight back into
    `lsp_definition`/`lsp_references`/`lsp_hover`.
    """
  end

  schema do
    field(:file_path, {:required, :string},
      description: "Path to a file in the project (anchor for the session)."
    )

    field(:query, :string,
      description: "Workspace symbol query; omit for the file's own outline."
    )

    field(:max, :integer, description: "Maximum symbols to report (default 400).", default: 400)

    field(:server, :string, description: "Pin a language server by name.")

    field(:project_path, :string, description: "Authoritative project root override.")
  end

  @impl true
  def execute(params, frame) do
    case Map.get(params, :file_path) do
      file_path when is_binary(file_path) and file_path != "" ->
        opts = Helpers.opts(params)
        max = Map.get(params, :max) || 400

        case Map.get(params, :query) do
          query when is_binary(query) and query != "" ->
            workspace_symbols(frame, file_path, query, opts, max)

          _ ->
            document_symbols(frame, file_path, opts, max)
        end

      _ ->
        Helpers.error(frame, "`file_path` is required (it anchors the session).")
    end
  end

  defp document_symbols(frame, file_path, opts, max) do
    case Headless.request(file_path, "textDocument/documentSymbol", %{}, opts) do
      {:ok, result} ->
        case first_list(result.results) do
          [] ->
            Helpers.ok(
              frame,
              Helpers.append_failures("No symbols reported for this file.", result)
            )

          symbols ->
            body =
              symbols
              |> Enum.flat_map(&flatten/1)
              |> Enum.take(max)
              |> Enum.map_join("\n", &format_outline/1)

            Helpers.ok(frame, Helpers.append_failures(body, result))
        end

      {:error, reason} ->
        Helpers.error(frame, "lsp_symbols failed: #{inspect(reason)}")
    end
  end

  defp workspace_symbols(frame, file_path, query, opts, max) do
    case Headless.request(file_path, "workspace/symbol", %{"query" => query}, opts) do
      {:ok, result} ->
        case first_list(result.results) do
          [] ->
            Helpers.ok(
              frame,
              Helpers.append_failures("No workspace symbol matches #{inspect(query)}.", result)
            )

          symbols ->
            body =
              symbols
              |> Enum.take(max)
              |> Enum.map_join("\n", &format_workspace_symbol/1)

            Helpers.ok(
              frame,
              Helpers.append_failures("Matches for #{inspect(query)}:\n" <> body, result)
            )
        end

      {:error, reason} ->
        Helpers.error(frame, "lsp_symbols failed: #{inspect(reason)}")
    end
  end

  defp first_list(results) do
    Enum.find_value(results, [], fn
      {_server, {:ok, list}} when is_list(list) and list != [] -> list
      _ -> nil
    end)
  end

  defp flatten(%{"location" => %{"range" => range}} = symbol) do
    [{0, symbol["name"], kind_name(symbol["kind"]), start_line(range), start_character(range)}]
  end

  defp flatten(%{"name" => name, "kind" => kind} = symbol) do
    range = symbol["selectionRange"] || symbol["range"] || %{}
    children = Map.get(symbol, "children") || []

    [{0, name, kind_name(kind), start_line(range), start_character(range)}] ++
      Enum.flat_map(children, &indent(&1, 1))
  end

  defp flatten(_other), do: []

  defp indent(symbol, depth) do
    symbol
    |> flatten()
    |> Enum.map(fn {_depth, name, kind, line, character} ->
      {depth, name, kind, line, character}
    end)
  end

  defp format_outline({depth, name, kind, line, character}) do
    "#{String.duplicate("  ", depth)}#{line}:#{character} #{kind} #{name}"
  end

  defp format_workspace_symbol(%{"location" => %{"uri" => uri, "range" => range}} = symbol) do
    file = Exhub.LspBridge.Document.path_from_uri(uri)

    "  #{shorten(file)}:#{start_line(range)}:#{start_character(range)} " <>
      "#{kind_name(symbol["kind"])} #{symbol["name"]}"
  end

  defp format_workspace_symbol(symbol), do: "  #{symbol["name"]}"

  defp start_line(%{"start" => start}), do: position(start["line"])
  defp start_line(_range), do: "?"

  defp start_character(%{"start" => start}), do: position(start["character"])
  defp start_character(_range), do: "?"

  defp position(nil), do: "?"
  defp position(value) when is_integer(value), do: value + 1

  defp shorten(path), do: String.replace_prefix(Path.expand(path), System.user_home!(), "~")

  @kinds %{
    1 => "file",
    2 => "module",
    3 => "namespace",
    4 => "package",
    5 => "class",
    6 => "method",
    7 => "property",
    8 => "field",
    9 => "constructor",
    10 => "enum",
    11 => "interface",
    12 => "function",
    13 => "variable",
    14 => "constant",
    15 => "string",
    16 => "number",
    17 => "boolean",
    18 => "array",
    19 => "object",
    20 => "key",
    21 => "null",
    22 => "enum-member",
    23 => "struct",
    24 => "event",
    25 => "operator",
    26 => "type-parameter"
  }

  defp kind_name(kind), do: Map.get(@kinds, kind, "symbol")
end
