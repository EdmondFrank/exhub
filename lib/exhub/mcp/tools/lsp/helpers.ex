defmodule Exhub.MCP.Tools.Lsp.Helpers do
  @moduledoc """
  Shared shaping for the `lsp_*` MCP tools.

  The tools are thin: they translate MCP parameters (1-based, human-facing
  positions) into LSP requests (0-based), hand them to
  `Exhub.LspBridge.Headless`, and render the result as compact text an agent
  can read without a second lookup.
  """

  alias Anubis.Server.Response
  alias Exhub.LspBridge.Headless

  @severity_names %{"error" => 1, "warning" => 2, "info" => 3, "information" => 3, "hint" => 4}

  @doc "Successful tool reply with plain text."
  def ok(frame, text), do: {:reply, Response.tool() |> Response.text(text), frame}

  @doc "Tool reply marked as an error (transport-level success, semantic failure)."
  def error(frame, message), do: {:reply, Response.tool() |> Response.error(message), frame}

  @doc """
  Convert the tool's 1-based `line`/`character` into an LSP position.
  """
  def position(params) do
    line = Map.get(params, :line)
    character = Map.get(params, :character)

    if is_integer(line) and is_integer(character) and line >= 1 and character >= 1 do
      {:ok, %{"line" => line - 1, "character" => character - 1}}
    else
      {:error,
       "`line` and `character` are required and 1-based (the first character of a file is 1:1)."}
    end
  end

  @doc "Headless options from the common tool parameters."
  def opts(params) do
    %{}
    |> put(:server, Map.get(params, :server))
    |> put(:multi, Map.get(params, :multi))
    |> put(:language_id, Map.get(params, :language_id))
    |> put(:project_path, Map.get(params, :project_path))
    |> put(:timeout_ms, Map.get(params, :timeout_ms))
    |> put(:wait_ms, Map.get(params, :wait_ms))
    |> put(:refresh, Map.get(params, :refresh))
  end

  @doc "LSP severity number for a tool's severity name (`\"all\"` → nil)."
  def severity(nil), do: 1

  def severity(name) when is_binary(name) do
    case String.downcase(name) do
      "all" -> nil
      other -> Map.get(@severity_names, other, 1)
    end
  end

  @doc "Filter diagnostics by severity and cap them."
  def filter_diagnostics(diagnostics, severity, max) do
    diagnostics
    |> Enum.filter(fn diagnostic ->
      is_nil(severity) or Map.get(diagnostic, "severity", 1) <= severity
    end)
    |> Enum.take(max)
  end

  @doc "Render `path:line:character` lines for a request's locations."
  def format_locations(%{results: results}, limit \\ 200) do
    locations = Headless.locations(results)

    case locations do
      [] ->
        {:ok, nil, 0}

      locations ->
        rendered =
          locations
          |> Enum.take(limit)
          |> Enum.map(fn location ->
            "#{location["file"]}:#{location["line"]}:#{location["character"]}"
          end)
          |> Enum.join("\n")

        {:ok, rendered, length(locations)}
    end
  end

  @doc "Server-side failures from a request, as `server: reason` lines."
  def failures(%{results: results}) do
    results
    |> Enum.flat_map(fn
      {_server, {:ok, _}} -> []
      {server, {:error, reason}} -> ["#{server}: #{inspect(reason)}"]
    end)
  end

  @doc "Append the servers that could not answer, when any."
  def append_failures(text, result) do
    case failures(result) do
      [] -> text
      list -> text <> "\n\nUnavailable server(s): " <> Enum.join(list, "; ")
    end
  end

  defp put(map, _key, nil), do: map
  defp put(map, key, value), do: Map.put(map, key, value)
end
