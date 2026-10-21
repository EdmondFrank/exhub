defmodule Exhub.Lua.Help do
  @moduledoc """
  On-demand tool documentation for the Lua sandbox — the
  [`Legion.Tools.Help`](https://legion.hexdocs.pm/Legion.Tools.Help.html) analog.

  Renders the *same* tool list the bridge exposes, either as a compact index
  (`render/2` with a blank path) or as one tool's full documentation. Like
  Legion's `Legion.Tools.Help`, the content is reachable twice:

    * as the MCP tool `help` (`Exhub.MCP.Tools.Lua.Help`) — so an agent can read
      docs without executing code; and
    * as the in-sandbox `help` global (`Exhub.Lua.Bridge`) — so a running snippet
      can read docs without spending a discovery round trip.

  Tool entries are the hub-style maps the bridge already carries:
  `%{"server" => …, "name" => …, "description" => …, "inputSchema" => …}`. Both
  `inputSchema` and `input_schema` are accepted, so the same module documents
  tools sourced from the hub or from the search index. Every function is pure —
  nothing here touches the hub.
  """

  # Keep one index line scannable; the full description is available per tool.
  @line_chars 160

  @doc "Compact index — one line per tool, sorted by `server.tool`."
  @spec index([map()]) :: String.t()
  def index(tools) do
    case entries(tools) do
      [] ->
        "No tools are visible."

      entries ->
        entries
        |> Enum.map_join("\n", fn {full, tool} -> "#{full} - #{summary(tool)}" end)
        |> Kernel.<>("\n\nCall help(\"server.tool\") for one tool's parameters.")
    end
  end

  @doc "Full documentation for one tool; the index when `path` is blank."
  @spec render([map()], String.t() | nil) :: String.t()
  def render(tools, path) when path in [nil, ""], do: index(tools)

  def render(tools, path) when is_binary(path) do
    case resolve(tools, path) do
      {:ok, {full, tool}} -> document(full, tool)
      {:error, message} -> message
    end
  end

  def render(tools, _path), do: index(tools)

  @doc """
  Resolve `path` — `server.tool`, `server__tool`, or a bare tool name — against
  `tools`. A bare name matching more than one tool is reported as ambiguous
  rather than silently picking one.
  """
  @spec resolve([map()], term()) :: {:ok, {String.t(), map()}} | {:error, String.t()}
  def resolve(tools, path) when is_binary(path) do
    trimmed =
      path
      |> String.trim()
      |> String.replace_prefix("tools.", "")
      |> String.replace_prefix("tools/", "")

    dotted = String.replace(trimmed, "__", ".")
    entries = entries(tools)

    case Enum.filter(entries, fn {full, _tool} -> full == dotted end) do
      [exact] -> {:ok, exact}
      [] -> resolve_bare(entries, path, trimmed)
    end
  end

  def resolve(_tools, path), do: {:error, unknown(path)}

  defp resolve_bare(entries, path, name) do
    case Enum.filter(entries, fn {_full, tool} -> Map.get(tool, "name") == name end) do
      [one] -> {:ok, one}
      [] -> {:error, unknown(path)}
      many -> {:error, ambiguous(name, many)}
    end
  end

  defp unknown(path) do
    "UnknownTool: #{inspect(path)} is not a visible tool. " <>
      "Call help() for the full index, or retrieve_tools({query = \"...\"}) to search."
  end

  defp ambiguous(name, many) do
    names = many |> Enum.map(fn {full, _tool} -> full end) |> Enum.sort() |> Enum.join(", ")
    "AmbiguousTool: #{inspect(name)} matches #{names}. Use the full server.tool name."
  end

  # --- Rendering ---

  defp document(full, tool) do
    [server, name] = String.split(full, ".", parts: 2)

    [
      "# #{full}",
      prose(Map.get(tool, "description")),
      parameters(tool),
      "Call: #{sanitize(server)}.#{sanitize(name)}({...}) or " <>
        "tools[\"#{server}__#{name}\"]({...})"
    ]
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n\n")
  end

  defp parameters(tool) do
    schema = Map.get(tool, "inputSchema") || Map.get(tool, "input_schema")

    properties = if is_map(schema), do: Map.get(schema, "properties"), else: nil
    required = if is_map(schema), do: Map.get(schema, "required") || [], else: []

    lines =
      case properties do
        map when is_map(map) and map_size(map) > 0 ->
          map
          |> Enum.sort_by(fn {name, _spec} -> name end)
          |> Enum.map(fn {name, spec} -> "  " <> param_line(name, spec, required) end)

        _ ->
          []
      end

    case lines do
      [] -> nil
      lines -> "Parameters:\n" <> Enum.join(lines, "\n")
    end
  end

  defp param_line(name, spec, required) do
    type = if is_map(spec), do: Map.get(spec, "type") || "any", else: "any"
    marker = if name in required, do: " (required)", else: ""

    suffix =
      case is_map(spec) && summary_text(Map.get(spec, "description")) do
        text when is_binary(text) and text != "" -> " - " <> text
        _ -> ""
      end

    "#{name}: #{type}#{marker}#{suffix}"
  end

  defp summary(tool) do
    case summary_text(Map.get(tool, "description")) do
      "" -> "(no description)"
      text -> text
    end
  end

  # First non-empty line of a description, trimmed and length-capped.
  defp summary_text(description) when is_binary(description) do
    description
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.find("", &(&1 != ""))
    |> String.slice(0, @line_chars)
  end

  defp summary_text(_description), do: ""

  defp prose(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      text -> text
    end
  end

  defp prose(_value), do: nil

  # --- Names ---

  # Sorted, deduplicated `{full_name, tool}` pairs; entries without a server or
  # name are skipped rather than rendered as a malformed name.
  defp entries(tools) when is_list(tools) do
    tools
    |> Enum.flat_map(fn tool ->
      case {name_of(tool, "server"), name_of(tool, "name")} do
        {server, name} when is_binary(server) and is_binary(name) -> [{"#{server}.#{name}", tool}]
        _ -> []
      end
    end)
    |> Enum.uniq_by(&elem(&1, 0))
    |> Enum.sort_by(&elem(&1, 0))
  end

  defp entries(_tools), do: []

  defp name_of(tool, key) when is_map(tool) do
    case Map.get(tool, key) do
      value when is_binary(value) and value != "" -> value
      _ -> nil
    end
  end

  defp name_of(_tool, _key), do: nil

  # Matches the bridge's nested-name rule so the call hint is always valid.
  defp sanitize(name) do
    name
    |> to_string()
    |> String.replace(~r/[^A-Za-z0-9_]/, "_")
  end
end
