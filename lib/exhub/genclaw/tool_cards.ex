defmodule Exhub.Genclaw.ToolCards do
  @moduledoc """
  Loads and renders GenClaw tool card YAMLs into description strings.

  Each tool card is a 7-section YAML file (what / when_to_use / when_not_to_use /
  usage / failure_modes / cross_tool / examples) that gets rendered into a
  Markdown description string passed to the LLM via `LangChain.Function`.
  """

  @card_dir "genclaw/tool_cards"

  @section_order [:what, :when_to_use, :when_not_to_use, :usage, :failure_modes, :cross_tool, :examples]

  @section_titles %{
    what: "## What",
    when_to_use: "## When to use",
    when_not_to_use: "## When NOT to use",
    usage: "## Usage notes",
    failure_modes: "## Failure modes",
    cross_tool: "## Cross-tool constraints",
    examples: "## Examples"
  }

  @doc """
  Render a single capability YAML into a description string.
  """
  def render_capability(yaml_path) do
    data = read_yaml!(yaml_path)

    parts = []

    one_liner = Map.get(data, "description", "")
    parts = if one_liner != "", do: parts ++ [one_liner], else: parts

    parts =
      Enum.reduce(@section_order, parts, fn key, acc ->
        body = Map.get(data, Atom.to_string(key)) || Map.get(data, to_string(key))

        case body do
          nil -> acc
          "" -> acc
          text when is_binary(text) ->
            acc ++ [@section_titles[key], String.trim(text)]
          list when is_list(list) ->
            items = Enum.map(list, fn item -> "- #{String.trim(to_string(item))}" end)
            acc ++ [@section_titles[key] | items]
          other ->
            acc ++ [@section_titles[key], to_string(other)]
        end
      end)

    parts |> Enum.join("\n\n") |> String.trim()
  end

  @doc """
  Return `{tool_name => rendered_description}` for all YAML cards.
  """
  def load_all_descriptions(dir \\ nil) do
    card_dir = dir || default_card_dir()

    card_dir
    |> list_yaml_files()
    |> Enum.map(fn path ->
      data = read_yaml!(path)
      name = Map.get(data, "name", Path.basename(path, ".yaml"))
      {name, render_capability(path)}
    end)
    |> Map.new()
  end

  @doc """
  Return `{tool_name => short_description}` for cards that define one.
  """
  def load_short_descriptions(dir \\ nil) do
    card_dir = dir || default_card_dir()

    card_dir
    |> list_yaml_files()
    |> Enum.map(fn path ->
      data = read_yaml!(path)
      name = Map.get(data, "name", Path.basename(path, ".yaml"))
      sd = Map.get(data, "short_description")
      {name, sd}
    end)
    |> Enum.filter(fn {_, sd} -> sd != nil and sd != "" end)
    |> Enum.map(fn {name, sd} -> {name, String.trim(to_string(sd))} end)
    |> Map.new()
  end

  @doc """
  Render a compact listing of all tools (name + one-liner) for the system prompt.
  """
  def render_tools_listing(dir \\ nil) do
    card_dir = dir || default_card_dir()

    card_dir
    |> list_yaml_files()
    |> Enum.map(fn path ->
      data = read_yaml!(path)
      name = Map.get(data, "name", Path.basename(path, ".yaml"))
      desc = data |> Map.get("description", "") |> String.split("\n") |> List.first() |> String.trim()
      "- `#{name}`: #{desc}"
    end)
    |> Enum.join("\n")
  end

  defp default_card_dir do
    :exhub
    |> :code.priv_dir()
    |> Path.join(@card_dir)
  end

  defp list_yaml_files(dir) do
    dir
    |> File.ls!()
    |> Enum.filter(&String.ends_with?(&1, ".yaml"))
    |> Enum.reject(&String.starts_with?(&1, "._"))
    |> Enum.map(&Path.join(dir, &1))
    |> Enum.sort()
  end

  defp read_yaml!(path) do
    path
    |> File.read!()
    |> parse_yaml!()
  end

  defp parse_yaml!(content) do
    case Code.ensure_loaded(:yamerl) do
      {:module, :yamerl} ->
        content
        |> :yamerl_constr.string()
        |> List.first()
        |> normalize_yaml()

      {:error, _} ->
        parse_simple_yaml(content)
    end
  end

  # Convert yamerl proplist output to a map with string keys.
  # Yamerl returns: mappings as proplists [{~c"key", value}],
  # strings as charlists, lists as Elixir lists.
  defp normalize_yaml(list) when is_list(list) do
    # Check if this is a proplist (list of 2-tuples)
    if is_proplist?(list) do
      list
      |> Enum.map(fn {k, v} -> {to_string(k), normalize_yaml(v)} end)
      |> Map.new()
    else
      # Try to interpret as a charlist (yamerl returns strings as charlists)
      case :unicode.characters_to_binary(list) do
        string when is_binary(string) ->
          string
        _ ->
          Enum.map(list, &normalize_yaml/1)
      end
    end
  end

  defp normalize_yaml(value) when is_binary(value), do: value
  defp normalize_yaml(value) when is_atom(value), do: Atom.to_string(value)
  defp normalize_yaml(value) when is_number(value), do: value
  defp normalize_yaml(value) when is_boolean(value), do: value
  defp normalize_yaml({k, v}), do: {to_string(k), normalize_yaml(v)}
  defp normalize_yaml(other), do: other

  defp is_proplist?([]), do: false
  defp is_proplist?(list) when is_list(list) do
    Enum.all?(list, fn
      {k, _} when is_list(k) -> true
      {k, _} when is_atom(k) -> true
      _ -> false
    end)
  end
  defp is_proplist?(_), do: false

  # Minimal YAML parser for the tool card format (key: value, lists with -)
  defp parse_simple_yaml(content) do
    lines = String.split(content, "\n")
    parse_yaml_lines(lines, %{}, nil)
  end

  defp parse_yaml_lines([], acc, _current_key), do: acc

  defp parse_yaml_lines([line | rest], acc, current_key) do
    trimmed = String.trim(line)

    cond do
      trimmed == "" or String.starts_with?(trimmed, "#") ->
        parse_yaml_lines(rest, acc, current_key)

      String.starts_with?(trimmed, "- ") ->
        # List item under current_key
        item = String.trim_leading(trimmed, "- ")
        item = String.trim(item, "\"")
        acc = Map.update(acc, current_key, [item], fn list -> list ++ [item] end)
        parse_yaml_lines(rest, acc, current_key)

      String.ends_with?(trimmed, ": |") or String.ends_with?(trimmed, ":|") ->
        # Block scalar (literal)
        key = trimmed |> String.split(":") |> List.first() |> String.trim()
        # Collect following indented lines
        {block, remaining} = collect_block(rest)
        parse_yaml_lines(remaining, Map.put(acc, key, block), key)

      String.ends_with?(trimmed, ":") ->
        # Key with no value on same line (could be list)
        key = String.trim_trailing(trimmed, ":")
        parse_yaml_lines(rest, Map.put(acc, key, []), key)

      String.contains?(trimmed, ":") ->
        # Key: value
        [key, value] = String.split(trimmed, ":", parts: 2)
        key = String.trim(key)
        value = value |> String.trim() |> String.trim("\"")
        parse_yaml_lines(rest, Map.put(acc, key, value), key)

      true ->
        # Continuation of a block scalar
        if current_key && Map.has_key?(acc, current_key) do
          existing = Map.get(acc, current_key)
          if is_binary(existing) do
            parse_yaml_lines(rest, Map.put(acc, current_key, existing <> "\n" <> trimmed), current_key)
          else
            parse_yaml_lines(rest, acc, current_key)
          end
        else
          parse_yaml_lines(rest, acc, current_key)
        end
    end
  end

  defp collect_block(lines) do
    {block_lines, rest} =
      Enum.split_while(lines, fn line ->
        String.starts_with?(line, " ") or String.starts_with?(line, "\t") or String.trim(line) == ""
      end)

    block = block_lines |> Enum.map(&String.trim/1) |> Enum.reject(&(&1 == "")) |> Enum.join("\n")
    {block, rest}
  end
end
