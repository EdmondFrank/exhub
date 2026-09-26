defmodule Exhub.BrowserAgent.Snapshot do
  @moduledoc """
  Parses a `kuri-agent snap` page snapshot into a Jev-style indexed element
  table and action space.

  The library form of [browser-use/jev-ultrafast](https://github.com/browser-use/jev-ultrafast)
  observes a page, builds a flat table of numbered elements, asks a System One
  model to choose an *operation* plus a *target*, and executes only the chosen
  operation's target head. This module is the observation half: it turns kuri's
  accessibility snapshot into that table.

  ## Input

  `kuri-agent snap` emits its compact text tree by default:

      {depth}{role} "{name}" @{ref} = {value} [{state}] desc="{description}"

  where every field after `role` is optional. `parse/1` also accepts the
  legacy `--text` format (`[ref] role "name" value="..." state="..."`), a
  `--json` array of `%{"ref" =>, "role" =>, "name" =>}` maps, and the JSON
  envelope returned by `Exhub.MCP.Tools.BrowserUse.Helper`
  (`%{"stdout" => ...}`).

  kuri truncates `name`/`value`/`state` at a byte boundary, which can split a
  `\\uXXXX` escape and produce invalid JSON, so the text tree is the reliable
  source and escapes are decoded leniently.
  """

  @clickable_roles ~w(button link menuitem menuitemcheckbox menuitemradio tab option checkbox radio switch)
  @editable_roles ~w(textbox searchbox combobox spinbutton)

  # Grammatical filler plus the boilerplate of a "look something up" goal: every
  # docs page has a "documentation" link and a sidebar, so those words match too
  # much to be evidence. See `goal_terms/1`.
  @stopwords ~w(a an and are as at be by do docs documentation find for from go
                in is it its look of on open or page read report search see
                sidebar signature site the this to with)

  @typedoc "One parsed accessibility node."
  @type element :: %{
          index: pos_integer(),
          ref: String.t() | nil,
          role: String.t(),
          name: String.t() | nil,
          value: String.t() | nil,
          state: String.t() | nil,
          description: String.t() | nil,
          depth: non_neg_integer()
        }

  @typedoc "Answer ids offered to the operation question."
  @type targets :: %{optional(String.t()) => %{optional(String.t()) => map()}}

  @doc "Roles that can receive a CLICK operation."
  @spec clickable_roles() :: [String.t()]
  def clickable_roles, do: @clickable_roles

  @doc "Roles that can receive a TYPE_TEXT operation."
  @spec editable_roles() :: [String.t()]
  def editable_roles, do: @editable_roles

  @doc """
  Parses a snapshot payload into a flat list of accessibility nodes.

  Accepts the raw `kuri-agent` text, a JSON array/object, or the helper's
  `%{"stdout" => text}` envelope. Output nodes are not yet indexed; call
  `action_space/2` (or `index/1`) for the Jev-style numbering.
  """
  @spec parse(term()) :: [map()]
  def parse(%{"stdout" => stdout}), do: parse(stdout)

  def parse(payload) when is_binary(payload) do
    trimmed = String.trim(payload)

    cond do
      trimmed == "" -> []
      String.starts_with?(trimmed, "{") -> parse_stdout_or_text(trimmed)
      String.starts_with?(trimmed, "[") -> parse_json_array(trimmed) || parse_text(trimmed)
      true -> parse_text(payload)
    end
  end

  def parse(payload) when is_list(payload), do: Enum.map(payload, &normalize_node/1)
  def parse(_), do: []

  @doc """
  Numbers `elements` 1..n (Jev-style) and builds the per-operation target heads.

  Returns `{indexed_elements, targets}` where `targets` maps an operation name
  (`"CLICK"`, `"TYPE_TEXT"`) to a map of `index_string => target` for every
  element that supports it. Only elements carrying a `ref` are offered, since
  an index without a ref cannot be executed.

  ## Options

    * `:max_targets` — candidates offered per operation (default 16, System
      One's per-question limit).
    * `:offset` — skip this many candidates in document order before taking
      `:max_targets`. Callers page a long head with it: a docs sidebar's first
      sixteen links are boilerplate navigation, so the article's own link is
      only choosable once the window slides past it. See `candidate_ceiling/1`.
    * `:prefer` — goal terms (see `goal_terms/1`). Candidates whose label shares
      more of them are offered first, so the one element a goal names out of a
      hundred sidebar links lands inside the sixteen-candidate head instead of
      outside it. A no-op when the terms match nothing, and always a stable sort,
      so unmatched candidates keep document order.

  The rendered table still numbers every reffed element, so an index keeps its
  label whichever window it falls in.
  """
  @spec action_space([map()], keyword()) :: {[element()], targets()}
  def action_space(elements, opts \\ []) do
    # System One accepts only 2..16 candidates per `choice` question.
    max_targets = Keyword.get(opts, :max_targets, 16)
    offset = Keyword.get(opts, :offset, 0)
    prefer = Keyword.get(opts, :prefer, [])

    indexed =
      elements
      |> Enum.filter(&(&1[:ref] not in [nil, ""]))
      |> Enum.with_index(1)
      |> Enum.map(fn {element, index} -> build_element(element, index) end)

    targets =
      %{
        "CLICK" => targets_for(indexed, @clickable_roles, max_targets, offset, prefer),
        "TYPE_TEXT" => targets_for(indexed, @editable_roles, max_targets, offset, prefer)
      }
      |> Enum.reject(fn {_op, head} -> head == %{} end)
      |> Map.new()

    {indexed, targets}
  end

  @doc """
  The significant terms of a goal, for `action_space/2`'s `:prefer` option.

  Lowercased, split on non-word characters (keeping `_`, so a Ruby `before_action`
  survives), with short words and task boilerplate ("open", "documentation",
  "sidebar"…) dropped — they would match half a docs page and drown the signal.
  Returns `[]` when nothing significant remains, which makes ranking a no-op.
  """
  @spec goal_terms(String.t()) :: [String.t()]
  def goal_terms(goal) when is_binary(goal) do
    goal
    |> tokenize()
    |> Enum.reject(&(&1 in @stopwords))
  end

  def goal_terms(_goal), do: []

  @doc """
  Number of candidates the largest target head can be windowed over.

  A caller that pages the head with `:offset` can walk this far before every
  head is exhausted; past it there is nothing left to reveal.
  """
  @spec candidate_ceiling([element()]) :: non_neg_integer()
  def candidate_ceiling(elements) do
    max(
      Enum.count(elements, &(&1.role in @clickable_roles)),
      Enum.count(elements, &(&1.role in @editable_roles))
    )
  end

  @doc "Indexes elements without filtering (useful for state rendering)."
  @spec index([map()]) :: [element()]
  def index(elements) do
    elements
    |> Enum.with_index(1)
    |> Enum.map(fn {element, index} -> build_element(element, index) end)
  end

  @doc "Returns the operations a target head map makes available."
  @spec operations(targets()) :: [String.t()]
  def operations(targets), do: targets |> Map.keys() |> Enum.sort()

  @doc """
  Renders indexed elements as a compact numbered table for the model state.

  Mirrors the Jev observation, e.g. `[1] button "Change ticket type"`.
  """
  @spec render([element()]) :: String.t()
  def render(elements) do
    elements
    |> Enum.map(fn element ->
      label = element.name || element.value || "(unnamed)"
      value = if element.value && element.value != "", do: " = #{element.value}", else: ""
      state = if element.state && element.state != "", do: " [#{element.state}]", else: ""
      "[#{element.index}] #{element.role} \"#{label}\"#{value}#{state}"
    end)
    |> Enum.join("\n")
  end

  # --- text parsing ---

  defp parse_stdout_or_text(binary) do
    case Jason.decode(binary) do
      {:ok, %{"stdout" => stdout}} -> parse(stdout)
      {:ok, %{} = decoded} -> parse(decoded["elements"] || [])
      {:ok, list} when is_list(list) -> Enum.map(list, &normalize_node/1)
      _ -> parse_text(binary)
    end
  end

  defp parse_json_array(binary) do
    case Jason.decode(binary) do
      {:ok, list} when is_list(list) -> Enum.map(list, &normalize_node/1)
      _ -> nil
    end
  end

  defp parse_text(binary) do
    binary
    |> String.split("\n")
    |> Enum.map(&String.trim_trailing/1)
    |> Enum.reject(&(String.trim(&1) == ""))
    |> Enum.map(&parse_line/1)
    |> Enum.reject(&is_nil/1)
  end

  defp parse_line(line) do
    depth = leading_spaces(line)
    rest = String.trim_leading(line)

    case String.split(rest, " ", parts: 2) do
      [role] ->
        %{role: role, name: nil, ref: nil, value: nil, state: nil, description: nil, depth: depth}

      [role, attrs] ->
        attrs |> parse_attrs() |> Map.merge(%{role: role, depth: depth})
    end
  end

  # Attributes always appear in the formatter's fixed order:
  # "name" @ref = value [state] desc="description"
  defp parse_attrs(attrs) do
    attrs = String.trim_leading(attrs)
    {name, attrs} = take(attrs, ~r/^"(.*?)"(?=\s|$)/s)
    {ref, attrs} = take(attrs, ~r/^@(\S+)/)
    {value, attrs} = take(attrs, ~r/^=\s*(.*?)(?=\s+\[|\s+desc=|$)/s)
    {state, attrs} = take(attrs, ~r/^\[(.*?)\]/s)
    {desc, _attrs} = take(attrs, ~r/^desc="(.*)"\s*$/s)

    %{
      name: decode(name),
      ref: ref,
      value: decode(value),
      state: state,
      description: decode(desc)
    }
  end

  defp take(binary, regex) do
    binary = String.trim_leading(binary)

    case Regex.run(regex, binary) do
      [full | captures] -> {List.first(captures), String.replace_prefix(binary, full, "")}
      nil -> {nil, binary}
    end
  end

  defp leading_spaces(line) do
    line
    |> String.graphemes()
    |> Enum.take_while(&(&1 == " "))
    |> length()
  end

  defp normalize_node(node) when is_map(node) do
    %{
      role: node["role"] || node[:role] || "unknown",
      name: node["name"] || node[:name],
      ref: node["ref"] || node[:ref],
      value: node["value"] || node[:value],
      state: node["state"] || node[:state],
      description: node["description"] || node[:description],
      depth: node["depth"] || node[:depth] || 0
    }
  end

  defp normalize_node(_), do: nil

  # kuri truncates names by bytes, which can split a `\uXXXX` escape. Decode the
  # valid ones and drop an incomplete trailing escape rather than fail.
  defp decode(nil), do: nil

  defp decode(binary) when is_binary(binary) do
    replaced =
      Regex.replace(~r/\\u([0-9a-fA-F]{4})/, binary, fn _, hex -> decode_codepoint(hex) end)

    String.replace(replaced, ~r/\\u[0-9a-fA-F]{0,3}$/, "")
  end

  defp decode_codepoint(hex) do
    codepoint = String.to_integer(hex, 16)

    if codepoint in 0xD800..0xDFFF do
      "\\u" <> hex
    else
      <<codepoint::utf8>>
    end
  end

  # --- action space ---

  defp build_element(element, index) do
    %{
      index: index,
      ref: element[:ref],
      role: element[:role],
      name: element[:name],
      value: element[:value],
      state: element[:state],
      description: element[:description],
      depth: element[:depth] || 0
    }
  end

  defp targets_for(elements, roles, max_targets, offset, prefer) do
    elements
    |> Enum.filter(&(&1.role in roles))
    |> rank_by_relevance(prefer)
    |> Enum.drop(offset)
    |> Enum.take(max_targets)
    |> Map.new(fn element -> {to_string(element.index), target(element)} end)
  end

  # Stable sort: the candidates sharing the most goal terms come first, and
  # everything else keeps document order (the tie-break index), so an unmatched
  # head behaves exactly as before.
  defp rank_by_relevance(elements, []), do: elements

  defp rank_by_relevance(elements, prefer) do
    elements
    |> Enum.with_index()
    |> Enum.sort_by(fn {element, position} -> {-relevance(element, prefer), position} end)
    |> Enum.map(fn {element, _position} -> element end)
  end

  defp relevance(element, prefer) do
    tokens = tokenize(element.name || element.value || "")
    Enum.count(prefer, &(&1 in tokens))
  end

  defp tokenize(text) do
    text
    |> to_string()
    |> String.downcase()
    |> String.split(~r/[^a-z0-9_]+/, trim: true)
  end

  defp target(element) do
    %{
      index: element.index,
      ref: element.ref,
      role: element.role,
      label: element.name || element.value || "(unnamed)",
      value: element.value,
      state: element.state
    }
  end
end
