defmodule Exhub.LspBridge.Handlers.Completion do
  @moduledoc """
  `textDocument/completion` — port of `core/handler/completion.py`.

  Turns a raw LSP `CompletionList`/`CompletionItem[]` into the candidate
  shape acm's built-in LSP backend consumes (`key`, `icon`, `label`,
  `displayLabel`, `insertText`, `insertTextFormat`, `textEdit`, `score`,
  `sortText`, `filterText`, `server`, `backend`, optional
  `additionalTextEdits`), applying the same pipeline as lsp-bridge:

    * drop items whose kind is in `block-kind-list`;
    * drop items that do not `string_match` the typed prefix;
    * convert LSP snippets to yasnippet placeholders;
    * sort by prefix-match, server `score`, `sortText`, method name, length;
    * cap at `items-limit`.

  Candidate construction is a snapshot: the backend also returns the raw
  items keyed by their candidate `key`, so `completion-item-resolve` can
  fetch documentation for a chosen candidate.

  Tuning travels in the command `args` (the front end supplies acm's
  current settings), keeping this module pure and testable without Emacs.
  """

  @behaviour Exhub.LspBridge.Handler

  # LSP `CompletionItemKind` (1-based) -> lsp-bridge display name.
  @kind_map [
    "",
    "Text",
    "Method",
    "Function",
    "Constructor",
    "Field",
    "Variable",
    "Class",
    "Interface",
    "Module",
    "Property",
    "Unit",
    "Value",
    "Enum",
    "Keyword",
    "Snippet",
    "Color",
    "File",
    "Reference",
    "Folder",
    "EnumMember",
    "Constant",
    "Struct",
    "Event",
    "Operator",
    "TypeParameter"
  ]

  @default_items_limit 100
  @default_display_max 60
  @placeholder ~r/\$\{(\d+):([^}]+)\}/

  @impl true
  def name, do: "completion"

  @impl true
  def method, do: "textDocument/completion"

  @impl true
  def cancel_on_change?, do: true

  @impl true
  def provider, do: "completion"

  @impl true
  def request_params(args, ctx) do
    position = Map.get(args, "position")
    char = Map.get(args, "char")

    context =
      if is_binary(char) and char != "" and char in (ctx[:trigger_characters] || []) do
        %{"triggerCharacter" => char, "triggerKind" => 2}
      else
        %{"triggerKind" => 1}
      end

    %{"position" => position, "context" => context}
  end

  @impl true
  def process_response(result, ctx) do
    opts = options(ctx)
    {candidates, items_by_key} = build(items(result), opts, ctx)
    {:completion, ctx.path, Map.get(ctx, :server) || "", candidates, items_by_key, meta(ctx)}
  end

  # -- options --------------------------------------------------------------

  defp options(ctx) do
    args = ctx.args || %{}

    %{
      prefix: Map.get(args, "prefix", ""),
      match_mode: Map.get(args, "match-mode", "fuzzy"),
      case_mode: Map.get(args, "case-mode", "ignore"),
      items_limit: Map.get(args, "items-limit") || @default_items_limit,
      auto_import: Map.get(args, "auto-import", true),
      display_max: Map.get(args, "display-label-max-length") || @default_display_max,
      block_kinds: Map.get(args, "block-kind-list")
    }
  end

  defp meta(ctx) do
    %{
      "server-names" => Map.get(ctx, :server_names, []),
      "trigger-characters" => Map.get(ctx, :trigger_characters, []),
      "position" => Map.get(ctx.args || %{}, "position")
    }
  end

  # -- result shape ---------------------------------------------------------

  defp items(%{"items" => list}) when is_list(list), do: list
  defp items(list) when is_list(list), do: list
  defp items(_other), do: []

  # -- candidate construction -----------------------------------------------

  defp build(items, opts, ctx) do
    {candidates, items_by_key} =
      Enum.reduce(items, {[], %{}}, fn item, {acc, by_key} ->
        kind = kind(item["kind"])
        label = string(Map.get(item, "label"))
        detail = string(Map.get(item, "detail"))

        cond do
          blocked?(kind, opts.block_kinds) ->
            {acc, by_key}

          not string_match(label, opts.prefix, opts.match_mode, opts.case_mode) ->
            {acc, by_key}

          true ->
            key = key(item, label, detail, opts)
            candidate = candidate(item, kind, label, detail, key, opts, ctx)
            {[candidate | acc], Map.put(by_key, key, item)}
        end
      end)

    candidates =
      candidates
      |> Enum.reverse()
      |> Enum.sort(&compare(&1, &2, opts.prefix))
      |> Enum.take(opts.items_limit)

    {candidates, items_by_key}
  end

  defp candidate(item, kind, label, detail, key, opts, ctx) do
    {insert_text, text_edit, label} = snippetify(kind, item, label)

    base = %{
      "key" => key,
      "icon" => kind,
      "label" => label,
      "displayLabel" => display_label(label, detail, opts),
      "deprecated" => 1 in List.wrap(Map.get(item, "tags", [])),
      "insertText" => insert_text,
      "insertTextFormat" => Map.get(item, "insertTextFormat", ""),
      "textEdit" => text_edit,
      "score" => Map.get(item, "score", 1000),
      "sortText" => Map.get(item, "sortText", ""),
      "filterText" => Map.get(item, "filterText"),
      "server" => Map.get(ctx, :server) || "",
      "backend" => "lsp"
    }

    if opts.auto_import do
      Map.put(base, "additionalTextEdits", Map.get(item, "additionalTextEdits", []))
    else
      base
    end
  end

  defp snippetify("snippet", item, label) do
    insert_text = Map.get(item, "insertText")
    text_edit = Map.get(item, "textEdit")

    cond do
      is_map(text_edit) ->
        {insert_text,
         Map.put(text_edit, "newText", convert_snippet(string(text_edit["newText"]))), label}

      is_binary(insert_text) ->
        {convert_snippet(insert_text), text_edit, label}

      true ->
        {insert_text, text_edit, convert_snippet(label)}
    end
  end

  defp snippetify(_kind, item, label),
    do: {Map.get(item, "insertText"), Map.get(item, "textEdit"), label}

  defp display_label(label, detail, opts) do
    text = if detail == "", do: label, else: "#{label} => #{detail}"

    if String.length(text) > opts.display_max do
      String.slice(text, 0, opts.display_max) <> " ..."
    else
      text
    end
  end

  # -- key ------------------------------------------------------------------

  defp key(item, label, detail, opts) do
    base = "#{label}_#{detail}"

    if opts.auto_import do
      suffix =
        item
        |> Map.get("additionalTextEdits", [])
        |> List.wrap()
        |> Enum.map_join("_", fn edit ->
          edit
          |> Map.get("newText", "")
          |> string()
          |> fnv_1a()
          |> Integer.to_string(16)
          |> String.slice(0, 8)
        end)

      base <> "_" <> suffix
    else
      base
    end
  end

  # FNV-1a, unmasked (Python ints are unbounded), matching lsp-bridge's hash.
  defp fnv_1a(text) do
    text
    |> :binary.bin_to_list()
    |> Enum.reduce(2_166_136_261, fn byte, h -> Bitwise.bxor(h, byte) * 16_777_219 end)
  end

  # -- sorting --------------------------------------------------------------

  defp compare(x, y, prefix) do
    prefix = String.downcase(prefix)
    x_label = filter_label(x)
    y_label = filter_label(y)
    x_prefix = String.starts_with?(x_label, prefix)
    y_prefix = String.starts_with?(y_label, prefix)
    x_score = x["score"] || 0
    y_score = y["score"] || 0
    x_sort = parse_sort_value(x["sortText"])
    y_sort = parse_sort_value(y["sortText"])

    cond do
      x_prefix != y_prefix ->
        x_prefix

      x_score != y_score ->
        x_score > y_score

      x_sort != "" and y_sort != "" and x_sort != y_sort ->
        x_sort < y_sort

      x["icon"] == "method" and y["icon"] == "method" and
          method_name(x_label) != method_name(y_label) ->
        method_name(x_label) < method_name(y_label)

      true ->
        String.length(x_label) < String.length(y_label)
    end
  end

  defp filter_label(candidate) do
    case candidate["filterText"] do
      text when is_binary(text) and text != "" -> String.downcase(text)
      _ -> String.downcase(string(candidate["label"]))
    end
  end

  defp method_name(label), do: label |> String.split("(") |> List.first()

  defp parse_sort_value(nil), do: ""

  defp parse_sort_value(text) when is_binary(text) do
    text |> String.replace(~r/[^0-9.]/, "") |> String.trim_trailing(".")
  end

  defp parse_sort_value(_other), do: ""

  # -- matching (core/utils.py::string_match) -------------------------------

  defp string_match(string, like_name, match_mode, case_mode) do
    {match_mode, case_mode} =
      if match_mode == "prefixCaseSensitive",
        do: {"prefix", "sensitive"},
        else: {match_mode, case_mode}

    {string, like_name} = apply_case(string, like_name, case_mode)

    case match_mode do
      "prefix" -> String.starts_with?(string, like_name)
      "substring" -> String.contains?(string, like_name)
      _ -> fuzzy_match(string, like_name)
    end
  end

  defp apply_case(string, like_name, "smart") do
    if String.match?(like_name, ~r/[A-Z]/),
      do: {string, like_name},
      else: {String.downcase(string), like_name}
  end

  defp apply_case(string, like_name, "ignore"),
    do: {String.downcase(string), String.downcase(like_name)}

  defp apply_case(string, like_name, _other), do: {string, like_name}

  defp fuzzy_match(string, like_name) do
    if String.length(like_name) <= 1 do
      String.contains?(string, like_name)
    else
      [first | rest] = String.graphemes(like_name)

      case :binary.match(string, first) do
        :nomatch ->
          false

        {pos, len} ->
          tail = binary_part(string, pos + len, byte_size(string) - (pos + len))
          fuzzy_match(tail, Enum.join(rest))
      end
    end
  end

  # -- kinds ----------------------------------------------------------------

  defp kind(nil), do: ""

  defp kind(value) when is_integer(value) do
    @kind_map |> Enum.at(value, "") |> String.downcase()
  end

  defp kind(_other), do: ""

  defp blocked?(_kind, nil), do: false
  defp blocked?(_kind, false), do: false
  defp blocked?(kind, list) when is_list(list), do: kind in list
  defp blocked?(_kind, _other), do: false

  # -- snippet conversion (LSP ${n:name} -> yas ${n}) -----------------------

  defp convert_snippet(snippet) do
    convert_snippet(snippet, %{}, [])
  end

  defp convert_snippet("", _placeholders, acc), do: IO.iodata_to_binary(Enum.reverse(acc))

  defp convert_snippet(string, placeholders, acc) do
    case Regex.run(@placeholder, string, return: :index) do
      nil ->
        IO.iodata_to_binary(Enum.reverse([string | acc]))

      [{start, len}, {index_start, index_len}, {name_start, name_len}] ->
        before = binary_part(string, 0, start)
        full = binary_part(string, start, len)
        index = binary_part(string, index_start, index_len)
        name = binary_part(string, name_start, name_len)
        rest = binary_part(string, start + len, byte_size(string) - (start + len))

        case Map.fetch(placeholders, name) do
          {:ok, first} ->
            convert_snippet(rest, placeholders, ["${" <> first <> "}", before | acc])

          :error ->
            convert_snippet(rest, Map.put(placeholders, name, index), [full, before | acc])
        end
    end
  end

  # -- helpers --------------------------------------------------------------

  defp string(value) when is_binary(value), do: value
  defp string(_other), do: ""
end
