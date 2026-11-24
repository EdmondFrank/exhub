defmodule Exhub.Lua.Lint do
  @moduledoc """
  Lexical checks for code-mode snippets, run only *after* the compiler has
  already failed.

  `Lua.CompilerException` is not always helpful about a near-miss:

    * a JSON-style table key (`{command: "ls"}`) makes the parser lose its
      place, so a multi-line snippet fails on a *later* line — the classic
      report is `Expected expression (line 3)` pointing at a `])` while the
      real mistake sits on line 2 — and some such failures arrive as bare
      strings with no position at all;
    * a long string opened with `[[` closes at the first `]]` in its body, so
      embedded JSON/Elixir/Elisp fails as `Expected ',' or '}'` or
      `A bare variable reference isn't a Lua statement` far from the opener.

  `findings/1` re-reads the snippet with `Lua.Lexer.tokenize/1` — which keeps
  positions and comments, and treats table braces unambiguously — and reports
  the mistakes the parser could not place. `repair/1` applies the two
  mechanical fixes (table-key colon, backtick string) so the sandbox can retry
  the snippet once before giving up.

  The checks are deliberately conservative: anything that is legal Lua is left
  alone, since a false finding would replace a better parser message.
  """

  @max_findings 3

  # How much of a line / value to quote back at the caller.
  @snippet_chars 120
  @value_chars 40

  @type finding :: %{
          kind: :json_key | :backtick,
          line: pos_integer(),
          column: pos_integer(),
          snippet: String.t(),
          offset: non_neg_integer()
        }

  @doc """
  Mistakes in `code` that the compiler is likely to misreport.

  Returns at most #{@max_findings} findings, in source order. An empty list
  means the snippet is lexically clean as far as these checks go, so the parser
  message is the best available explanation.
  """
  @spec findings(String.t()) :: [finding()]
  def findings(code) when is_binary(code) do
    (key_findings(code) ++ backtick_findings(code))
    |> Enum.sort_by(& &1.offset)
    |> Enum.take(@max_findings)
  rescue
    _ -> []
  end

  @doc """
  Renders findings as a diagnostic body (no `SyntaxError:` prefix, no leading
  indent).
  """
  @spec render([finding()]) :: String.t()
  def render(findings) when is_list(findings) do
    findings
    |> Enum.map(&render_one/1)
    |> Enum.join("\n  ")
  end

  @doc """
  Mechanical repairs for the mistakes `findings/1` reports.

  Returns the rewritten snippet plus one change per fix, or the input unchanged
  with `[]` when there is nothing (safely) to rewrite. Rewrites are limited to
  constructs that cannot be legal Lua, so a repaired snippet that still fails
  to compile fails for a different reason.
  """
  @spec repair(String.t()) :: {String.t(), [map()]}
  def repair(code) when is_binary(code) do
    {code, backticks} = repair_backticks(code)
    {code, keys} = repair_keys(code)
    {code, backticks ++ keys}
  end

  @doc "One-line summary of repairs, for the `note:` prefixed to a result."
  @spec repair_note([map()]) :: String.t()
  def repair_note(changes) when is_list(changes) do
    body =
      changes
      |> Enum.group_by(& &1.kind)
      |> Enum.map_join(", ", fn
        {:json_key, list} ->
          "table #{plural(list, "key")} at #{at_lines(list)} (`key:` → `key =`)"

        {:backtick, list} ->
          "backtick #{plural(list, "string")} at #{at_lines(list)} (`` `…` `` → `[==[…]==]`)"
      end)

    "note: auto-repaired #{body}. The snippet ran with that fix applied."
  end

  @doc """
  Explains where a `[[` literal closes, when the compiler complained about a
  long string / stray expression.

  Returns `nil` when the snippet does not use a bare `[[` delimiter.
  """
  @spec long_string_note(String.t()) :: String.t() | nil
  def long_string_note(code) when is_binary(code) do
    # `[=[ … ]=]` and stronger delimiters cannot be closed early, so the note
    # only applies to snippets that use plain `[[`.
    if String.contains?(code, "[=") do
      nil
    else
      case :binary.match(code, "[[") do
        {open, 2} -> closure_note(code, open)
        :nomatch -> nil
      end
    end
  end

  def long_string_note(_), do: nil

  # --- Long-string closure ---

  defp closure_note(code, open) do
    from = open + 2

    case :binary.match(code, "]]", scope: {from, byte_size(code) - from}) do
      {close, 2} ->
        "the `[[` at line #{line_of(code, open)} closes at line #{line_of(code, close)} " <>
          "(#{quote_line(code, close)}) — if that `]]` is part of the text, " <>
          "delimit with `[==[ … ]==]` instead."

      :nomatch ->
        "the `[[` at line #{line_of(code, open)} is never closed — if the text contains " <>
          "`]]`, delimit with `[==[ … ]==]`."
    end
  end

  # --- Rendering ---

  defp render_one(%{kind: :json_key} = finding) do
    "line #{finding.line}: table keys use '=', not ':' — " <>
      "`#{finding.snippet}` (#{finding.fix})"
  end

  defp render_one(%{kind: :backtick} = finding) do
    "line #{finding.line}: backticks are not Lua string delimiters — " <>
      "`#{finding.snippet}` should be \"…\" or `[==[ … ]==]`"
  end

  defp plural([_], word), do: word
  defp plural(_list, word), do: word <> "s"

  defp at_lines(list) do
    case list |> Enum.map(& &1.line) |> Enum.uniq() do
      [one] -> "line #{one}"
      many -> "lines " <> Enum.join(many, ", ")
    end
  end

  # --- JSON-style table keys ---

  # Walks the token stream, tracking whether the innermost open delimiter is a
  # table and whether the next token would sit in key position (right after
  # `{`, `,` or `;`). In that position `name :` can never be legal Lua — a
  # method call needs a value to hang off, which key position cannot provide.
  defp key_findings(code) do
    case Lua.Lexer.tokenize(code) do
      {:ok, tokens} ->
        tokens
        |> key_scan([], false, [])
        |> Enum.reverse()
        |> Enum.map(&add_json_key_snippet(code, &1))

      {:error, _reason} ->
        []
    end
  end

  defp key_scan(
         [{:identifier, name, pos}, {:delimiter, :colon, colon_pos} | rest],
         stack,
         true,
         acc
       ) do
    finding = %{
      kind: :json_key,
      line: pos.line,
      column: pos.column,
      offset: colon_pos.byte_offset,
      name: name,
      snippet: "",
      fix: ""
    }

    key_scan(rest, stack, false, [finding | acc])
  end

  defp key_scan([token | rest], stack, key_pos, acc) do
    {stack, key_pos} = advance(token, stack, key_pos)
    key_scan(rest, stack, key_pos, acc)
  end

  defp key_scan([], _stack, _key_pos, acc), do: acc

  defp advance({:delimiter, :lbrace, _pos}, stack, _key_pos), do: {[:brace | stack], true}
  defp advance({:delimiter, :rbrace, _pos}, [:brace | stack], _key_pos), do: {stack, false}
  defp advance({:delimiter, :lparen, _pos}, stack, _key_pos), do: {[:paren | stack], false}
  defp advance({:delimiter, :rparen, _pos}, [:paren | stack], _key_pos), do: {stack, false}
  defp advance({:delimiter, :lbracket, _pos}, stack, _key_pos), do: {[:bracket | stack], false}
  defp advance({:delimiter, :rbracket, _pos}, [:bracket | stack], _key_pos), do: {stack, false}

  defp advance({:delimiter, separator, _pos}, [:brace | _] = stack, _key_pos)
       when separator in [:comma, :semicolon],
       do: {stack, true}

  defp advance({:comment, _kind, _text, _pos}, stack, key_pos), do: {stack, key_pos}
  defp advance(_token, stack, _key_pos), do: {stack, false}

  defp add_json_key_snippet(code, finding) do
    value = value_after(code, finding.offset)

    finding
    |> Map.put(:snippet, line_text(code, finding.line))
    |> Map.put(:fix, "write `#{finding.name} = #{value}`")
  end

  defp value_after(code, offset) do
    from = offset + 1
    rest = binary_part(code, from, byte_size(code) - from)

    value =
      rest
      |> String.slice(0, @value_chars * 4)
      |> then(fn text -> String.split(text, ~r/[\n,}]/, parts: 2) |> hd() end)
      |> String.trim()

    if String.length(value) > @value_chars do
      String.slice(value, 0, @value_chars) <> "…"
    else
      value
    end
  rescue
    _ -> "value"
  end

  # --- Backtick strings ---

  # A minimal scanner is enough here: `Lua.Lexer.tokenize/1` rejects the first
  # backtick outright, so there is no token stream to consult. Strings, comments
  # and long-bracket literals are blanked out first (same byte length) so a
  # backtick that is *content* is not reported.
  defp backtick_findings(code) do
    {pairs, leftover} = code |> mask_non_code() |> backtick_offsets() |> pairs_and_rest()
    opens = Enum.map(pairs, &elem(&1, 0)) ++ leftover

    Enum.map(opens, fn offset ->
      %{
        kind: :backtick,
        line: line_of(code, offset),
        column: 1,
        offset: offset,
        name: nil,
        snippet: quote_backtick(code, offset),
        fix: ""
      }
    end)
  end

  defp backtick_offsets(masked) do
    masked
    |> :binary.matches("`")
    |> Enum.map(&elem(&1, 0))
  end

  defp quote_backtick(code, offset) do
    rest = binary_part(code, offset + 1, byte_size(code) - offset - 1)
    text = rest |> String.split("`", parts: 2) |> hd()

    if String.length(text) > 60 do
      String.slice(text, 0, 60) <> "…"
    else
      text
    end
  rescue
    _ -> "…"
  end

  defp quote_line(code, offset) do
    code |> line_text(line_of(code, offset)) |> String.slice(0, 80)
  rescue
    _ -> "]]"
  end

  # --- Repairs ---

  defp repair_keys(code) do
    offsets =
      case Lua.Lexer.tokenize(code) do
        {:ok, tokens} -> tokens |> key_scan([], false, []) |> Enum.map(& &1.offset)
        {:error, _} -> []
      end

    repaired =
      offsets
      |> Enum.sort(:desc)
      |> Enum.reduce(code, fn offset, acc -> replace_colon(acc, offset) end)

    changes =
      offsets
      |> Enum.sort()
      |> Enum.map(&%{kind: :json_key, line: line_of(code, &1)})

    {repaired, changes}
  rescue
    _ -> {code, []}
  end

  # `key: value` → `key = value`, consuming one space when present so the
  # original spacing survives the rewrite.
  defp replace_colon(code, offset) do
    next = if offset + 1 < byte_size(code), do: binary_part(code, offset + 1, 1), else: ""
    {replacement, consumed} = if next == " ", do: {" = ", 2}, else: {" = ", 1}

    binary_part(code, 0, offset) <>
      replacement <> binary_part(code, offset + consumed, byte_size(code) - offset - consumed)
  end

  defp repair_backticks(code) do
    offsets = code |> mask_non_code() |> backtick_offsets()

    with {pairs, []} <- pairs_and_rest(offsets),
         false <- Enum.any?(pairs, &unsafe_backtick_body?(code, &1)) do
      repaired = rewrite_backticks(code, pairs)

      {repaired,
       Enum.map(pairs, fn {open, _close} -> %{kind: :backtick, line: line_of(code, open)} end)}
    else
      _ -> {code, []}
    end
  end

  defp unsafe_backtick_body?(code, {open, close}) do
    body = binary_part(code, open + 1, close - open - 1)
    String.contains?(body, "]==]")
  end

  defp pairs_and_rest([]), do: {[], []}

  defp pairs_and_rest([a, b | rest]) do
    {pairs, leftover} = pairs_and_rest(rest)
    {[{a, b} | pairs], leftover}
  end

  defp pairs_and_rest([last]), do: {[], [last]}

  # Applied back-to-front so earlier offsets stay valid.
  defp rewrite_backticks(code, pairs) do
    pairs
    |> Enum.sort_by(&elem(&1, 0), :desc)
    |> Enum.reduce(code, fn {open, close}, acc ->
      binary_part(acc, 0, open) <>
        "[==[" <>
        binary_part(acc, open + 1, close - open - 1) <>
        "]==]" <> binary_part(acc, close + 1, byte_size(acc) - close - 1)
    end)
  end

  # --- Source masking for the scanner ---

  # Replaces the contents of quoted strings, comments and long-bracket literals
  # with spaces, preserving byte length so offsets still map onto the original.
  defp mask_non_code(code), do: code |> mask([]) |> IO.iodata_to_binary()

  defp mask(<<>>, acc), do: Enum.reverse(acc)

  defp mask(<<"--", rest::binary>>, acc) do
    {line_len, rest} = split_line(rest, 0)
    mask(rest, [spaces(2 + line_len) | acc])
  end

  defp mask(<<"[", rest::binary>> = bin, acc) do
    case long_bracket(rest) do
      {equals, closer} ->
        open_len = 2 + equals
        after_open = binary_part(rest, equals + 1, byte_size(rest) - equals - 1)

        case :binary.match(after_open, closer) do
          {close, closer_len} ->
            total = open_len + close + closer_len
            mask(binary_part(bin, total, byte_size(bin) - total), [spaces(total) | acc])

          :nomatch ->
            mask(after_open, [spaces(open_len) | acc])
        end

      nil ->
        mask(rest, ["[" | acc])
    end
  end

  defp mask(<<quote, rest::binary>>, acc) when quote in [?", ?'] do
    {consumed, rest} = quoted(rest, quote, 1)
    mask(rest, [spaces(consumed) | acc])
  end

  defp mask(<<byte, rest::binary>>, acc), do: mask(rest, [<<byte>> | acc])

  # `[[`, `[=[`, `[==[` … → {number of '=', closer}. `rest` is the source after
  # the first `[`.
  defp long_bracket(rest) do
    equals = count_equals(rest, 0)

    if byte_size(rest) > equals and binary_part(rest, equals, 1) == "[" do
      {equals, "]" <> String.duplicate("=", equals) <> "]"}
    end
  end

  defp count_equals(<<?=, rest::binary>>, n), do: count_equals(rest, n + 1)
  defp count_equals(_rest, n), do: n

  defp split_line(<<>>, n), do: {n, <<>>}
  defp split_line(<<"\n", rest::binary>>, n), do: {n + 1, rest}
  defp split_line(<<_byte, rest::binary>>, n), do: split_line(rest, n + 1)

  # Length consumed by a quoted string, including the closing quote; stops
  # before a newline (Lua quoted strings cannot span lines).
  defp quoted(<<>>, _quote, n), do: {n, <<>>}
  defp quoted(<<"\\", _escaped, rest::binary>>, quote, n), do: quoted(rest, quote, n + 2)
  defp quoted(<<char, rest::binary>>, quote, n) when char == quote, do: {n + 1, rest}
  defp quoted(<<"\n", _rest::binary>> = bin, _quote, n), do: {n, bin}
  defp quoted(<<_byte, rest::binary>>, quote, n), do: quoted(rest, quote, n + 1)

  defp spaces(0), do: ""
  defp spaces(n) when n > 0, do: :binary.copy(" ", n)

  # --- Positions ---

  defp line_of(code, offset) do
    prefix_len = min(offset, byte_size(code))
    prefix = binary_part(code, 0, prefix_len)
    length(:binary.matches(prefix, "\n")) + 1
  end

  defp line_text(code, n) do
    code
    |> String.split("\n")
    |> Enum.at(n - 1, "")
    |> String.trim()
    |> String.slice(0, @snippet_chars)
  rescue
    _ -> ""
  end
end
