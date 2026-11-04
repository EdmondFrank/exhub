defmodule Exhub.Lua.Diagnostics do
  @moduledoc """
  Classifies a Lua sandbox failure into a stable, model-friendly category so the
  caller can react (fix syntax, discover a tool, retry) instead of parsing a raw
  Lua trace. Mirrors AiderDesk's PTC diagnostics.

  Categories: `SyntaxError` (the snippet did not compile), `UnknownTool` (a call
  on a missing tool or global, with a `retrieve_tools` hint), `ToolFailure`
  (`server.tool failed: …`) and `TimeoutExceeded`.

  ## `SyntaxError` detail

  `Exception.message/1` on `Lua.CompilerException` deliberately flattens the
  report to bare messages. That is unhelpful to a caller that has to *rewrite*
  the snippet, so a parse failure is rendered here from
  `Lua.CompilerException.to_map/1` instead:

      SyntaxError: Expected expression (line 1)
        1 | return 2 +
          |          ^
        suggestion: Reached end of file unexpectedly. …

  When the parser pins an error to a line, the line number, the offending
  source line and a caret are included, along with the parser's suggestion and
  — for mistakes the parser cannot name — a hand-written `hint:` (see
  `@hints`). Some parser errors carry no position at all (they come back as a
  bare string rather than a `Lua.Parser.Error`); those still get their hint,
  which is what makes `{key: value}` actionable.
  """

  # The parser reports some failures with no position, no source context and no
  # suggestion — `{key: value}` (JSON-style table key) is the common one, and the
  # bare message ("Expected '(', string, or table after method name") does not
  # name the mistake. Matched by substring against the cleaned message.
  @json_key_hint "Lua table keys use '=', not ':' — write `{path = \"/x\"}`, not `{path: \"/x\"}`. " <>
                   "A ':' after a name starts a method call (`s:sub(1, 2)`), so the JSON-style form never compiles."

  # Which message the parser produces depends only on what follows the colon: an
  # identifier (`{key: value}`) trips the method-name branch, anything else
  # (`{key: \"value\"}`) trips the identifier expectation.
  @hints [
    {"after method name", @json_key_hint},
    {"Expected :identifier, got", @json_key_hint}
  ]

  @max_errors 3

  @doc """
  Turns a sandbox exception into a categorised message.

  `Lua.CompilerException` → `SyntaxError` (with line, source excerpt, suggestion
  and/or hint); any other (runtime) error → `UnknownTool` when it is a call on a
  nil value, else the raw message.
  """
  @spec classify(Exception.t()) :: String.t()
  def classify(%Lua.CompilerException{} = error) do
    case syntax_report(error) do
      "" -> "SyntaxError: code could not be parsed"
      body -> "SyntaxError: " <> body
    end
  end

  def classify(error) do
    error
    |> Exception.message()
    |> String.replace_prefix("Lua runtime error: ", "")
    |> classify_runtime()
  end

  defp classify_runtime(message) do
    case Regex.run(~r/attempt to (?:call|index) a nil value \([^']*'([^']+)'/, message) do
      [_, name] ->
        "UnknownTool: '#{name}' is not available in this sandbox. " <>
          "Use retrieve_tools({query = '...'}) inside the code to find available tools."

      _ ->
        message
    end
  end

  @doc "Normalises a process exit / tool-call failure reason to a message string."
  @spec format_reason(term()) :: String.t()
  def format_reason(reason) when is_binary(reason), do: reason
  def format_reason(%{__exception__: true} = exception), do: Exception.message(exception)
  def format_reason(%{message: message}) when is_binary(message), do: message
  def format_reason(reason), do: inspect(reason)

  # --- SyntaxError rendering ---

  defp syntax_report(%Lua.CompilerException{} = error) do
    case Lua.CompilerException.to_map(error) do
      [_ | _] = errors -> render_errors(errors)
      [] -> error |> legacy_message() |> clean_message()
    end
  end

  # Compiler/lexer failures carry no structured diagnostics — only the plain
  # `:errors` strings under the `Failed to compile Lua!` header.
  defp legacy_message(%Lua.CompilerException{} = error) do
    error
    |> Exception.message()
    |> String.replace_prefix("Failed to compile Lua!", "")
    |> String.trim()
  end

  defp render_errors(errors) do
    rendered = errors |> Enum.take(@max_errors) |> Enum.map(&render_error/1)

    rendered =
      if length(errors) > @max_errors,
        do: rendered ++ ["… (#{length(errors) - @max_errors} more)"],
        else: rendered

    Enum.join(rendered, "\n")
  end

  defp render_error(error) do
    message = clean_message(error[:message])
    excerpt = excerpt_target(error[:source_context])
    head = message <> location(excerpt)

    [head, render_excerpt(excerpt), suggestion(error[:suggestion]), hint(message)]
    |> Enum.reject(&is_nil/1)
    |> Enum.join("\n")
  end

  defp location(nil), do: ""
  defp location({number, _text, _column}), do: " (line #{number})"

  # Prefer the highlighted line; if it is blank (the parser pinned the failure to
  # the end of input) fall back to the last non-blank line, pointing past its
  # final character — the place the missing token belongs.
  defp excerpt_target(%{lines: lines} = context) when is_list(lines) do
    pointer = context[:pointer_column] || 1
    highlighted = Enum.find(lines, & &1[:highlight?])

    cond do
      highlighted && String.trim(highlighted[:text] || "") != "" ->
        {highlighted[:number], highlighted[:text], pointer}

      true ->
        case lines |> Enum.reverse() |> Enum.find(&(String.trim(&1[:text] || "") != "")) do
          nil -> nil
          line -> {line[:number], line[:text], String.length(line[:text]) + 1}
        end
    end
  end

  defp excerpt_target(_), do: nil

  defp render_excerpt(nil), do: nil

  defp render_excerpt({number, text, column}) do
    label = to_string(number)
    gutter = String.duplicate(" ", String.length(label))
    caret = String.duplicate(" ", max(column - 1, 0))

    "  #{label} | #{text}\n  #{gutter} | #{caret}^"
  end

  defp suggestion(nil), do: nil

  defp suggestion(text) when is_binary(text) do
    case text |> String.split("\n") |> Enum.map_join(" ", &String.trim/1) |> String.trim() do
      "" -> nil
      flat -> "  suggestion: #{flat}"
    end
  end

  defp suggestion(_), do: nil

  defp hint(message) do
    case Enum.find(@hints, fn {needle, _hint} -> String.contains?(message, needle) end) do
      nil -> nil
      {_needle, hint} -> "  hint: " <> hint
    end
  end

  # The parser surfaces positionless failures as `Parse error: "…"` — an
  # inspected message, quotes and escapes included. Strip that wrapper so the
  # caller reads the parser's sentence, not Elixir's rendering of it.
  defp clean_message(message) when is_binary(message) do
    message
    |> String.trim()
    |> then(fn text ->
      case Regex.run(~r/^Parse error:\s*"(.*)"$/s, text) do
        [_, inner] -> String.replace(inner, "\\\"", "\"")
        _ -> text
      end
    end)
  end

  defp clean_message(_), do: "code could not be parsed"
end
