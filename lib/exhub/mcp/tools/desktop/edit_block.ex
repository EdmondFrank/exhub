defmodule Exhub.MCP.Tools.Desktop.EditBlock do
  @moduledoc """
  MCP Tool: edit_block

  Perform a targeted find-and-replace edit within a text file.

  Improvements over the naive implementation (inspired by Desktop Commander MCP):
  - Line ending detection & normalization (LF / CRLF / CR)
  - Precise single-occurrence replacement via `global: false`
  - Helpful count-mismatch error with actionable suggestions
  - Fuzzy-match fallback with Levenshtein similarity and character-level diff
  - Large-edit warning when search/replace text exceeds the recommended line limit
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  # Minimum similarity (0–1) for a fuzzy match to be reported as "close"
  @fuzzy_threshold 0.7

  # Lines above this threshold trigger a warning (mirrors Desktop Commander default)
  @max_lines_warning 50

  def name, do: "edit_block"

  @impl true
  def description do
    """
    Perform a targeted find-and-replace edit within a text file.

    Finds the exact occurrence of `old_string` in the file and replaces it with
    `new_string`. The match is exact (case-sensitive). By default, exactly one
    replacement is expected; set `expected_replacements` to allow more.

    When an exact match is not found, a fuzzy search is attempted and the closest
    match is reported together with a character-level diff so you can correct the
    search string.

    Use this tool for surgical edits — prefer it over rewriting the whole file
    when only a small section needs to change.

    Parameters:
    - file_path: Absolute path to the file to edit
    - old_string: The exact text to find and replace
    - new_string: The replacement text
    - expected_replacements: Number of replacements expected (default 1)
    """
  end

  schema do
    field(:file_path, {:required, :string}, description: "Absolute path to the file to edit")
    field(:old_string, {:required, :string}, description: "The exact text to find and replace")
    field(:new_string, {:required, :string}, description: "The replacement text")

    field(:expected_replacements, :integer,
      description: "Number of replacements expected (default 1)",
      default: 1
    )
  end

  @impl true
  def execute(params, frame) do
    file_path = Map.get(params, :file_path) |> Helpers.expand_path()
    old_string = Map.get(params, :old_string)
    new_string = Map.get(params, :new_string)
    expected = Map.get(params, :expected_replacements, 1)

    case perform_edit(file_path, old_string, new_string, expected) do
      {:ok, replacements, warning} ->
        message =
          "Successfully applied #{replacements} edit(s) to #{file_path}" <> warning

        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "success" => true,
            "file_path" => file_path,
            "replacements_made" => replacements,
            "message" => message
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Edit failed: #{reason}")
        {:reply, resp, frame}
    end
  end

  # ---------------------------------------------------------------------------
  # Core edit logic
  # ---------------------------------------------------------------------------

  defp perform_edit(file_path, old_string, new_string, expected) do
    with :ok <- validate_inputs(old_string),
         {:ok, content} <- read_file(file_path) do
      # Detect the file's native line-ending style and normalise the search /
      # replace strings so that CRLF vs LF differences never cause a miss.
      file_line_ending = detect_line_ending(content)
      normalized_search = normalize_line_endings(old_string, file_line_ending)

      count = count_occurrences(content, normalized_search)

      cond do
        count == 0 ->
          # Exact match failed – try fuzzy search as a helpful fallback.
          handle_fuzzy_fallback(content, old_string, file_path)

        count != expected ->
          {:error,
           "Expected #{expected} replacement(s) but found #{count} occurrence(s) in #{file_path}. " <>
             "Double-check that you understand all occurrences. " <>
             "To replace all #{count} occurrences set expected_replacements to #{count}. " <>
             "To target a specific occurrence, add more surrounding context to old_string " <>
             "so that it uniquely identifies the section you want to change."}

        true ->
          normalized_replace = normalize_line_endings(new_string, file_line_ending)
          new_content = apply_replacement(content, normalized_search, normalized_replace, expected)

          warning = build_line_warning(old_string, new_string)

          case File.write(file_path, new_content) do
            :ok -> {:ok, count, warning}
            {:error, :eacces} -> {:error, "Permission denied: #{file_path}"}
            {:error, reason} -> {:error, inspect(reason)}
          end
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Replacement helpers
  # ---------------------------------------------------------------------------

  # Single replacement – use global: false to replace only the first occurrence.
  defp apply_replacement(content, search, replace, 1) do
    String.replace(content, search, replace, global: false)
  end

  # Multiple replacements – replace all occurrences.
  defp apply_replacement(content, search, replace, _expected) do
    String.replace(content, search, replace)
  end

  # ---------------------------------------------------------------------------
  # Large-edit warning
  # ---------------------------------------------------------------------------

  defp build_line_warning(old_string, new_string) do
    search_lines = old_string |> String.split("\n") |> length()
    replace_lines = new_string |> String.split("\n") |> length()
    max_lines = max(search_lines, replace_lines)

    if max_lines > @max_lines_warning do
      problem = if search_lines > replace_lines, do: "search text", else: "replacement text"

      "\n\nWARNING: The #{problem} spans #{max_lines} lines " <>
        "(recommended maximum: #{@max_lines_warning}). " <>
        "For large edits consider splitting the operation into smaller chunks."
    else
      ""
    end
  end

  # ---------------------------------------------------------------------------
  # Fuzzy-match fallback
  # ---------------------------------------------------------------------------

  defp handle_fuzzy_fallback(content, search_string, file_path) do
    {found_text, similarity} = find_best_fuzzy_match(content, search_string)

    if similarity >= @fuzzy_threshold do
      diff = highlight_differences(search_string, found_text)

      {:error,
       "Exact match not found in #{file_path}, but a similar passage was found " <>
         "with #{round(similarity * 100)}% similarity.\n\n" <>
         "Character-level diff (searched → found):\n#{diff}\n\n" <>
         "Copy the exact text shown above as the found passage and use it as old_string."}
    else
      preview = String.slice(found_text, 0, 120)

      {:error,
       "String not found in #{file_path}. " <>
         "The closest match (\"#{preview}…\") has only #{round(similarity * 100)}% similarity, " <>
         "which is below the #{round(@fuzzy_threshold * 100)}% threshold. " <>
         "old_string must match the file content exactly (case-sensitive, including whitespace)."}
    end
  end

  # Sliding-window search: sample windows of `query_len` characters across the
  # file at intervals of `step`, pick the one with the lowest Levenshtein
  # distance to the query, then return it together with a similarity score.
  defp find_best_fuzzy_match(content, search_string) do
    query_len = String.length(search_string)
    content_len = String.length(content)

    if content_len <= query_len do
      # The whole file is the only candidate.
      sim = similarity_ratio(content, search_string)
      {content, sim}
    else
      # Step size: quarter of query length (minimum 1) for a reasonable balance
      # between coverage and performance.
      step = max(1, div(query_len, 4))
      max_start = content_len - query_len

      {best_text, best_dist} =
        0..max_start//step
        |> Enum.reduce({"", content_len + query_len}, fn start, {best_t, best_d} ->
          window = String.slice(content, start, query_len)
          d = levenshtein_distance(window, search_string)
          if d < best_d, do: {window, d}, else: {best_t, best_d}
        end)

      max_len = max(String.length(best_text), query_len)
      sim = if max_len == 0, do: 1.0, else: 1.0 - best_dist / max_len
      {best_text, sim}
    end
  end

  # Levenshtein distance between two strings (O(m × n) DP, row-based).
  defp levenshtein_distance(s, t) do
    s_chars = String.graphemes(s)
    t_chars = String.graphemes(t)
    t_len = length(t_chars)

    # Initial row: cost of transforming "" into t[0..j]
    initial_row = Enum.to_list(0..t_len)

    s_chars
    |> Enum.with_index(1)
    |> Enum.reduce(initial_row, fn {s_char, i}, prev_row ->
      # Build the next row left-to-right.
      {_left, next_row} =
        t_chars
        |> Enum.with_index(1)
        |> Enum.reduce({i, [i]}, fn {t_char, j}, {left, row_acc} ->
          diag = Enum.at(prev_row, j - 1)
          up = Enum.at(prev_row, j)
          cost = if s_char == t_char, do: 0, else: 1
          curr = Enum.min([left + 1, up + 1, diag + cost])
          {curr, row_acc ++ [curr]}
        end)

      next_row
    end)
    |> List.last()
  end

  # Similarity as a ratio in [0, 1] where 1 = identical strings.
  defp similarity_ratio(a, b) do
    max_len = max(String.length(a), String.length(b))
    if max_len == 0, do: 1.0, else: 1.0 - levenshtein_distance(a, b) / max_len
  end

  # ---------------------------------------------------------------------------
  # Character-level diff  {-removed-}{+added+}
  # ---------------------------------------------------------------------------

  defp highlight_differences(expected, actual) do
    exp_chars = String.graphemes(expected)
    act_chars = String.graphemes(actual)
    min_len = min(length(exp_chars), length(act_chars))

    # Common prefix
    prefix_len =
      Enum.zip(exp_chars, act_chars)
      |> Enum.take_while(fn {a, b} -> a == b end)
      |> length()

    # Common suffix (from the end, not overlapping the prefix)
    suffix_len =
      Enum.zip(Enum.reverse(exp_chars), Enum.reverse(act_chars))
      |> Enum.take_while(fn {a, b} -> a == b end)
      |> Enum.take(min_len - prefix_len)
      |> length()

    common_prefix = Enum.take(exp_chars, prefix_len) |> Enum.join()
    common_suffix = Enum.take(Enum.reverse(exp_chars), suffix_len) |> Enum.reverse() |> Enum.join()

    exp_diff =
      exp_chars |> Enum.drop(prefix_len) |> Enum.drop(-suffix_len) |> Enum.join()

    act_diff =
      act_chars |> Enum.drop(prefix_len) |> Enum.drop(-suffix_len) |> Enum.join()

    "#{common_prefix}{-#{exp_diff}-}{+#{act_diff}+}#{common_suffix}"
  end

  # ---------------------------------------------------------------------------
  # File I/O helpers
  # ---------------------------------------------------------------------------

  defp read_file(path) do
    case File.read(path) do
      {:ok, content} -> {:ok, content}
      {:error, :enoent} -> {:error, "File not found: #{path}"}
      {:error, :eacces} -> {:error, "Permission denied: #{path}"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp count_occurrences(content, pattern) do
    content |> String.split(pattern) |> length() |> Kernel.-(1)
  end

  # ---------------------------------------------------------------------------
  # Line-ending helpers
  # ---------------------------------------------------------------------------

  # Detect the predominant line-ending style in *content*.
  # CRLF is checked first because it contains LF; checking LF first would give
  # false positives on Windows files.
  defp detect_line_ending(content) do
    cond do
      String.contains?(content, "\r\n") -> "\r\n"
      String.contains?(content, "\r") -> "\r"
      true -> "\n"
    end
  end

  # Normalise *text* so it uses *target_ending* throughout.
  defp normalize_line_endings(text, target_ending) do
    # First collapse everything to bare LF …
    lf_only =
      text
      |> String.replace("\r\n", "\n")
      |> String.replace("\r", "\n")

    # … then expand to the target style.
    case target_ending do
      "\r\n" -> String.replace(lf_only, "\n", "\r\n")
      "\r" -> String.replace(lf_only, "\n", "\r")
      _ -> lf_only
    end
  end

  # ---------------------------------------------------------------------------
  # Input validation
  # ---------------------------------------------------------------------------

  defp validate_inputs("") do
    {:error, "old_string cannot be empty — an empty search string would match everywhere."}
  end

  defp validate_inputs(_old_string), do: :ok
end
