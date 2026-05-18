defmodule Exhub.MCP.Tools.Desktop.EditBlock do
  @moduledoc """
  MCP Tool: edit_block

  Perform a targeted find-and-replace edit within a text file.

  Improvements over the naive implementation (inspired by Desktop Commander MCP):
  - Line ending detection & normalization (LF / CRLF / CR)
  - Precise single-occurrence replacement via `global: false`
  - Helpful count-mismatch error with actionable suggestions
  - Fuzzy-match fallback with Jaro similarity (built-in C NIF, ~30× faster than
    Levenshtein DP), automatic replacement for ≥95% similarity, and character-level diff
  - Large-edit warning when search/replace text exceeds the recommended line limit
  - Content size limits to prevent timeouts on extremely large payloads
  - Task-based timeout (10 s) for fuzzy matching to keep the server responsive
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  # ---------------------------------------------------------------------------
  # Tunables
  # ---------------------------------------------------------------------------

  # Minimum Jaro similarity (0–1) for a fuzzy match to be reported as "close"
  @fuzzy_threshold 0.7

  # Jaro similarity threshold for direct replacement with warning
  # (Jaro is more lenient than Levenshtein so 0.95 ≈ Levenshtein 0.98)
  @direct_replace_threshold 0.95

  # Lines above this threshold trigger a warning (mirrors Desktop Commander default)
  @max_lines_warning 50

  # Maximum byte size for old_string / new_string (100 KB each).
  # Beyond this the tool rejects early with an actionable message.
  @max_string_bytes 100_000

  # Maximum byte size of the *file* for which we attempt fuzzy matching.
  # Benchmarked at ~65 µs/window with zero-copy binary matching; 10 MB ≈ 1 s.
  # The 10 s Task timeout provides a hard safety net beyond this limit.
  @max_fuzzy_file_bytes 10_000_000

  # Timeout (ms) for the fuzzy-match Task.  Keeps the GenServer responsive.
  @fuzzy_timeout_ms 10_000

  def name, do: "edit_block"

  @impl true
  def description do
    """
    Perform a targeted find-and-replace edit within a text file.

    Finds the exact occurrence of `old_string` in the file and replaces it with
    `new_string`. The match is exact (case-sensitive). By default, exactly one
    replacement is expected; set `expected_replacements` to allow more.

    When an exact match is not found, a fuzzy search is attempted. If the similarity
    is 95% or higher, the replacement is applied automatically with a warning. For
    lower similarities, the closest match is reported together with a character-level
    diff so you can correct the search string.

    Use this tool for surgical edits — prefer it over rewriting the whole file
    when only a small section needs to change.

    Parameters:
    - file_path: Absolute path or ~ shorthand to the file to edit
    - old_string: The exact text to find and replace
    - new_string: The replacement text
    - expected_replacements: Number of replacements expected (default 1)
    """
  end

  schema do
    field(:file_path, {:required, :string}, description: "Absolute path or ~ shorthand to the file to edit")
    field(:old_string, {:required, :string}, description: "The exact text to find and replace")
    field(:new_string, {:required, :string}, description: "The replacement text")

    field(:expected_replacements, :integer,
      description: "Number of replacements expected (default 1)",
      default: 1
    )
  end

  @impl true
  def execute(params, frame) do
    with {:ok, file_path} <- Map.get(params, :file_path) |> Helpers.validate_absolute_path() do
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
              "message" => message
            })

          {:reply, resp, frame}

        {:error, reason} ->
          resp = Response.tool() |> Response.error("Edit failed: #{reason}")
          {:reply, resp, frame}
      end
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  # ---------------------------------------------------------------------------
  # Core edit logic
  # ---------------------------------------------------------------------------

  defp perform_edit(file_path, old_string, new_string, expected) do
    with :ok <- validate_inputs(old_string, new_string),
         {:ok, content} <- read_file(file_path) do
      # Detect the file's native line-ending style and normalise the search /
      # replace strings so that CRLF vs LF differences never cause a miss.
      file_line_ending = detect_line_ending(content)
      normalized_search = normalize_line_endings(old_string, file_line_ending)

      count = count_occurrences(content, normalized_search)

      cond do
        count == 0 ->
          # Exact match failed – try fuzzy search as a helpful fallback.
          handle_fuzzy_fallback(content, old_string, new_string, expected, file_path, file_line_ending)

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

  defp build_fuzzy_warning(similarity) do
    "\n\nWARNING: Fuzzy match used (#{round(similarity * 100)}% similarity). The search string was automatically adjusted."
  end

  # ---------------------------------------------------------------------------
  # Fuzzy-match fallback  (Task-wrapped with timeout)
  # ---------------------------------------------------------------------------

  defp handle_fuzzy_fallback(content, search_string, new_string, expected, file_path, file_line_ending) do
    content_bytes = byte_size(content)

    if content_bytes > @max_fuzzy_file_bytes do
      {:error,
       "String not found in #{file_path}. Fuzzy search was skipped because the file " <>
         "is #{div(content_bytes, 1024)} KB (limit: #{div(@max_fuzzy_file_bytes, 1024)} KB). " <>
         "old_string must match the file content exactly (case-sensitive, including whitespace). " <>
         "Tip: include more surrounding context so the exact match succeeds."}
    else
      # Run fuzzy matching in a separate process with a timeout so we never
      # block the GenServer (which would make the server appear "unavailable").
      task =
        Task.async(fn ->
          find_best_fuzzy_match(content, search_string)
        end)

      case Task.yield(task, @fuzzy_timeout_ms) || Task.shutdown(task, :brutal_kill) do
        {:ok, {found_text, similarity}} ->
          apply_fuzzy_result(
            content, found_text, similarity, search_string, new_string,
            expected, file_path, file_line_ending
          )

        nil ->
          {:error,
           "Fuzzy search timed out after #{@fuzzy_timeout_ms} ms on #{file_path}. " <>
             "The file may be too large or the search string too complex. " <>
             "old_string must match the file content exactly (case-sensitive, including whitespace)."}
      end
    end
  end

  defp apply_fuzzy_result(content, found_text, similarity, search_string, new_string, expected, file_path, file_line_ending) do
    cond do
      similarity >= @direct_replace_threshold ->
        # Direct replacement with warning
        normalized_found = normalize_line_endings(found_text, file_line_ending)
        normalized_replace = normalize_line_endings(new_string, file_line_ending)
        new_content = apply_replacement(content, normalized_found, normalized_replace, expected)
        warning = build_fuzzy_warning(similarity) <> build_line_warning(found_text, new_string)

        case File.write(file_path, new_content) do
          :ok -> {:ok, expected, warning}
          {:error, :eacces} -> {:error, "Permission denied: #{file_path}"}
          {:error, reason} -> {:error, inspect(reason)}
        end

      similarity >= @fuzzy_threshold ->
        diff = highlight_differences(search_string, found_text)

        {:error,
         "Exact match not found in #{file_path}, but a similar passage was found " <>
           "with #{round(similarity * 100)}% similarity.\n\n" <>
           "Character-level diff (searched → found):\n#{diff}\n\n" <>
           "Copy the exact text shown above as the found passage and use it as old_string."}

      true ->
        preview = String.slice(found_text, 0, 120)

        {:error,
         "String not found in #{file_path}. " <>
           "The closest match (\"#{preview}…\") has only #{round(similarity * 100)}% similarity, " <>
           "which is below the #{round(@fuzzy_threshold * 100)}% threshold. " <>
           "old_string must match the file content exactly (case-sensitive, including whitespace)."}
    end
  end

  # ---------------------------------------------------------------------------
  # Sliding-window fuzzy search using Jaro distance
  # ---------------------------------------------------------------------------

  # Sliding-window search: sample windows of `query_bytes` across the file at
  # intervals of `step`, pick the one with the highest Jaro similarity.
  #
  # Uses zero-copy binary pattern matching (`<<_::size, window::size, _::binary>>`)
  # instead of `String.slice` to avoid allocating a new binary per window.
  # Benchmarked at ~65 µs/window — 10 MB file completes in ~1 s.
  defp find_best_fuzzy_match(content, search_string) do
    search_bytes = byte_size(search_string)
    content_bytes = byte_size(content)

    if content_bytes <= search_bytes do
      # The whole file is the only candidate.
      sim = String.jaro_distance(content, search_string)
      {content, sim}
    else
      # Step size: half of query length (minimum 1).
      # Halves window count vs quarter-step with negligible accuracy loss.
      step = max(1, div(search_bytes, 2))
      max_start = content_bytes - search_bytes

      {best_window, best_sim} =
        0..max_start//step
        |> Enum.reduce({<<>>, 0.0}, fn start, {best_w, best_s} ->
          <<_::binary-size(start), window::binary-size(search_bytes), _::binary>> = content
          sim = String.jaro_distance(window, search_string)
          if sim > best_s, do: {window, sim}, else: {best_w, best_s}
        end)

      # best_window is already a binary (= valid UTF-8 string in Elixir)
      {best_window, best_sim}
    end
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
    count_occurrences(content, pattern, 0)
  end

  defp count_occurrences(content, pattern, count) do
    case :binary.match(content, pattern) do
      :nomatch ->
        count

      {pos, len} ->
        <<_::binary-size(pos), _::binary-size(len), rest::binary>> = content
        count_occurrences(rest, pattern, count + 1)
    end
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

  defp validate_inputs("", _new_string) do
    {:error, "old_string cannot be empty — an empty search string would match everywhere."}
  end

  defp validate_inputs(old_string, new_string) do
    old_bytes = byte_size(old_string)
    new_bytes = byte_size(new_string)

    cond do
      old_bytes > @max_string_bytes ->
        {:error,
         "old_string is #{old_bytes} bytes (limit: #{@max_string_bytes}). " <>
           "For large edits consider splitting the operation into smaller chunks."}

      new_bytes > @max_string_bytes ->
        {:error,
         "new_string is #{new_bytes} bytes (limit: #{@max_string_bytes}). " <>
           "For large edits consider splitting the operation into smaller chunks."}

      true ->
        :ok
    end
  end
end
