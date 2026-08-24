defmodule Exhub.BlinkSearch.Backends.GrepPDF do
  @moduledoc """
  Grep PDF backend — searches PDF files across configured directories using `rga`.

  Uses `rga --json` for structured output. Handles multiple search paths
  with `$D0`, `$D1` placeholders for path compression.
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @rga_page_pattern ~r/None:.*?: Page /

  @impl true
  def search_match(prefix, state) do
    search_paths = Map.get(state, :search_paths, [])
    clean_prefix = String.replace(prefix, "*", "")
    words = String.split(clean_prefix, ~r/\s+/, trim: true)

    if words == [] or search_paths == [] do
      []
    else
      rga_bin = System.find_executable("rga")

      if rga_bin do
        pattern = Enum.join(words, ".*")

        command =
          [rga_bin, "--json", "-S", "--max-columns", "300", pattern] ++
            Enum.map(search_paths, &Path.expand/1)

        Backend.get_process_result(command)
        |> Enum.map(fn line -> parse_rga_line(line, search_paths) end)
        |> Enum.reject(&is_nil/1)
      else
        []
      end
    end
  end

  @impl true
  def do_action(candidate, state) do
    search_paths = Map.get(state, :search_paths, [])
    match_text = candidate_match_text(candidate, state)
    text = Backend.candidate_text(candidate)

    case String.split(text, ":", parts: 3) do
      [file, line, _rest] ->
        real_path = get_real_path(file, search_paths)

        Exhub.send_message(
          ~s|(blink-search-grep-pdf-do #{Backend.elisp_quote(real_path)} #{line} #{Backend.elisp_quote(match_text)})|
        )

      _ ->
        :ok
    end

    :ok
  end

  @impl true
  def select(candidate, state) do
    search_paths = Map.get(state, :search_paths, [])
    match_text = candidate_match_text(candidate, state)
    text = Backend.candidate_text(candidate)

    case String.split(text, ":", parts: 3) do
      [file, line, _rest] ->
        real_path = get_real_path(file, search_paths)

        Exhub.send_message(
          ~s|(blink-search-grep-pdf-preview #{Backend.elisp_quote(real_path)} #{line} #{Backend.elisp_quote(match_text)})|
        )

      _ ->
        :ok
    end

    :ok
  end

  @impl true
  def clean(state) do
    Exhub.send_message("(blink-search-grep-pdf-clean)")
    state
  end

  @impl true
  def init_dir(search_dir, state) do
    search_paths = Map.get(state, :grep_pdf_search_paths, [])

    paths =
      if search_paths != [] do
        search_paths
      else
        [search_dir]
      end

    Map.put(state, :search_paths, paths)
  end

  # Private helpers

  # Candidates arrive as plain strings from Emacs (plist info is stripped),
  # so fall back to the search keyword stored in state by the Server.
  defp candidate_match_text(candidate, state) do
    case candidate do
      %{match_text: match_text} when is_binary(match_text) -> match_text
      %{"match_text" => match_text} when is_binary(match_text) -> match_text
      _ -> Map.get(state, :match_text, "")
    end
  end

  defp parse_rga_line(line, search_paths) do
    base_path = if length(search_paths) > 1, do: "/", else: List.first(search_paths)
    result = Backend.parse_rg_line(line, base_path)

    if result do
      # Remove "None:...: Page " prefix from rga output
      cleaned_text = Regex.replace(@rga_page_pattern, result.text, "")
      remove_len = String.length(result.text) - String.length(cleaned_text)

      # Compress paths for multi-directory search
      {final_text, extra_remove} =
        if length(search_paths) > 1 do
          compress_paths(cleaned_text, search_paths)
        else
          {cleaned_text, 0}
        end

      total_remove = remove_len + extra_remove

      adjusted_matches =
        Enum.map(result.matches, fn [s, e] ->
          [s - total_remove, e - total_remove]
        end)

      # Only include PDF files
      file_part = final_text |> String.split(":") |> List.first()

      if String.contains?(file_part || "", ".pdf") do
        # Extract match text for the candidate
        match_text =
          case adjusted_matches do
            [[s, e] | _] when s >= 0 and e >= 0 ->
              binary = final_text
              safe_s = max(s, 0)
              safe_e = min(e, String.length(binary))
              if safe_e > safe_s, do: String.slice(binary, safe_s, safe_e - safe_s), else: ""

            _ ->
              ""
          end

        %{text: final_text, matches: adjusted_matches, match_text: match_text}
      else
        nil
      end
    else
      nil
    end
  rescue
    _ -> nil
  end

  defp compress_paths(text, search_paths) do
    Enum.reduce_while(search_paths, {text, 0}, fn path, {acc_text, acc_remove} ->
      # Check if path (without leading /) appears in text
      path_suffix = String.trim_leading(path, "/")

      if String.contains?(acc_text, path_suffix) do
        index = Enum.find_index(search_paths, &(&1 == path))
        marker = "$D#{index}"
        new_text = String.replace(acc_text, path_suffix, marker, global: false)
        extra = String.length(acc_text) - String.length(new_text)
        {:halt, {new_text, acc_remove + extra}}
      else
        {:cont, {acc_text, acc_remove}}
      end
    end)
  end

  defp get_real_path(file, search_paths) do
    if length(search_paths) == 1 do
      Path.join(List.first(search_paths), file)
    else
      Enum.reduce(search_paths, file, fn path, acc ->
        index = Enum.find_index(search_paths, &(&1 == path))
        marker = "$D#{index}"

        if String.contains?(acc, marker) do
          String.replace(acc, marker, path)
        else
          acc
        end
      end)
    end
  end
end
