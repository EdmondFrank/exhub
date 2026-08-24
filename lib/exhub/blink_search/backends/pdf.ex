defmodule Exhub.BlinkSearch.Backends.PDF do
  @moduledoc """
  PDF backend — searches the current PDF file using `rga`.

  Activated when the current buffer is a PDF file and `rga` is available.
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
        |> Enum.map(fn line -> parse_rga_line(line) end)
        |> Enum.reject(&is_nil/1)
      else
        []
      end
    end
  end

  @impl true
  def do_action(candidate, state) do
    search_path = List.first(Map.get(state, :search_paths, [])) || ""
    match_text = candidate_match_text(candidate, state)
    text = Backend.candidate_text(candidate)

    case String.split(text, ":", parts: 2) do
      [line, _rest] ->
        Exhub.send_message(
          ~s|(blink-search-pdf-do #{Backend.elisp_quote(search_path)} #{line} #{Backend.elisp_quote(match_text)})|
        )

      _ ->
        :ok
    end

    :ok
  end

  @impl true
  def select(candidate, state) do
    search_path = List.first(Map.get(state, :search_paths, [])) || ""
    text = Backend.candidate_text(candidate)
    match_text = candidate_match_text(candidate, state)

    case String.split(text, ":", parts: 2) do
      [line, _rest] ->
        Exhub.send_message(
          ~s|(blink-search-pdf-preview #{Backend.elisp_quote(search_path)} #{line} #{Backend.elisp_quote(match_text)})|
        )

      _ ->
        :ok
    end

    :ok
  end

  @impl true
  def clean(state) do
    Exhub.send_message("(blink-search-pdf-clean)")
    state
  end

  @impl true
  def init_dir(search_dir, state) do
    Map.put(state, :search_paths, [search_dir])
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

  defp parse_rga_line(line) do
    result = Backend.parse_rg_line(line)

    if result do
      # Remove "None:...: Page " prefix from rga output
      cleaned_text = Regex.replace(@rga_page_pattern, result.text, "")
      remove_len = String.length(result.text) - String.length(cleaned_text)

      adjusted_matches =
        Enum.map(result.matches, fn [s, e] ->
          [s - remove_len, e - remove_len]
        end)

      # Extract match text
      match_text =
        case adjusted_matches do
          [[s, e] | _] when s >= 0 and e >= 0 ->
            safe_s = max(s, 0)
            safe_e = min(e, String.length(cleaned_text))
            if safe_e > safe_s, do: String.slice(cleaned_text, safe_s, safe_e - safe_s), else: ""

          _ ->
            ""
        end

      %{text: cleaned_text, matches: adjusted_matches, match_text: match_text}
    else
      nil
    end
  rescue
    _ -> nil
  end
end
