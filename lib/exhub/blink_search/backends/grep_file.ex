defmodule Exhub.BlinkSearch.Backends.GrepFile do
  @moduledoc """
  Grep File backend — searches file contents using `ripgrep`.

  Uses `rg --json` for structured output with match positions.
  Streams results in batches for progressive display.
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @impl true
  def search_match(prefix, state) do
    search_path = Map.get(state, :search_path, System.user_home!())
    clean_prefix = String.replace(prefix, "*", "")
    words = String.split(clean_prefix, ~r/\s+/, trim: true)

    if words == [] do
      []
    else
      rg_bin = System.find_executable("rg")

      if rg_bin do
        pattern = Enum.join(words, ".*")

        command = [
          rg_bin,
          "--json",
          "-S",
          "--max-columns",
          "300",
          "-g",
          "!node_modules",
          "-g",
          "!__pycache__",
          "-g",
          "!dist",
          pattern,
          Path.expand(search_path)
        ]

        # Collect all results (streaming would require GenServer integration)
        Backend.get_process_result(command)
        |> Enum.map(fn line -> Backend.parse_rg_line(line, search_path) end)
        |> Enum.reject(&is_nil/1)
        |> Enum.map(fn result ->
          if String.length(result.text) < 1000 do
            result
          else
            %{text: String.slice(result.text, 0, 1000), matches: [hd(result.matches)]}
          end
        end)
      else
        []
      end
    end
  end

  @impl true
  def do_action(candidate, state) do
    search_path = Map.get(state, :search_path, "")
    text = Backend.candidate_text(candidate)

    case String.split(text, ":", parts: 4) do
      [file, line, col, _rest] ->
        full_path = Path.join(search_path, file)

        Exhub.send_message(
          ~s|(blink-search-grep-file-do #{Backend.elisp_quote(full_path)} #{line} #{col})|
        )

      _ ->
        :ok
    end

    :ok
  end

  @impl true
  def select(candidate, state) do
    search_path = Map.get(state, :search_path, "")
    text = Backend.candidate_text(candidate)

    case String.split(text, ":", parts: 4) do
      [file, line, col, _rest] ->
        full_path = Path.join(search_path, file)

        Exhub.send_message(
          ~s|(blink-search-grep-file-preview #{Backend.elisp_quote(full_path)} #{line} #{col})|
        )

      _ ->
        :ok
    end

    :ok
  end

  @impl true
  def copy(candidate, _state) do
    text = Backend.candidate_text(candidate)
    # Copy only the content after the last colon prefix
    copy_text = text |> String.split(":") |> List.last() |> String.trim()
    Exhub.send_message(~s|(kill-new #{Backend.elisp_quote(copy_text)})|)
    Exhub.send_message(~s|(message "[Blink-Search] Copy: #{Backend.escape_message(copy_text)}")|)
    :ok
  end

  @impl true
  def parent(candidate, state) do
    search_path = Map.get(state, :search_path, "")
    text = Backend.candidate_text(candidate)

    case String.split(text, ":", parts: 4) do
      [file | _] ->
        full_path = Path.join(search_path, file)
        parent_dir = Path.dirname(full_path)
        Exhub.send_message(~s|(blink-search-open-file #{Backend.elisp_quote(parent_dir)})|)

      _ ->
        :ok
    end

    :ok
  end

  @impl true
  def clean(state) do
    Exhub.send_message("(blink-search-grep-file-clean)")
    state
  end

  @impl true
  def init_dir(search_dir, state) do
    project_path = Backend.get_project_path(search_dir)
    Map.put(state, :search_path, project_path)
  end
end
