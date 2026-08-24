defmodule Exhub.BlinkSearch.Backends.FindFile do
  @moduledoc """
  Find File backend — searches files using `fd`.

  Uses `fd --regex` for fuzzy file search within the git project root
  or current directory. Falls back to directory listing when prefix is empty.
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @impl true
  def search_match(prefix, state) do
    search_path = Map.get(state, :search_path)
    search_dir = Map.get(state, :search_dir, System.user_home!())
    clean_prefix = String.replace(prefix, "*", "")

    words = String.split(clean_prefix, ~r/\s+/, trim: true)

    if words != [] and search_path do
      fd_bin = find_fd()

      if fd_bin do
        pattern = Enum.join(words, ".*")

        command = [
          fd_bin,
          "--regex",
          pattern,
          "--full-path",
          "--search-path",
          search_path
        ]

        results = Backend.get_process_result(command)

        Enum.map(results, fn path ->
          Path.relative_to(path, search_path)
        end)
      else
        []
      end
    else
      # Empty prefix — list directory contents
      case File.ls(search_dir) do
        {:ok, entries} -> entries
        _ -> []
      end
    end
  end

  @impl true
  def do_action(candidate, state) do
    search_path = Map.get(state, :search_path, "")
    text = Backend.candidate_text(candidate)
    full_path = Path.join(search_path, text)
    Exhub.send_message(~s|(blink-search-open-file #{Backend.elisp_quote(full_path)})|)
    :ok
  end

  @impl true
  def copy(candidate, state) do
    search_path = Map.get(state, :search_path, "")
    text = Backend.candidate_text(candidate)
    full_path = Path.join(search_path, text)
    Exhub.send_message(~s|(kill-new #{Backend.elisp_quote(full_path)})|)
    Exhub.send_message(~s|(message "[Blink-Search] Copy: #{Backend.escape_message(full_path)}")|)
    :ok
  end

  @impl true
  def parent(candidate, state) do
    search_path = Map.get(state, :search_path, "")
    text = Backend.candidate_text(candidate)
    full_path = Path.join(search_path, text)
    parent_dir = Path.dirname(full_path)
    Exhub.send_message(~s|(blink-search-open-file #{Backend.elisp_quote(parent_dir)})|)
    :ok
  end

  @impl true
  def continue_search(candidate, state) do
    search_path = Map.get(state, :search_path, "")
    text = Backend.candidate_text(candidate)
    candidate_path = Path.join(search_path, text)

    continue_path =
      if File.dir?(candidate_path) do
        candidate_path
      else
        Path.dirname(candidate_path)
      end

    {:ok, continue_path}
  end

  @impl true
  def record_name(candidate, state) do
    search_path = Map.get(state, :search_path, "")
    text = Backend.candidate_text(candidate)
    Path.join(search_path, text)
  end

  @impl true
  def init_dir(search_dir, state) do
    project_path = Backend.get_project_path(search_dir)

    state
    |> Map.put(:search_dir, search_dir)
    |> Map.put(:search_path, project_path)
  end

  # Find fd or fdfind binary
  defp find_fd do
    cond do
      System.find_executable("fd") -> "fd"
      System.find_executable("fdfind") -> "fdfind"
      true -> nil
    end
  end
end
