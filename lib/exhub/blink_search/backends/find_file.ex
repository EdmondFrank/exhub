defmodule Exhub.BlinkSearch.Backends.FindFile do
  @moduledoc """
  Find File backend — searches files using `fd`.

  Uses `fd --regex` for fuzzy file search within the git project root
  or current directory. Falls back to directory listing when prefix is empty.

  Results for an identical `{search_path, pattern}` pair are cached briefly in
  ETS, and results are capped via `--max-results`, so a keystroke burst never
  turns into a storm of full-tree `fd` scans (each spawn triggers a sandboxd
  TCC attribution query — see the tccd high-CPU investigation).
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @cache_table :exhub_blink_search_find_file_cache
  @fd_max_results 500

  @impl true
  def search_match(prefix, state) do
    search_path = Map.get(state, :search_path)
    search_dir = Map.get(state, :search_dir, System.user_home!())
    clean_prefix = String.replace(prefix, "*", "")

    words = String.split(clean_prefix, ~r/\s+/, trim: true)

    if words != [] and search_path do
      pattern = Enum.join(words, ".*")
      cache_key = {search_path, pattern}

      case cache_get(cache_key) do
        {:ok, results} ->
          results

        :miss ->
          results = run_fd(pattern, search_path)
          cache_put(cache_key, results)
          results
      end
    else
      # Empty prefix — list directory contents
      case File.ls(search_dir) do
        {:ok, entries} -> entries
        _ -> []
      end
    end
  end

  defp run_fd(pattern, search_path) do
    if fd_bin = find_fd() do
      command = [
        fd_bin,
        "--regex",
        pattern,
        "--full-path",
        "--max-results",
        to_string(@fd_max_results),
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

  # ---------------------------------------------------------------------------
  # Result cache
  # ---------------------------------------------------------------------------

  @doc "Clear the fd result cache (tests and manual refresh)."
  @spec clear_cache() :: :ok
  def clear_cache do
    if :ets.whereis(@cache_table) != :undefined do
      :ets.delete_all_objects(@cache_table)
    end

    :ok
  end

  defp cache_get(key) do
    if :ets.whereis(@cache_table) == :undefined do
      :miss
    else
      case :ets.lookup(@cache_table, key) do
        [{^key, expires_at, value}] ->
          if System.monotonic_time(:millisecond) < expires_at do
            {:ok, value}
          else
            :ets.delete(@cache_table, key)
            :miss
          end

        [] ->
          :miss
      end
    end
  end

  defp cache_put(key, value) do
    ensure_cache_table()

    ttl = Application.get_env(:exhub, :blink_search_find_file_cache_ttl_ms, 5_000)

    :ets.insert(@cache_table, {key, System.monotonic_time(:millisecond) + ttl, value})
    :ok
  end

  defp ensure_cache_table do
    if :ets.whereis(@cache_table) == :undefined do
      try do
        :ets.new(@cache_table, [:set, :named_table, :public, read_concurrency: true])
      rescue
        # Another process created it concurrently
        ArgumentError -> :ok
      end
    end

    :ok
  end

  # Find fd or fdfind binary — resolved once and cached in the ETS table
  # (System.find_executable/1 walks PATH on every call otherwise).
  defp find_fd do
    ensure_cache_table()

    case :ets.lookup(@cache_table, :fd_bin) do
      [{:fd_bin, cached}] ->
        cached

      [] ->
        fd =
          cond do
            System.find_executable("fd") -> "fd"
            System.find_executable("fdfind") -> "fdfind"
            true -> nil
          end

        :ets.insert(@cache_table, {:fd_bin, fd})
        fd
    end
  end
end
