defmodule Exhub.BlinkSearch.Backends.CommonDirectory do
  @moduledoc """
  Common Directory backend — searches configured common directories.

  Lists directory contents under configured aliases (e.g. HOME ~/).
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @impl true
  def search_match(prefix, state) do
    common_directory = Map.get(state, :common_directory, [{"HOME", "~/"}])
    clean_prefix = String.replace(prefix, "*", "")
    regex = Backend.fuzzy_regex(prefix)

    if is_list(common_directory) and common_directory != [] do
      Enum.flat_map(common_directory, fn
        [alias_name, directory] ->
          search_directory(alias_name, directory, clean_prefix, regex)

        {alias_name, directory} ->
          search_directory(alias_name, directory, clean_prefix, regex)

        _ ->
          []
      end)
      |> Enum.sort()
    else
      []
    end
  end

  @impl true
  def do_action(candidate, state) do
    text = Backend.candidate_text(candidate)

    case get_candidate_dir(text, state) do
      nil -> :ok
      dir ->
        Exhub.send_message(~s|(blink-search-open-file #{Backend.elisp_quote(dir)})|)
    end

    :ok
  end

  @impl true
  def parent(candidate, state) do
    text = Backend.candidate_text(candidate)

    case get_candidate_dir(text, state) do
      nil -> :ok
      dir ->
        parent_dir = Path.dirname(dir)
        Exhub.send_message(~s|(blink-search-open-file #{Backend.elisp_quote(parent_dir)})|)
    end

    :ok
  end

  @impl true
  def continue_search(candidate, state) do
    text = Backend.candidate_text(candidate)

    case get_candidate_dir(text, state) do
      nil -> :error
      dir -> {:ok, dir}
    end
  end

  # Private helpers

  defp search_directory(alias_name, directory, prefix, regex) do
    expanded = Path.expand(directory)

    case File.ls(expanded) do
      {:ok, entries} ->
        Enum.filter(entries, fn path ->
          combined = "#{String.downcase(alias_name)} #{String.downcase(path)}"
          Backend.is_match?(prefix, regex, combined)
        end)
        |> Enum.map(fn path -> "#{alias_name} #{path}" end)

      _ ->
        []
    end
  end

  defp get_candidate_dir(candidate_text, state) do
    common_directory = Map.get(state, :common_directory, [{"HOME", "~/"}])
    [prefix | rest] = String.split(candidate_text, " ", parts: 2)
    remainder = List.first(rest, "")

    Enum.find_value(common_directory, fn
      [alias_name, directory] ->
        if prefix == alias_name do
          Path.join(Path.expand(directory), remainder)
        end

      {alias_name, directory} ->
        if prefix == alias_name do
          Path.join(Path.expand(directory), remainder)
        end

      _ ->
        nil
    end)
  end
end
