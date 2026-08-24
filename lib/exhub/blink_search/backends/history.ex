defmodule Exhub.BlinkSearch.Backends.History do
  @moduledoc """
  History backend — searches previously executed actions.

  History is stored in a file with format: `candidateᛡBackendName`.
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @impl true
  def search_match(prefix, state) do
    history_path = Map.get(state, :history_path, default_history_path())
    clean_prefix = String.replace(prefix, "*", "")

    if File.exists?(history_path) and clean_prefix != "" do
      regex = Backend.fuzzy_regex(prefix)

      history_path
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.filter(fn line ->
        case String.split(line, "ᛡ", parts: 2) do
          [candidate, backend] ->
            Regex.match?(regex, String.downcase(candidate)) or
              Regex.match?(regex, String.downcase(backend))

          _ ->
            false
        end
      end)
      |> Enum.map(fn line ->
        case String.split(line, "ᛡ", parts: 2) do
          [candidate, backend] -> "#{candidate} [#{backend}]"
          _ -> line
        end
      end)
    else
      []
    end
  rescue
    _ -> []
  end

  @impl true
  def do_action(candidate, _state) do
    text = Backend.candidate_text(candidate)

    case parse_history_candidate(text) do
      {match_candidate, match_backend} ->
        # Delegate to the original backend's do action via the Server
        Exhub.BlinkSearch.Server.do_action(match_backend, match_candidate)

      :error ->
        :ok
    end

    :ok
  end

  @impl true
  def parent(candidate, _state) do
    text = Backend.candidate_text(candidate)

    case parse_history_candidate(text) do
      {match_candidate, match_backend} ->
        Exhub.BlinkSearch.Server.parent(match_backend, match_candidate)

      :error ->
        :ok
    end

    :ok
  end

  # Parse "candidate [BackendName]" format
  defp parse_history_candidate(text) do
    case Regex.run(~r/^(.+)\s+\[([^\[\]]+)\]$/, text) do
      [_, candidate, backend] -> {candidate, backend}
      _ -> :error
    end
  end

  defp default_history_path do
    Path.join([System.user_home!(), ".emacs.d", "blink-search", "history.txt"])
  end
end
