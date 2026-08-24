defmodule Exhub.BlinkSearch.Backends.RecentFile do
  @moduledoc """
  Recent File backend — searches recently opened files.

  Data is pushed from Emacs via `update/2` (recentf-list).
  """

  use Exhub.BlinkSearch.Backend

  @impl true
  def search_match(prefix, state) do
    items = Map.get(state, :items, [])
    Exhub.BlinkSearch.Backend.filter_match(items, prefix)
  end

  @impl true
  def do_action(candidate, _state) do
    text = Exhub.BlinkSearch.Backend.candidate_text(candidate)
    Exhub.send_message(~s|(find-file #{Exhub.BlinkSearch.Backend.elisp_quote(text)})|)
    :ok
  end

  @impl true
  def parent(candidate, _state) do
    text = Exhub.BlinkSearch.Backend.candidate_text(candidate)
    parent_dir = Path.dirname(text)
    Exhub.send_message(~s|(blink-search-open-file #{Exhub.BlinkSearch.Backend.elisp_quote(parent_dir)})|)
    :ok
  end
end
