defmodule Exhub.BlinkSearch.Backends.ElispSymbol do
  @moduledoc """
  Elisp Symbol backend — searches Emacs Lisp symbols.

  Data is pushed from Emacs via `update/2` (list of symbol names).
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @impl true
  def search_match(prefix, state) do
    items = Map.get(state, :items, [])
    Backend.filter_match(items, prefix)
  end

  @impl true
  def do_action(candidate, _state) do
    text = Backend.candidate_text(candidate)
    Exhub.send_message(~s|(blink-search-elisp-symbol-do #{Backend.elisp_quote(text)})|)
    :ok
  end

  @impl true
  def update(items, state) do
    # Sort by length (shorter first), matching Python behavior
    sorted = Enum.sort(items, fn a, b -> String.length(a) <= String.length(b) end)
    Map.put(state, :items, sorted)
  end
end
