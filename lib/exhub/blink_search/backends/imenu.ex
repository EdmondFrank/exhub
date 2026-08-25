defmodule Exhub.BlinkSearch.Backends.IMenu do
  @moduledoc """
  IMenu backend — searches imenu symbols (functions, variables) of the current buffer.

  Data is pushed from Emacs as a list of [name, position] pairs.
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @impl true
  def search_match(prefix, state) do
    item_dict = Map.get(state, :item_dict, %{})
    keys = Map.keys(item_dict)
    Backend.filter_match(keys, prefix)
  end

  @impl true
  def do_action(candidate, state) do
    item_dict = Map.get(state, :item_dict, %{})
    text = Backend.candidate_text(candidate)

    case Map.get(item_dict, text) do
      nil -> :ok
      position ->
        Exhub.send_message(~s|(blink-search-imenu-do #{position})|)
    end

    :ok
  end

  @impl true
  def update(items, state) do
    # items is a list of [name, position] pairs from Emacs.
    # When Emacs sends [["name", pos], ...], json-encode converts it to
    # {"name": [pos], ...} (alist-to-object), which we then convert back
    # to [name, [pos]] pairs in the handler. Handle both forms.
    item_dict =
      Enum.reduce(items, %{}, fn
        [name, [position]], acc -> Map.put(acc, to_string(name), position)
        [name, position], acc -> Map.put(acc, to_string(name), position)
        %{"name" => name, "position" => position}, acc -> Map.put(acc, to_string(name), position)
        _, acc -> acc
      end)

    state
    |> Map.put(:items, Enum.map(items, fn
      [name, _pos] -> to_string(name)
      %{"name" => name} -> to_string(name)
      other -> to_string(other)
    end))
    |> Map.put(:item_dict, item_dict)
  end
end
