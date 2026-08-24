defmodule Exhub.BlinkSearch.Backends.BufferList do
  @moduledoc """
  Buffer List backend — searches Emacs buffer names.

  Data is pushed from Emacs via `update/2`. Supports special prefix filters:
  - `*` prefix: show only `*special*` buffers
  - ` *` prefix: show only hidden ` *internal*` buffers
  - `*` suffix: show only modified buffers
  """

  use Exhub.BlinkSearch.Backend

  @impl true
  def search_match(prefix, state) do
    items = Map.get(state, :items, [])
    clean_prefix = String.replace(prefix, "*", "")
    regex = Exhub.BlinkSearch.Backend.fuzzy_regex(prefix)

    match_items = Enum.filter(items, &Exhub.BlinkSearch.Backend.is_match?(clean_prefix, regex, &1))

    cond do
      String.starts_with?(prefix, "*") ->
        Enum.filter(match_items, &String.starts_with?(&1, "*"))

      String.starts_with?(prefix, " *") ->
        Enum.filter(match_items, &String.starts_with?(&1, " *"))

      String.ends_with?(prefix, "*") ->
        Enum.filter(match_items, &String.ends_with?(&1, "*"))

      true ->
        match_items
    end
  end

  @impl true
  def do_action(candidate, _state) do
    text = Exhub.BlinkSearch.Backend.candidate_text(candidate)
    Exhub.send_message(~s|(switch-to-buffer #{Exhub.BlinkSearch.Backend.elisp_quote(text)})|)
    :ok
  end

  @impl true
  def update(items, state) do
    # Sort: non-hidden buffers first, then alphabetically
    sorted =
      Enum.sort(items, fn a, b ->
        a_hidden = String.starts_with?(a, " *")
        b_hidden = String.starts_with?(b, " *")

        cond do
          not a_hidden and b_hidden -> true
          a_hidden and not b_hidden -> false
          true -> a < b
        end
      end)

    Map.put(state, :items, sorted)
  end
end
