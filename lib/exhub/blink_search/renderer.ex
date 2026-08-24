defmodule Exhub.BlinkSearch.Renderer do
  @moduledoc """
  Pure rendering logic for blink-search candidate/backend pagination.

  Mirrors the Python `BlinkSearch` class's rendering methods:
  - `message_handler` → `aggregate_results/4`
  - `render_items` → `build_render_payload/1`
  - `select_next/prev_candidate_item` → `select_next_candidate/1` / `select_prev_candidate/1`
  - `select_next/prev_backend_item` → `select_next_backend/1` / `select_prev_backend/1`
  - `select_next/prev_candidate_group` → `select_next_group/1` / `select_prev_group/1`

  All functions operate on a `render_state` map and return updated state.
  The Server GenServer holds this state and calls `send_to_emacs/1` to push updates.
  """

  alias Exhub.BlinkSearch.Backend

  @typedoc """
  Render state tracked by the Server.

  - `:search_dict` — map of backend_name => [candidate]
  - `:search_backend_list` — ordered list of active backend names
  - `:search_keyword` — current search keyword
  - `:row_number` — visible rows in the candidate window
  - `:search_candidate_items` — flattened list of %{backend: name, candidate: cand}
  - `:search_backend_items` — full candidate list for the focused backend
  - `:render_candidate_items` — visible slice of candidate items
  - `:render_backend_items` — visible slice of backend items
  - `:render_candidate_index` — cursor index within visible candidates
  - `:render_backend_index` — cursor index within visible backend items
  - `:render_candidate_offset` — scroll offset for candidates
  - `:render_backend_offset` — scroll offset for backend items
  """
  @type render_state :: %{
          search_dict: %{String.t() => [Backend.candidate()]},
          search_backend_list: [String.t()],
          search_keyword: String.t(),
          row_number: pos_integer(),
          search_candidate_items: [%{backend: String.t(), candidate: Backend.candidate()}],
          search_backend_items: [Backend.candidate()],
          render_candidate_items: [%{backend: String.t(), candidate: Backend.candidate()}],
          render_backend_items: [Backend.candidate()],
          render_candidate_index: non_neg_integer(),
          render_backend_index: non_neg_integer(),
          render_candidate_offset: non_neg_integer(),
          render_backend_offset: non_neg_integer()
        }

  @doc "Create a fresh render state."
  @spec init_state() :: render_state()
  def init_state do
    %{
      search_dict: %{},
      search_backend_list: [],
      search_keyword: "",
      row_number: 20,
      search_candidate_items: [],
      search_backend_items: [],
      render_candidate_items: [],
      render_backend_items: [],
      render_candidate_index: 0,
      render_backend_index: 0,
      render_candidate_offset: 0,
      render_backend_offset: 0
    }
  end

  # ===========================================================================
  # Result Aggregation (replaces Python message_handler)
  # ===========================================================================

  @doc """
  Update the search dict with new backend results and recompute render items.

  Called when a backend finishes searching. `keyword_changed` indicates whether
  the search keyword changed since the last update (resets cursor to top).
  """
  @spec update_backend_results(render_state(), String.t(), [Backend.candidate()], String.t()) ::
          render_state()
  def update_backend_results(state, backend_name, items, keyword) do
    search_dict = Map.put(state.search_dict, backend_name, items)
    keyword_changed = keyword != state.search_keyword

    state = %{state | search_dict: search_dict, search_keyword: keyword}

    # Build flattened candidate items across all active backends
    candidate_counter =
      Enum.reduce(state.search_backend_list, 0, fn name, acc ->
        acc + backend_candidate_count(search_dict, name)
      end)

    candidate_items =
      Enum.flat_map(state.search_backend_list, fn name ->
        candidates = Map.get(search_dict, name, [])

        if candidates != [] do
          show_number =
            if length(state.search_backend_list) > 1 do
              if candidate_counter < state.row_number do
                length(candidates)
              else
                max(5, div(state.row_number, max(candidate_counter, 1)))
              end
            else
              length(candidates)
            end

          Enum.take(candidates, min(length(candidates), show_number))
          |> Enum.map(fn cand -> %{backend: name, candidate: cand} end)
        else
          []
        end
      end)

    if candidate_items != [] do
      first_backend = hd(candidate_items).backend
      backend_items = Map.get(search_dict, first_backend, [])

      state = %{
        state
        | search_candidate_items: candidate_items,
          search_backend_items: backend_items
      }

      if keyword_changed do
        # Reset cursor to top on new keyword
        %{
          state
          | render_candidate_items: Enum.take(candidate_items, state.row_number),
            render_backend_items: Enum.take(backend_items, state.row_number),
            render_candidate_offset: 0,
            render_candidate_index: 0,
            render_backend_offset: 0,
            render_backend_index: 0
        }
      else
        # Preserve cursor position
        state
        |> update_render_candidate_items()
        |> update_render_index_and_offset()
      end
    else
      # No results — clear everything
      %{
        state
        | search_candidate_items: [],
          search_backend_items: [],
          render_candidate_items: [],
          render_backend_items: [],
          render_candidate_offset: 0,
          render_candidate_index: 0,
          render_backend_offset: 0,
          render_backend_index: 0
      }
    end
  end

  # ===========================================================================
  # Candidate Navigation
  # ===========================================================================

  @doc "Move cursor to the next candidate item."
  @spec select_next_candidate(render_state()) :: render_state()
  def select_next_candidate(state) do
    items = state.search_candidate_items

    if items == [] do
      state
    else
      total = length(items)
      abs_index = state.render_candidate_offset + state.render_candidate_index

      cond do
        state.render_candidate_index <
            min(state.row_number, length(state.render_candidate_items)) - 1 ->
          state = %{state | render_candidate_index: state.render_candidate_index + 1}
          state |> update_render_candidate_items() |> update_render_index_and_offset()

        abs_index >= total - 1 ->
          # Already at the end
          state

        true ->
          state = %{state | render_candidate_offset: state.render_candidate_offset + 1}
          state |> update_render_candidate_items() |> update_render_index_and_offset()
      end
    end
  end

  @doc "Move cursor to the previous candidate item."
  @spec select_prev_candidate(render_state()) :: render_state()
  def select_prev_candidate(state) do
    items = state.search_candidate_items

    if items == [] do
      state
    else
      cond do
        state.render_candidate_index > 0 ->
          state = %{state | render_candidate_index: state.render_candidate_index - 1}
          state |> update_render_candidate_items() |> update_render_index_and_offset()

        state.render_candidate_offset == 0 and state.render_candidate_index == 0 ->
          # Already at the beginning
          state

        true ->
          state = %{state | render_candidate_offset: state.render_candidate_offset - 1}
          state |> update_render_candidate_items() |> update_render_index_and_offset()
      end
    end
  end

  # ===========================================================================
  # Backend Item Navigation (within focused backend)
  # ===========================================================================

  @doc "Move cursor to the next item within the focused backend."
  @spec select_next_backend(render_state()) :: render_state()
  def select_next_backend(state) do
    items = state.search_backend_items

    if items == [] do
      state
    else
      total = length(items)
      abs_index = state.render_backend_offset + state.render_backend_index

      cond do
        state.render_backend_index <
            min(state.row_number - 1, length(state.render_backend_items)) - 1 ->
          %{state | render_backend_index: state.render_backend_index + 1}

        abs_index >= total - 1 ->
          state

        true ->
          state = %{state | render_backend_offset: state.render_backend_offset + 1}
          update_render_backend_items(state)
      end
    end
  end

  @doc "Move cursor to the previous item within the focused backend."
  @spec select_prev_backend(render_state()) :: render_state()
  def select_prev_backend(state) do
    items = state.search_backend_items

    if items == [] do
      state
    else
      cond do
        state.render_backend_index > 0 ->
          %{state | render_backend_index: state.render_backend_index - 1}

        state.render_backend_offset == 0 and state.render_backend_index == 0 ->
          state

        true ->
          state = %{state | render_backend_offset: state.render_backend_offset - 1}
          update_render_backend_items(state)
      end
    end
  end

  # ===========================================================================
  # Group Navigation (jump between backend groups)
  # ===========================================================================

  @doc "Jump to the next backend group in the candidate list."
  @spec select_next_group(render_state()) :: render_state()
  def select_next_group(state) do
    items = state.search_candidate_items

    if items == [] do
      state
    else
      candidate_index = state.render_candidate_offset + state.render_candidate_index
      group_list = candidate_group_list(items)

      case Enum.filter(group_list, &(&1 > candidate_index)) do
        [] ->
          state

        [next_index | _] ->
          state =
            if state.render_candidate_index == 0 and state.render_candidate_offset == 0 do
              %{state | render_candidate_index: next_index}
            else
              if next_index >= state.render_candidate_offset and
                   next_index < state.render_candidate_offset + state.row_number do
                %{state | render_candidate_index: next_index - state.render_candidate_offset}
              else
                %{
                  state
                  | render_candidate_index: state.row_number - 1,
                    render_candidate_offset: next_index - (state.row_number - 1)
                }
              end
            end

          state
          |> update_render_candidate_items()
          |> update_render_index_and_offset()
      end
    end
  end

  @doc "Jump to the previous backend group in the candidate list."
  @spec select_prev_group(render_state()) :: render_state()
  def select_prev_group(state) do
    items = state.search_candidate_items

    if items == [] do
      state
    else
      candidate_index = state.render_candidate_offset + state.render_candidate_index
      group_list = candidate_group_list(items)

      case Enum.filter(group_list, &(&1 < candidate_index)) do
        [] ->
          state

        match_list ->
          next_index = List.last(match_list)

          state =
            if state.render_candidate_index == 0 and state.render_candidate_offset == 0 do
              %{state | render_candidate_index: next_index}
            else
              if next_index >= state.render_candidate_offset and
                   next_index < state.render_candidate_offset + state.row_number do
                %{state | render_candidate_index: next_index - state.render_candidate_offset}
              else
                %{
                  state
                  | render_candidate_offset: next_index,
                    render_candidate_index: 0
                }
              end
            end

          state
          |> update_render_candidate_items()
          |> update_render_index_and_offset()
      end
    end
  end

  # ===========================================================================
  # Get currently selected candidate
  # ===========================================================================

  @doc "Get the currently selected candidate item (backend name + candidate)."
  @spec selected_candidate(render_state()) :: {String.t(), Backend.candidate()} | nil
  def selected_candidate(state) do
    abs_index = state.render_candidate_offset + state.render_candidate_index

    case Enum.at(state.search_candidate_items, abs_index) do
      %{backend: backend, candidate: candidate} -> {backend, candidate}
      _ -> nil
    end
  end

  @doc "Get the currently selected backend item (for backend panel navigation)."
  @spec selected_backend_item(render_state()) :: {String.t(), Backend.candidate()} | nil
  def selected_backend_item(state) do
    abs_index = state.render_candidate_offset + state.render_candidate_index

    case Enum.at(state.search_candidate_items, abs_index) do
      %{backend: backend} ->
        backend_abs = state.render_backend_offset + state.render_backend_index

        case Enum.at(state.search_backend_items, backend_abs) do
          nil -> nil
          candidate -> {backend, candidate}
        end

      _ ->
        nil
    end
  end

  # ===========================================================================
  # Build elisp payload for Emacs
  # ===========================================================================

  @doc """
  Build the elisp expression string to send to Emacs for rendering.

  Produces: `(blink-search-exhub-update-items '(...) idx '(...) idx "name" item_idx total backend_count)`
  """
  @spec build_elisp_payload(render_state()) :: String.t()
  def build_elisp_payload(state) do
    {backend_name, candidate_index} =
      case selected_candidate(state) do
        {name, _} ->
          abs_backend_index = state.render_backend_offset + state.render_backend_index + 1
          {name, abs_backend_index}

        nil ->
          {"", 0}
      end

    candidate_items_elisp = render_candidate_items_to_elisp(state.render_candidate_items)
    backend_items_elisp = render_backend_items_to_elisp(state.render_backend_items)

    items_number = length(state.search_backend_items)
    backend_number = length(state.search_backend_list)

    "(blink-search-exhub-update-items #{candidate_items_elisp} #{state.render_candidate_index} " <>
      "#{backend_items_elisp} #{state.render_backend_index} " <>
      "#{Backend.elisp_quote(backend_name)} #{candidate_index} " <>
      "#{items_number} #{backend_number})"
  end

  # ===========================================================================
  # Private helpers
  # ===========================================================================

  defp backend_candidate_count(search_dict, name) do
    case Map.get(search_dict, name) do
      nil -> 0
      items -> length(items)
    end
  end

  defp update_render_candidate_items(state) do
    render_items =
      Enum.slice(
        state.search_candidate_items,
        state.render_candidate_offset,
        state.row_number
      )

    # Update backend items to match the focused backend
    backend_items =
      case Enum.at(render_items, state.render_candidate_index) do
        %{backend: name} -> Map.get(state.search_dict, name, [])
        _ -> state.search_backend_items
      end

    %{state | render_candidate_items: render_items, search_backend_items: backend_items}
  end

  defp update_render_index_and_offset(state) do
    abs_index = state.render_candidate_offset + state.render_candidate_index

    case Enum.at(state.search_candidate_items, abs_index) do
      %{candidate: candidate} ->
        backend_index = find_candidate_index(state.search_backend_items, candidate)

        if backend_index >= state.row_number do
          render_backend_items =
            Enum.slice(
              state.search_backend_items,
              backend_index - (state.row_number - 1),
              state.row_number
            )

          %{
            state
            | render_backend_offset: backend_index - (state.row_number - 1),
              render_backend_index: state.row_number - 1,
              render_backend_items: render_backend_items
          }
        else
          render_backend_items = Enum.take(state.search_backend_items, state.row_number)

          %{
            state
            | render_backend_offset: 0,
              render_backend_index: backend_index,
              render_backend_items: render_backend_items
          }
        end

      _ ->
        render_backend_items = Enum.take(state.search_backend_items, state.row_number)

        %{
          state
          | render_backend_offset: 0,
            render_backend_index: 0,
            render_backend_items: render_backend_items
        }
    end
  end

  defp update_render_backend_items(state) do
    render_items =
      Enum.slice(
        state.search_backend_items,
        state.render_backend_offset,
        state.row_number
      )

    %{state | render_backend_items: render_items}
  end

  defp find_candidate_index(items, candidate) do
    case Enum.find_index(items, &(&1 == candidate)) do
      nil -> 0
      idx -> idx
    end
  end

  defp candidate_group_list(candidate_items) do
    candidate_items
    |> Enum.with_index()
    |> Enum.reduce({[], ""}, fn {%{backend: backend}, index}, {acc, last_backend} ->
      if backend != last_backend do
        {acc ++ [index], backend}
      else
        {acc, last_backend}
      end
    end)
    |> elem(0)
  end

  # ===========================================================================
  # Elisp serialization
  # ===========================================================================

  defp render_candidate_items_to_elisp(items) do
    elisp_items =
      Enum.map(items, fn %{backend: backend, candidate: candidate} ->
        text = Backend.candidate_text(candidate)
        matches = Backend.candidate_matches(candidate)

        candidate_elisp =
          if matches do
            matches_elisp =
              matches
              |> Enum.map(fn [s, e] -> "(#{s} #{e})" end)
              |> Enum.join(" ")

            "(:text #{Backend.elisp_quote(text)} :matches (#{matches_elisp}))"
          else
            Backend.elisp_quote(text)
          end

        "(:backend #{Backend.elisp_quote(backend)} :candidate #{candidate_elisp})"
      end)

    "'(#{Enum.join(elisp_items, " ")})"
  end

  defp render_backend_items_to_elisp(items) do
    elisp_items =
      Enum.map(items, fn candidate ->
        text = Backend.candidate_text(candidate)
        matches = Backend.candidate_matches(candidate)

        if matches do
          matches_elisp =
            matches
            |> Enum.map(fn [s, e] -> "(#{s} #{e})" end)
            |> Enum.join(" ")

          "(:text #{Backend.elisp_quote(text)} :matches (#{matches_elisp}))"
        else
          Backend.elisp_quote(text)
        end
      end)

    "'(#{Enum.join(elisp_items, " ")})"
  end
end
