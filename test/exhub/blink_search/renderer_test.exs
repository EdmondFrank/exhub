defmodule Exhub.BlinkSearch.RendererTest do
  use ExUnit.Case, async: true

  alias Exhub.BlinkSearch.Renderer

  # ---------------------------------------------------------------------------
  # Result aggregation
  # ---------------------------------------------------------------------------

  describe "update_backend_results/4" do
    test "builds flattened candidate items across backends" do
      state =
        Renderer.init_state()
        |> Map.put(:search_backend_list, ["Find File", "Buffer List"])
        |> Renderer.update_backend_results("Find File", ["a.ex", "b.ex"], "foo")
        |> Renderer.update_backend_results("Buffer List", ["*scratch*"], "foo")

      assert state.search_candidate_items == [
               %{backend: "Find File", candidate: "a.ex"},
               %{backend: "Find File", candidate: "b.ex"},
               %{backend: "Buffer List", candidate: "*scratch*"}
             ]

      # Focused backend items follow the first backend with results
      assert state.search_backend_items == ["a.ex", "b.ex"]
    end

    test "resets cursor when keyword changes" do
      state = Renderer.init_state()

      state =
        Renderer.update_backend_results(state, "Find File", ["x.ex"], "first")

      state = %{state | render_candidate_index: 0}

      state =
        Renderer.update_backend_results(
          %{state | search_keyword: "old"},
          "Find File",
          ["y.ex"],
          "new"
        )

      assert state.render_candidate_offset == 0
      assert state.render_candidate_index == 0
      assert state.render_backend_offset == 0
      assert state.render_backend_index == 0
    end

    test "clears everything when no backend has results" do
      state =
        Renderer.init_state()
        |> Map.put(:search_backend_list, ["Find File"])
        |> Renderer.update_backend_results("Find File", [], "")

      assert state.search_candidate_items == []
      assert state.render_candidate_items == []
      assert state.render_backend_items == []
    end

    test "limits per-backend candidates when multiple backends have results" do
      many = Enum.map(1..50, &"file-#{&1}.ex")

      state =
        Renderer.init_state()
        |> Map.put(:search_backend_list, ["A", "B"])
        |> Map.put(:row_number, 20)
        |> Renderer.update_backend_results("A", many, "")
        |> Renderer.update_backend_results("B", many, "")

      shown_backends =
        state.render_candidate_items
        |> Enum.map(& &1.backend)
        |> Enum.uniq()

      # Both backends stay represented in the visible slice
      unless length(shown_backends) >= 2 or state.render_candidate_items == [] do
        flunk("expected both backends represented: #{inspect(shown_backends)}")
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Candidate navigation
  # ---------------------------------------------------------------------------

  describe "select_next_candidate/1 and select_prev_candidate/1" do
    setup do
      items = Enum.map(1..40, &%{backend: "Find File", candidate: "c#{&1}"})

      render =
        Renderer.init_state()
        |> Map.put(:row_number, 10)
        |> Map.put(:search_candidate_items, items)
        |> Map.put(:render_candidate_items, Enum.take(items, 10))

      {:ok, render: render}
    end

    test "moves index down within the window", %{render: render} do
      state = Renderer.select_next_candidate(render)
      assert state.render_candidate_index == 1

      state = Renderer.select_prev_candidate(state)
      assert state.render_candidate_index == 0
    end

    test "scrolls offset at the bottom of the window", %{render: render} do
      state =
        render
        |> Map.put(:render_candidate_index, 9)
        |> Renderer.select_next_candidate()

      assert state.render_candidate_offset == 1
      assert state.render_candidate_index == 9
    end

    test "scrolls offset up at the top of the window", %{render: render} do
      state =
        render
        |> Map.put(:render_candidate_offset, 5)
        |> Renderer.select_prev_candidate()

      assert state.render_candidate_offset == 4
      assert state.render_candidate_index == 0
    end

    test "stops at the last item", %{render: render} do
      state =
        render
        |> Map.put(:render_candidate_offset, 30)
        |> Map.put(:render_candidate_index, 9)
        |> Renderer.select_next_candidate()

      assert state.render_candidate_offset == 30
      assert state.render_candidate_index == 9
    end

    test "stops at the first item", %{render: render} do
      state = Renderer.select_prev_candidate(render)
      assert state.render_candidate_offset == 0
      assert state.render_candidate_index == 0
    end

    test "no-ops on empty results" do
      state = Renderer.select_next_candidate(Renderer.init_state())
      assert state == Renderer.init_state()
    end
  end

  # ---------------------------------------------------------------------------
  # Group navigation
  # ---------------------------------------------------------------------------

  describe "select_next_group/1 and select_prev_group/1" do
    test "jumps to the start of the next backend group" do
      items = [
        %{backend: "A", candidate: "a1"},
        %{backend: "A", candidate: "a2"},
        %{backend: "B", candidate: "b1"},
        %{backend: "C", candidate: "c1"}
      ]

      state =
        Renderer.init_state()
        |> Map.put(:search_candidate_items, items)
        |> Renderer.select_next_group()

      assert state.render_candidate_index == 2

      state = Renderer.select_next_group(state)
      assert state.render_candidate_index == 3

      # No further group — stays put
      state = Renderer.select_next_group(state)
      assert state.render_candidate_index == 3
    end

    test "jumps back to previous group starts" do
      items = [
        %{backend: "A", candidate: "a1"},
        %{backend: "A", candidate: "a2"},
        %{backend: "B", candidate: "b1"},
        %{backend: "C", candidate: "c1"}
      ]

      state =
        Renderer.init_state()
        |> Map.put(:search_candidate_items, items)
        |> Map.put(:render_candidate_index, 3)
        |> Renderer.select_prev_group()

      assert state.render_candidate_index == 2

      state = Renderer.select_prev_group(state)
      assert state.render_candidate_index == 0

      # Already at first group — stays put
      state = Renderer.select_prev_group(state)
      assert state.render_candidate_index == 0
    end
  end

  # ---------------------------------------------------------------------------
  # Selection helpers
  # ---------------------------------------------------------------------------

  describe "selected_candidate/1" do
    test "returns backend and candidate at the cursor" do
      state = %{
        Renderer.init_state()
        | search_candidate_items: [
            %{backend: "A", candidate: "x"},
            %{backend: "B", candidate: "y"}
          ],
          render_candidate_index: 1
      }

      assert Renderer.selected_candidate(state) == {"B", "y"}
    end

    test "returns nil for empty results" do
      assert Renderer.selected_candidate(Renderer.init_state()) == nil
    end
  end

  # ---------------------------------------------------------------------------
  # Elisp payload
  # ---------------------------------------------------------------------------

  describe "build_elisp_payload/1" do
    test "emits a blink-search-exhub-update-items call" do
      state =
        Renderer.init_state()
        |> Map.put(:search_backend_list, ["Find File"])
        |> Map.put(:search_keyword, "foo")
        |> Renderer.update_backend_results(
          "Find File",
          [
            %{text: "src/a.ex:1:1: defmodule A", matches: [[16, 17]]},
            "plain.ex"
          ],
          "foo"
        )

      payload = Renderer.build_elisp_payload(state)

      assert payload =~ ~r/^\(blink-search-exhub-update-items /
      assert payload =~ ~s|:text "src/a.ex:1:1: defmodule A"|
      assert payload =~ ":matches ((16 17))"
      assert payload =~ ~s|:candidate "plain.ex"|
      assert payload =~ ~s|:backend "Find File"|
      assert String.ends_with?(payload, "2 1)")
    end

    test "escapes quotes and backslashes in candidate text" do
      state =
        Renderer.init_state()
        |> Map.put(:search_backend_list, ["Find File"])
        |> Renderer.update_backend_results("Find File", [~s(weird\\"name)], "")

      payload = Renderer.build_elisp_payload(state)
      assert payload =~ ~s|"weird\\\\\\"name"|
    end

    test "empty state renders empty lists" do
      payload = Renderer.build_elisp_payload(Renderer.init_state())
      assert payload == "(blink-search-exhub-update-items '() 0 '() 0 \"\" 0 0 0)"
    end
  end
end
