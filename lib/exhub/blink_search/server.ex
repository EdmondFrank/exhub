defmodule Exhub.BlinkSearch.Server do
  @moduledoc """
  GenServer coordinator for blink-search.

  Replaces the Python `BlinkSearch` class. Manages:
  - Backend registry and state
  - Concurrent search dispatch via Tasks
  - Result aggregation and rendering (delegates to `Renderer`)
  - Action dispatch (do, copy, parent, select, continue)
  - History recording
  - Emacs communication via `Exhub.send_message/1`

  ## Message flow

  1. Emacs sends `["func", ["blink-search", "search", keyword, row_number, backend_list]]`
  2. Server dispatches `search_match` to each backend in parallel Tasks
  3. Each Task sends `{:search_result, backend_name, candidates, keyword}` back
  4. Server aggregates via `Renderer.update_backend_results/4`
  5. Server sends elisp render payload to Emacs via `Exhub.send_message/1`
  """

  use GenServer
  require Logger

  alias Exhub.BlinkSearch.Backend
  alias Exhub.BlinkSearch.Renderer

  # ===========================================================================
  # Backend registry
  # ===========================================================================

  @default_backends [
    "History",
    "Buffer List",
    "Common Directory",
    "Find File",
    "Recent File",
    "IMenu",
    "Elisp Symbol",
    "Google Suggest",
    "Key Value"
  ]

  @backend_modules %{
    "Find File" => Exhub.BlinkSearch.Backends.FindFile,
    "Grep File" => Exhub.BlinkSearch.Backends.GrepFile,
    "Current Buffer" => Exhub.BlinkSearch.Backends.CurrentBuffer,
    "Buffer List" => Exhub.BlinkSearch.Backends.BufferList,
    "Recent File" => Exhub.BlinkSearch.Backends.RecentFile,
    "Elisp Symbol" => Exhub.BlinkSearch.Backends.ElispSymbol,
    "IMenu" => Exhub.BlinkSearch.Backends.IMenu,
    "History" => Exhub.BlinkSearch.Backends.History,
    "Common Directory" => Exhub.BlinkSearch.Backends.CommonDirectory,
    "Google Suggest" => Exhub.BlinkSearch.Backends.GoogleSuggest,
    "Key Value" => Exhub.BlinkSearch.Backends.KeyValueStore,
    "Grep PDF" => Exhub.BlinkSearch.Backends.GrepPDF,
    "PDF" => Exhub.BlinkSearch.Backends.PDF
  }

  # ===========================================================================
  # Public API
  # ===========================================================================

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Search across backends."
  def search(keyword, row_number, backend_list \\ []) do
    GenServer.cast(__MODULE__, {:search, keyword, row_number, backend_list})
  end

  @doc "Execute the primary action for a candidate."
  def do_action(backend_name, candidate) do
    GenServer.cast(__MODULE__, {:do_action, backend_name, candidate})
  end

  @doc "Copy candidate text."
  def copy(backend_name, candidate) do
    GenServer.cast(__MODULE__, {:copy, backend_name, candidate})
  end

  @doc "Navigate to parent context."
  def parent(backend_name, candidate) do
    GenServer.cast(__MODULE__, {:parent, backend_name, candidate})
  end

  @doc "Preview/select action."
  def select(backend_name, candidate) do
    GenServer.cast(__MODULE__, {:select, backend_name, candidate})
  end

  @doc "Continue search in a subdirectory."
  def continue_search(backend_name, candidate) do
    GenServer.cast(__MODULE__, {:continue_search, backend_name, candidate})
  end

  @doc "Select next candidate item."
  def select_next_candidate do
    GenServer.cast(__MODULE__, :select_next_candidate)
  end

  @doc "Select previous candidate item."
  def select_prev_candidate do
    GenServer.cast(__MODULE__, :select_prev_candidate)
  end

  @doc "Select next backend item."
  def select_next_backend do
    GenServer.cast(__MODULE__, :select_next_backend)
  end

  @doc "Select previous backend item."
  def select_prev_backend do
    GenServer.cast(__MODULE__, :select_prev_backend)
  end

  @doc "Select next candidate group."
  def select_next_group do
    GenServer.cast(__MODULE__, :select_next_group)
  end

  @doc "Select previous candidate group."
  def select_prev_group do
    GenServer.cast(__MODULE__, :select_prev_group)
  end

  @doc "Update backend data pushed from Emacs (buffer list, recent files, etc.)."
  def update_backend(backend_name, items) do
    GenServer.cast(__MODULE__, {:update_backend, backend_name, items})
  end

  @doc "Initialize the search directory."
  def init_search_dir(start_dir) do
    GenServer.cast(__MODULE__, {:init_search_dir, start_dir})
  end

  @doc "Initialize the current buffer for Current Buffer backend."
  def init_current_buffer(buffer_name, buffer_content_base64) do
    GenServer.cast(__MODULE__, {:init_current_buffer, buffer_name, buffer_content_base64})
  end

  @doc "Set common directories (alias + path pairs) pushed from Emacs."
  def set_common_directory(dirs) do
    GenServer.cast(__MODULE__, {:set_common_directory, dirs})
  end

  @doc "Set grep-pdf search paths pushed from Emacs."
  def set_grep_pdf_search_paths(paths) do
    GenServer.cast(__MODULE__, {:set_grep_pdf_search_paths, paths})
  end

  @doc "Clean up all backends."
  def clean do
    GenServer.cast(__MODULE__, :clean)
  end

  @doc "Get current render state (for debugging)."
  def get_state do
    GenServer.call(__MODULE__, :get_state)
  end

  # ===========================================================================
  # GenServer callbacks
  # ===========================================================================

  @impl true
  def init(_opts) do
    # Initialize backend states
    backend_states =
      Map.new(@backend_modules, fn {name, _module} ->
        {name, %{}}
      end)

    state = %{
      render: Renderer.init_state(),
      backend_states: backend_states,
      search_ticker: 0,
      start_buffer_name: "",
      history_path: default_history_path()
    }

    {:ok, state}
  end

  @impl true
  def handle_cast({:search, keyword, row_number, backend_list}, state) do
    ticker = state.search_ticker + 1

    # Determine active backends
    backend_list =
      if backend_list == [] do
        @default_backends
      else
        backend_list
      end

    # Update render state
    render = %{state.render | row_number: row_number, search_backend_list: backend_list}
    state = %{state | render: render, search_ticker: ticker}

    # Dispatch search to each backend in parallel
    parent = self()

    Enum.each(backend_list, fn backend_name ->
      module = Map.get(@backend_modules, backend_name)
      backend_state = Map.get(state.backend_states, backend_name, %{})

      if module do
        Task.start(fn ->
          try do
            candidates = module.search_match(keyword, backend_state)
            send(parent, {:search_result, backend_name, candidates, keyword, ticker})
          rescue
            e ->
              Logger.warning("Backend #{backend_name} search failed: #{inspect(e)}")
              send(parent, {:search_result, backend_name, [], keyword, ticker})
          end
        end)
      end
    end)

    {:noreply, state}
  end

  @impl true
  def handle_cast({:do_action, backend_name, candidate}, state) do
    module = Map.get(@backend_modules, backend_name)
    backend_state = action_state(state, backend_name)

    if module do
      # Record history in a separate task
      Task.start(fn ->
        record_history(backend_name, candidate, module, backend_state, state.history_path)
      end)

      module.do_action(candidate, backend_state)
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast({:copy, backend_name, candidate}, state) do
    module = Map.get(@backend_modules, backend_name)
    backend_state = Map.get(state.backend_states, backend_name, %{})

    if module do
      module.copy(candidate, backend_state)
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast({:parent, backend_name, candidate}, state) do
    module = Map.get(@backend_modules, backend_name)
    backend_state = Map.get(state.backend_states, backend_name, %{})

    if module do
      module.parent(candidate, backend_state)
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast({:select, backend_name, candidate}, state) do
    module = Map.get(@backend_modules, backend_name)
    backend_state = action_state(state, backend_name)

    if module do
      module.select(candidate, backend_state)
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast({:continue_search, backend_name, candidate}, state) do
    module = Map.get(@backend_modules, backend_name)
    backend_state = Map.get(state.backend_states, backend_name, %{})

    if module do
      case module.continue_search(candidate, backend_state) do
        {:ok, new_dir} ->
          Exhub.send_message(~s|(blink-search-continue-search #{Backend.elisp_quote(new_dir)})|)

        :error ->
          Exhub.send_message(
            ~s|(message "[Blink-Search] #{Backend.escape_message(backend_name)} does not support continue search.")|
          )
      end
    end

    {:noreply, state}
  end

  @impl true
  def handle_cast(:select_next_candidate, state) do
    render = Renderer.select_next_candidate(state.render)
    send_render_to_emacs(render)
    {:noreply, %{state | render: render}}
  end

  @impl true
  def handle_cast(:select_prev_candidate, state) do
    render = Renderer.select_prev_candidate(state.render)
    send_render_to_emacs(render)
    {:noreply, %{state | render: render}}
  end

  @impl true
  def handle_cast(:select_next_backend, state) do
    render = Renderer.select_next_backend(state.render)
    send_render_to_emacs(render)

    # Also trigger select action for the newly focused backend item
    case Renderer.selected_backend_item(render) do
      {backend_name, candidate} ->
        module = Map.get(@backend_modules, backend_name)
        backend_state = Map.get(state.backend_states, backend_name, %{})
        if module, do: module.select(candidate, backend_state)

      nil ->
        :ok
    end

    {:noreply, %{state | render: render}}
  end

  @impl true
  def handle_cast(:select_prev_backend, state) do
    render = Renderer.select_prev_backend(state.render)
    send_render_to_emacs(render)

    case Renderer.selected_backend_item(render) do
      {backend_name, candidate} ->
        module = Map.get(@backend_modules, backend_name)
        backend_state = Map.get(state.backend_states, backend_name, %{})
        if module, do: module.select(candidate, backend_state)

      nil ->
        :ok
    end

    {:noreply, %{state | render: render}}
  end

  @impl true
  def handle_cast(:select_next_group, state) do
    render = Renderer.select_next_group(state.render)
    send_render_to_emacs(render)
    {:noreply, %{state | render: render}}
  end

  @impl true
  def handle_cast(:select_prev_group, state) do
    render = Renderer.select_prev_group(state.render)
    send_render_to_emacs(render)
    {:noreply, %{state | render: render}}
  end

  @impl true
  def handle_cast({:update_backend, backend_name, items}, state) do
    module = Map.get(@backend_modules, backend_name)
    backend_state = Map.get(state.backend_states, backend_name, %{})

    new_backend_state =
      if module do
        module.update(items, backend_state)
      else
        Map.put(backend_state, :items, items)
      end

    backend_states = Map.put(state.backend_states, backend_name, new_backend_state)
    {:noreply, %{state | backend_states: backend_states}}
  end

  @impl true
  def handle_cast({:init_search_dir, start_dir}, state) do
    backend_states =
      Enum.reduce(@backend_modules, state.backend_states, fn {name, module}, acc ->
        backend_state = Map.get(acc, name, %{})

        if function_exported?(module, :init_dir, 2) do
          Map.put(acc, name, module.init_dir(start_dir, backend_state))
        else
          acc
        end
      end)

    {:noreply, %{state | backend_states: backend_states}}
  end

  @impl true
  def handle_cast({:init_current_buffer, buffer_name, buffer_content_base64}, state) do
    backend_state = Map.get(state.backend_states, "Current Buffer", %{})

    new_backend_state =
      Exhub.BlinkSearch.Backends.CurrentBuffer.init_buffer(
        buffer_name,
        buffer_content_base64,
        backend_state
      )

    backend_states = Map.put(state.backend_states, "Current Buffer", new_backend_state)

    {:noreply, %{state | backend_states: backend_states, start_buffer_name: buffer_name}}
  end

  @impl true
  def handle_cast({:set_common_directory, dirs}, state) do
    backend_state =
      state.backend_states
      |> Map.get("Common Directory", %{})
      |> Map.put(:common_directory, dirs)

    backend_states = Map.put(state.backend_states, "Common Directory", backend_state)
    {:noreply, %{state | backend_states: backend_states}}
  end

  @impl true
  def handle_cast({:set_grep_pdf_search_paths, paths}, state) do
    backend_state =
      state.backend_states
      |> Map.get("Grep PDF", %{})
      |> Map.put(:grep_pdf_search_paths, paths)

    backend_states = Map.put(state.backend_states, "Grep PDF", backend_state)
    {:noreply, %{state | backend_states: backend_states}}
  end

  @impl true
  def handle_cast(:clean, state) do
    backend_states =
      Enum.reduce(@backend_modules, state.backend_states, fn {name, module}, acc ->
        backend_state = Map.get(acc, name, %{})
        Map.put(acc, name, module.clean(backend_state))
      end)

    {:noreply, %{state | backend_states: backend_states}}
  end

  @impl true
  def handle_call(:get_state, _from, state) do
    {:reply, state, state}
  end

  @impl true
  def handle_info({:search_result, backend_name, candidates, keyword, ticker}, state) do
    # Only process results from the latest search (ticker check)
    if ticker == state.search_ticker do
      render =
        Renderer.update_backend_results(state.render, backend_name, candidates, keyword)

      send_render_to_emacs(render)
      {:noreply, %{state | render: render}}
    else
      # Stale result, ignore
      {:noreply, state}
    end
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  # ===========================================================================
  # Private helpers
  # ===========================================================================

  # Backend state enriched for action dispatch: exposes the current search
  # keyword as `:match_text` (mirrors Python backends storing the prefix).
  defp action_state(state, backend_name) do
    state.backend_states
    |> Map.get(backend_name, %{})
    |> Map.put(:match_text, state.render.search_keyword)
  end

  defp send_render_to_emacs(render) do
    elisp = Renderer.build_elisp_payload(render)
    Exhub.send_message(elisp)
  end

  defp record_history(backend_name, candidate, module, backend_state, history_path) do
    # Skip history for certain backends (matching Python behavior)
    if backend_name not in ["History", "PDF", "Current Buffer", "IMenu", "Grep File"] do
      record_name = module.record_name(candidate, backend_state)
      history_item = "#{record_name}ᛡ#{backend_name}"

      # Ensure file exists
      unless File.exists?(history_path) do
        history_path |> Path.dirname() |> File.mkdir_p!()
        File.touch!(history_path)
      end

      # Append if not already present
      content = File.read!(history_path)
      lines = String.split(content, "\n", trim: true)

      unless Enum.any?(lines, &(&1 == history_item)) do
        File.write!(history_path, history_item <> "\n", [:append])
      end
    end
  rescue
    e -> Logger.warning("Failed to record history: #{inspect(e)}")
  end

  defp default_history_path do
    Path.join([System.user_home!(), ".emacs.d", "blink-search", "history.txt"])
  end
end
