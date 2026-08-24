defmodule Exhub.ResponseHandlers.ExhubBlinkSearch do
  @moduledoc """
  WebSocket response handler for blink-search commands from Emacs.

  Dispatches `["func", ["blink-search", action, ...args]]` messages
  to the `Exhub.BlinkSearch.Server` GenServer.

  ## Supported actions

  - `"search"` — search across backends
  - `"do"` — execute primary action
  - `"copy"` — copy candidate text
  - `"parent"` — navigate to parent
  - `"select"` — preview/select action
  - `"continue"` — continue search in subdirectory
  - `"select_next_candidate"` / `"select_prev_candidate"` — candidate navigation
  - `"select_next_backend"` / `"select_prev_backend"` — backend item navigation
  - `"select_next_group"` / `"select_prev_group"` — group navigation
  - `"update"` — update backend data from Emacs
  - `"init_search_dir"` — set search directory
  - `"init_current_buffer"` — set current buffer content
  - `"init_common_directory"` — set common directories (alias + path pairs)
  - `"init_grep_pdf_paths"` — set grep-pdf search paths
  - `"clean"` — clean up all backends
  """

  alias Exhub.BlinkSearch.Server

  def call(["blink-search", "search", keyword, row_number, backend_list]) do
    backend_list = if is_list(backend_list), do: backend_list, else: []
    row_number = if is_integer(row_number), do: row_number, else: 20
    Server.search(to_string(keyword), row_number, Enum.map(backend_list, &to_string/1))
    nil
  end

  def call(["blink-search", "do", backend_name, candidate]) do
    Server.do_action(to_string(backend_name), candidate)
    nil
  end

  def call(["blink-search", "copy", backend_name, candidate]) do
    Server.copy(to_string(backend_name), candidate)
    nil
  end

  def call(["blink-search", "parent", backend_name, candidate]) do
    Server.parent(to_string(backend_name), candidate)
    nil
  end

  def call(["blink-search", "select", backend_name, candidate]) do
    Server.select(to_string(backend_name), candidate)
    nil
  end

  def call(["blink-search", "continue", backend_name, candidate]) do
    Server.continue_search(to_string(backend_name), candidate)
    nil
  end

  def call(["blink-search", "select_next_candidate"]) do
    Server.select_next_candidate()
    nil
  end

  def call(["blink-search", "select_prev_candidate"]) do
    Server.select_prev_candidate()
    nil
  end

  def call(["blink-search", "select_next_backend"]) do
    Server.select_next_backend()
    nil
  end

  def call(["blink-search", "select_prev_backend"]) do
    Server.select_prev_backend()
    nil
  end

  def call(["blink-search", "select_next_group"]) do
    Server.select_next_group()
    nil
  end

  def call(["blink-search", "select_prev_group"]) do
    Server.select_prev_group()
    nil
  end

  def call(["blink-search", "update", backend_name, items]) when is_list(items) do
    Server.update_backend(to_string(backend_name), items)
    nil
  end

  def call(["blink-search", "init_search_dir", start_dir]) do
    Server.init_search_dir(to_string(start_dir))
    nil
  end

  def call(["blink-search", "init_current_buffer", buffer_name, buffer_content]) do
    Server.init_current_buffer(to_string(buffer_name), to_string(buffer_content))
    nil
  end

  def call(["blink-search", "init_common_directory", dirs]) when is_list(dirs) do
    normalized =
      Enum.flat_map(dirs, fn
        [name, path] -> [[to_string(name), to_string(path)]]
        %{"name" => name, "path" => path} -> [[to_string(name), to_string(path)]]
        {name, path} when is_binary(name) and is_binary(path) -> [[name, path]]
        _ -> []
      end)

    Server.set_common_directory(normalized)
    nil
  end

  def call(["blink-search", "init_grep_pdf_paths", paths]) when is_list(paths) do
    Server.set_grep_pdf_search_paths(Enum.map(paths, &to_string/1))
    nil
  end

  def call(["blink-search", "clean"]) do
    Server.clean()
    nil
  end

  # Catch-all for unknown blink-search actions
  def call(["blink-search" | rest]) do
    require Logger
    Logger.warning("Unknown blink-search action: #{inspect(rest)}")
    nil
  end
end
