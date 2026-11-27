defmodule Exhub.LspBridge.Ledger do
  @moduledoc """
  Out-of-band record of the diagnostics language servers have reported.

  The Emacs front end renders diagnostics as they arrive and forgets them when
  a buffer closes; a headless client (the `lsp` MCP server) has no buffer to
  render into. The Ledger keeps the last error-level diagnostics per file, so
  an agent can ask "what did I break that I have not looked at yet" long after
  the edit — the proactive half of the LSP surface.

  It is an ordinary `GenServer`:
  one of the `Exhub.LspBridge.Session` owners, and also attached to
  editor-owned sessions via `Exhub.LspBridge.Session.subscribe/2`, so both
  paths feed the same table. Entries are capped per file and bounded in count;
  the table is deliberately in-memory (diagnostics are cheap to recompute, and
  a stale ledger across restarts would be worse than none).
  """

  use GenServer

  @table __MODULE__
  @max_entries 512
  @max_diagnostics 20

  @typedoc "One file's last reported diagnostics."
  @type entry :: %{
          path: String.t(),
          count: non_neg_integer(),
          errors: non_neg_integer(),
          warnings: non_neg_integer(),
          hints: non_neg_integer(),
          updated_at: integer(),
          diagnostics: [map()]
        }

  # ===========================================================================
  # Public API
  # ===========================================================================

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Record the merged diagnostics for `path` (errors are kept in full)."
  @spec record(Path.t(), [map()]) :: :ok
  def record(path, diagnostics) when is_list(diagnostics) do
    GenServer.call(__MODULE__, {:record, Path.expand(path), diagnostics})
  end

  @doc "The stored entry for `path`, or `nil`."
  @spec get(Path.t()) :: entry() | nil
  def get(path) do
    case :ets.whereis(@table) do
      :undefined ->
        nil

      _ ->
        case :ets.lookup(@table, Path.expand(path)) do
          [{_path, entry}] -> entry
          [] -> nil
        end
    end
  catch
    :error, :badarg -> nil
  end

  @doc "All entries, most recently updated first."
  @spec all() :: [entry()]
  def all do
    case :ets.whereis(@table) do
      :undefined ->
        []

      _ ->
        @table
        |> :ets.tab2list()
        |> Enum.map(fn {_path, entry} -> entry end)
        |> Enum.sort_by(& &1.updated_at, :desc)
    end
  catch
    :error, :badarg -> []
  end

  @doc "Aggregate counts for a `lsp_status`-style summary."
  @spec summary() :: map()
  def summary do
    entries = all()

    updated_at =
      case entries do
        [] -> nil
        [newest | _] -> newest.updated_at
      end

    %{
      files: length(entries),
      errors: entries |> Enum.map(& &1.errors) |> Enum.sum(),
      warnings: entries |> Enum.map(& &1.warnings) |> Enum.sum(),
      updated_at: updated_at
    }
  end

  @doc "Forget one file (e.g. after it was fixed and re-diagnosed clean)."
  @spec clear(Path.t()) :: :ok
  def clear(path), do: GenServer.call(__MODULE__, {:clear, Path.expand(path)})

  @doc "Forget everything."
  @spec reset() :: :ok
  def reset, do: GenServer.call(__MODULE__, :reset)

  # ===========================================================================
  # GenServer callbacks
  # ===========================================================================

  @impl true
  def init(_opts) do
    table =
      case :ets.whereis(@table) do
        :undefined -> :ets.new(@table, [:named_table, :set, :public, read_concurrency: true])
        tid -> tid
      end

    {:ok, %{table: table}}
  end

  @impl true
  def handle_call({:record, path, diagnostics}, _from, state) do
    store(path, diagnostics)
    {:reply, :ok, state}
  end

  def handle_call({:clear, path}, _from, state) do
    :ets.delete(@table, path)
    {:reply, :ok, state}
  end

  def handle_call(:reset, _from, state) do
    :ets.delete_all_objects(@table)
    {:reply, :ok, state}
  end

  # Owners/subscribers deliver the session's messages here; the Ledger only
  # cares about diagnostics and ignores the rest.
  @impl true
  def handle_info({:lsp_diagnostics_update, path, diagnostics, _count}, state) do
    store(Path.expand(path), diagnostics)
    {:noreply, state}
  end

  def handle_info(_message, state), do: {:noreply, state}

  # ===========================================================================
  # Internals
  # ===========================================================================

  defp store(path, diagnostics) do
    {errors, warnings, hints} = count_severities(diagnostics)
    error_list = Enum.filter(diagnostics, &(severity(&1) == 1))

    entry = %{
      path: path,
      count: length(diagnostics),
      errors: errors,
      warnings: warnings,
      hints: hints,
      updated_at: System.system_time(:millisecond),
      diagnostics: Enum.take(error_list, @max_diagnostics)
    }

    :ets.insert(@table, {path, entry})
    evict()
    :ok
  end

  defp count_severities(diagnostics) do
    Enum.reduce(diagnostics, {0, 0, 0}, fn diagnostic, {e, w, h} ->
      case severity(diagnostic) do
        1 -> {e + 1, w, h}
        2 -> {e, w + 1, h}
        _ -> {e, w, h + 1}
      end
    end)
  end

  defp severity(diagnostic), do: Map.get(diagnostic, "severity", 1)

  defp evict do
    size = :ets.info(@table, :size)

    if size > @max_entries do
      @table
      |> :ets.tab2list()
      |> Enum.sort_by(fn {_path, entry} -> entry.updated_at end)
      |> Enum.take(size - @max_entries)
      |> Enum.each(fn {path, _entry} -> :ets.delete(@table, path) end)
    end
  end
end
