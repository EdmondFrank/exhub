defmodule Exhub.MCP.ScratchpadStore do
  @moduledoc """
  Multi-session in-memory store backing the `think` and `plan` tools.

  The scratchpad was originally kept in the MCP session frame's `assigns`, which
  only survives when a request is served by `Anubis.Server.Session`. ExHub routes
  every `tools/call` through `Exhub.MCP.ConcurrentToolDispatcher`, which builds a
  fresh frame per request and discards the frame returned by the tool — so
  frame-backed state never accumulated (each call started from an empty list).

  This store fixes that by keying entries on the transport-independent
  `frame.context.session_id`, which is populated identically in both dispatch
  paths. Appends happen atomically inside the GenServer, so concurrent calls to
  the same session can't lose updates.

  ## Data model (per `{session_id, key}`)

      %{
        entries: [String.t()],   # bounded, oldest dropped first
        updated_at: DateTime.t()
      }

  Entries untouched for more than `@expiry_seconds` are removed by a periodic
  cleanup task, mirroring `Exhub.MCP.TodoStore`.
  """

  use GenServer

  require Logger

  @default_table :exhub_scratchpad_store
  # run cleanup every 30 minutes
  @cleanup_interval_ms 30 * 60 * 1_000
  # expire entries after 2 hours of inactivity
  @expiry_seconds 2 * 60 * 60
  # hard cap on retained entries per bucket (oldest dropped first)
  @max_entries 50

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc """
  Atomically appends `entry` to the list stored under `{session_id, key}`.

  Returns the updated, bounded list of entries. `server` defaults to the named
  process started in the supervision tree; pass an explicit pid/atom in tests.
  """
  @spec append(GenServer.server(), String.t() | nil, atom(), String.t()) :: [String.t()]
  def append(server \\ __MODULE__, session_id, key, entry)
      when is_atom(key) and is_binary(entry) do
    GenServer.call(server, {:append, session_id, key, entry})
  end

  @doc "Return the current entries for `{session_id, key}`, or `[]` if none."
  @spec entries(GenServer.server(), String.t() | nil, atom()) :: [String.t()]
  def entries(server \\ __MODULE__, session_id, key) when is_atom(key) do
    GenServer.call(server, {:entries, session_id, key})
  end

  @doc "Drop all scratchpad buckets belonging to `session_id`."
  @spec clear(GenServer.server(), String.t() | nil) :: :ok
  def clear(server \\ __MODULE__, session_id) do
    GenServer.call(server, {:clear, session_id})
  end

  # ---------------------------------------------------------------------------
  # GenServer callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(opts) do
    table = Keyword.get(opts, :table, @default_table)

    tbl =
      :ets.new(table, [:set, :public, :named_table, read_concurrency: true])

    schedule_cleanup()
    {:ok, %{table: tbl}}
  end

  @impl true
  def handle_call({:append, session_id, key, entry}, _from, state) do
    bucket = {session_id, key}
    existing = lookup(state.table, bucket)

    entries =
      (existing ++ [entry])
      |> Enum.take(-@max_entries)

    :ets.insert(state.table, {bucket, %{entries: entries, updated_at: DateTime.utc_now()}})
    {:reply, entries, state}
  end

  @impl true
  def handle_call({:entries, session_id, key}, _from, state) do
    {:reply, lookup(state.table, {session_id, key}), state}
  end

  @impl true
  def handle_call({:clear, session_id}, _from, state) do
    # Delete every bucket whose session_id matches (regardless of key).
    :ets.select_delete(state.table, [
      {{{session_id, :_}, :_}, [], [true]}
    ])

    {:reply, :ok, state}
  end

  @impl true
  def handle_info(:cleanup, state) do
    now = DateTime.utc_now()
    cutoff = DateTime.add(now, -@expiry_seconds, :second)

    expired =
      :ets.tab2list(state.table)
      |> Enum.filter(fn {_bucket, entry} ->
        DateTime.compare(entry.updated_at, cutoff) == :lt
      end)

    Enum.each(expired, fn {bucket, _} -> :ets.delete(state.table, bucket) end)

    if length(expired) > 0 do
      Logger.debug("[ScratchpadStore] Cleaned up #{length(expired)} expired bucket(s)")
    end

    schedule_cleanup()
    {:noreply, state}
  end

  # ---------------------------------------------------------------------------
  # Private helpers
  # ---------------------------------------------------------------------------

  defp lookup(table, bucket) do
    case :ets.lookup(table, bucket) do
      [{^bucket, %{entries: entries}}] -> entries
      [] -> []
    end
  end

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup, @cleanup_interval_ms)
  end
end
