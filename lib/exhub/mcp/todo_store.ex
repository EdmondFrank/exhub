defmodule Exhub.MCP.TodoStore do
  @moduledoc """
  Multi-tenant in-memory store for todo lists.

  Each tenant (identified by a `tenant_id` string) has an isolated todo list.
  Todo lists that have not been updated for more than 2 hours are automatically
  removed by a periodic cleanup task.

  ## Data model (per tenant)

      %{
        items: [%{name: String.t(), completed: boolean()}],
        initial_user_prompt: String.t(),
        updated_at: DateTime.t()
      }
  """

  use GenServer

  require Logger

  @table :todo_store
  @cleanup_interval_ms 30 * 60 * 1_000   # run cleanup every 30 minutes
  @expiry_seconds 2 * 60 * 60             # expire entries after 2 hours

  # ---------------------------------------------------------------------------
  # Client API
  # ---------------------------------------------------------------------------

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc "Overwrite (or create) the todo list for a tenant."
  def set_todos(server \\ __MODULE__, tenant_id, items, initial_user_prompt) do
    GenServer.call(server, {:set_todos, tenant_id, items, initial_user_prompt})
  end

  @doc "Return the todo list for a tenant, or {:error, :not_found}."
  def get_todos(server \\ __MODULE__, tenant_id) do
    GenServer.call(server, {:get_todos, tenant_id})
  end

  @doc "Update the `completed` flag of a single item by name."
  def update_item(server \\ __MODULE__, tenant_id, item_name, completed) do
    GenServer.call(server, {:update_item, tenant_id, item_name, completed})
  end

  @doc "Remove all items for a tenant (keeps the entry with an empty list)."
  def clear_todos(server \\ __MODULE__, tenant_id) do
    GenServer.call(server, {:clear_todos, tenant_id})
  end

  # ---------------------------------------------------------------------------
  # GenServer callbacks
  # ---------------------------------------------------------------------------

  @impl true
  def init(_opts) do
    table = :ets.new(@table, [:set, :protected, :named_table, read_concurrency: true])
    schedule_cleanup()
    {:ok, %{table: table}}
  end

  @impl true
  def handle_call({:set_todos, tenant_id, items, initial_user_prompt}, _from, state) do
    entry = %{
      items: normalise_items(items),
      initial_user_prompt: initial_user_prompt || "",
      updated_at: DateTime.utc_now()
    }

    :ets.insert(state.table, {tenant_id, entry})
    {:reply, :ok, state}
  end

  @impl true
  def handle_call({:get_todos, tenant_id}, _from, state) do
    case :ets.lookup(state.table, tenant_id) do
      [{^tenant_id, entry}] -> {:reply, {:ok, entry}, state}
      [] -> {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call({:update_item, tenant_id, item_name, completed}, _from, state) do
    case :ets.lookup(state.table, tenant_id) do
      [{^tenant_id, entry}] ->
        updated_items =
          Enum.map(entry.items, fn item ->
            if item.name == item_name, do: %{item | completed: completed}, else: item
          end)

        new_entry = %{entry | items: updated_items, updated_at: DateTime.utc_now()}
        :ets.insert(state.table, {tenant_id, new_entry})
        {:reply, {:ok, new_entry}, state}

      [] ->
        {:reply, {:error, :not_found}, state}
    end
  end

  @impl true
  def handle_call({:clear_todos, tenant_id}, _from, state) do
    case :ets.lookup(state.table, tenant_id) do
      [{^tenant_id, entry}] ->
        new_entry = %{entry | items: [], updated_at: DateTime.utc_now()}
        :ets.insert(state.table, {tenant_id, new_entry})
        {:reply, :ok, state}

      [] ->
        # Nothing to clear — treat as success
        :ets.insert(state.table, {tenant_id, %{items: [], initial_user_prompt: "", updated_at: DateTime.utc_now()}})
        {:reply, :ok, state}
    end
  end

  @impl true
  def handle_info(:cleanup, state) do
    now = DateTime.utc_now()
    cutoff = DateTime.add(now, -@expiry_seconds, :second)

    expired =
      :ets.tab2list(state.table)
      |> Enum.filter(fn {_tenant_id, entry} ->
        DateTime.compare(entry.updated_at, cutoff) == :lt
      end)

    Enum.each(expired, fn {tenant_id, _} ->
      Logger.debug("[TodoStore] Removing expired todo list for tenant: #{tenant_id}")
      :ets.delete(state.table, tenant_id)
    end)

    if length(expired) > 0 do
      Logger.info("[TodoStore] Cleaned up #{length(expired)} expired todo list(s)")
    end

    schedule_cleanup()
    {:noreply, state}
  end

  # ---------------------------------------------------------------------------
  # Private helpers
  # ---------------------------------------------------------------------------

  defp schedule_cleanup do
    Process.send_after(self(), :cleanup, @cleanup_interval_ms)
  end

  defp normalise_items(items) when is_list(items) do
    Enum.map(items, fn
      %{name: name, completed: completed} -> %{name: name, completed: completed}
      %{"name" => name, "completed" => completed} -> %{name: name, completed: completed}
      %{name: name} -> %{name: name, completed: false}
      %{"name" => name} -> %{name: name, completed: false}
      other -> %{name: inspect(other), completed: false}
    end)
  end

  defp normalise_items(_), do: []
end
