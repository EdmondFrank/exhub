defmodule Exhub.MCP.Agent.Store do
  @moduledoc """
  GenServer that manages running ACP agent clients.

  State is a map of `agent_id => entry` where each entry contains:
  - `client` — the `ExMCP.ACP.Client` pid
  - `agent_id` — string
  - `command` — list of strings (the command to spawn the agent)
  - `started_at` — DateTime
  - `status_text` — string (default "initialized")
  - `sessions` — MapSet of session_ids
  - `event_queues` — map of `session_id => list of events`
  - `waiters` — map of `session_id => list of {from, timer_ref}`
  - `pending_permissions` — map of `session_id => {tool_call, options, from}`
  """

  use GenServer

  # Client API

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  def register(agent_id, client, command) do
    GenServer.call(__MODULE__, {:register, agent_id, client, command})
  end

  def unregister(agent_id) do
    GenServer.call(__MODULE__, {:unregister, agent_id})
  end

  def get(agent_id) do
    GenServer.call(__MODULE__, {:get, agent_id})
  end

  def list do
    GenServer.call(__MODULE__, :list)
  end

  def add_session(agent_id, session_id) do
    GenServer.cast(__MODULE__, {:add_session, agent_id, session_id})
  end

  def remove_session(agent_id, session_id) do
    GenServer.cast(__MODULE__, {:remove_session, agent_id, session_id})
  end

  def set_status(agent_id, text) do
    GenServer.cast(__MODULE__, {:set_status, agent_id, text})
  end

  def push_event(agent_id, session_id, event) do
    GenServer.cast(__MODULE__, {:push_event, agent_id, session_id, event})
  end

  def pop_events(agent_id, session_id) do
    GenServer.call(__MODULE__, {:pop_events, agent_id, session_id})
  end

  def wait_for_events(agent_id, session_id, timeout_ms) do
    GenServer.call(__MODULE__, {:wait_for_events, agent_id, session_id, timeout_ms}, timeout_ms + 5000)
  end

  def set_pending_permission(agent_id, session_id, tool_call, options, from) do
    GenServer.cast(__MODULE__, {:set_pending_permission, agent_id, session_id, tool_call, options, from})
  end

  def resolve_permission(agent_id, session_id, option_id) do
    GenServer.call(__MODULE__, {:resolve_permission, agent_id, session_id, option_id})
  end

  def cancel_permission(agent_id, session_id) do
    GenServer.call(__MODULE__, {:cancel_permission, agent_id, session_id})
  end

  # Server callbacks

  @impl true
  def init(_opts) do
    {:ok, %{agents: %{}, monitors: %{}}}
  end

  @impl true
  def handle_call({:register, agent_id, client, command}, _from, state) do
    if Map.has_key?(state.agents, agent_id) do
      {:reply, {:error, :already_exists}, state}
    else
      entry = %{
        client: client,
        agent_id: agent_id,
        command: command,
        started_at: DateTime.utc_now(),
        status_text: "initialized",
        sessions: MapSet.new(),
        event_queues: %{},
        waiters: %{},
        pending_permissions: %{}
      }

      # Monitor the client process
      ref = Process.monitor(client)

      new_state =
        state
        |> put_in([:agents, agent_id], entry)
        |> put_in([:monitors, ref], agent_id)

      {:reply, {:ok, entry}, new_state}
    end
  end

  def handle_call({:unregister, agent_id}, _from, state) do
    case Map.get(state.agents, agent_id) do
      nil ->
        {:reply, {:error, :not_found}, state}

      entry ->
        # Cancel any waiters
        Enum.each(entry.waiters, fn {_session_id, waiters} ->
          Enum.each(waiters, fn {from, timer_ref} ->
            Process.cancel_timer(timer_ref)
            GenServer.reply(from, {:ok, []})
          end)
        end)

        new_state = update_in(state.agents, &Map.delete(&1, agent_id))
        {:reply, :ok, new_state}
    end
  end

  def handle_call({:get, agent_id}, _from, state) do
    case Map.get(state.agents, agent_id) do
      nil -> {:reply, {:error, :not_found}, state}
      entry -> {:reply, {:ok, entry}, state}
    end
  end

  def handle_call(:list, _from, state) do
    entries = Map.values(state.agents)
    {:reply, entries, state}
  end

  def handle_call({:pop_events, agent_id, session_id}, _from, state) do
    case get_in(state, [:agents, agent_id, :event_queues, session_id]) do
      nil ->
        {:reply, {:ok, []}, state}

      events ->
        new_state = put_in(state, [:agents, agent_id, :event_queues, session_id], [])
        {:reply, {:ok, events}, new_state}
    end
  end

  def handle_call({:wait_for_events, agent_id, session_id, timeout_ms}, from, state) do
    case get_in(state, [:agents, agent_id]) do
      nil ->
        {:reply, {:error, :not_found}, state}

      entry ->
        case get_in(entry, [:event_queues, session_id]) do
          events when is_list(events) and events != [] ->
            # Return immediately if events already queued
            new_state = put_in(state, [:agents, agent_id, :event_queues, session_id], [])
            {:reply, {:ok, events}, new_state}

          _ ->
            # Register a waiter
            timer_ref = Process.send_after(self(), {:timeout, session_id, agent_id}, timeout_ms)
            waiter = {from, timer_ref}
            new_state = update_in(state, [:agents, agent_id, :waiters, session_id], fn
              nil -> [waiter]
              waiters -> waiters ++ [waiter]
            end)
            {:noreply, new_state}
        end
    end
  end

  def handle_call({:resolve_permission, agent_id, session_id, option_id}, _from, state) do
    case get_in(state, [:agents, agent_id, :pending_permissions, session_id]) do
      nil ->
        {:reply, {:error, :not_found}, state}

      {_tool_call, _options, {pid, ref}} ->
        send(pid, {ref, {:ok, %{"optionId" => option_id}}})
        new_state = update_in(state, [:agents, agent_id, :pending_permissions], &Map.delete(&1, session_id))
        {:reply, {:ok, %{status: "granted", option_id: option_id}}, new_state}
    end
  end

  def handle_call({:cancel_permission, agent_id, session_id}, _from, state) do
    case get_in(state, [:agents, agent_id, :pending_permissions, session_id]) do
      nil ->
        {:reply, {:error, :not_found}, state}

      {_tool_call, _options, {pid, ref}} ->
        send(pid, {ref, {:ok, %{"cancelled" => true}}})
        new_state = update_in(state, [:agents, agent_id, :pending_permissions], &Map.delete(&1, session_id))
        {:reply, {:ok, %{status: "cancelled"}}, new_state}
    end
  end

  @impl true
  def handle_cast({:add_session, agent_id, session_id}, state) do
    new_state = update_in(state, [:agents, agent_id, :sessions], fn
      nil -> MapSet.new([session_id])
      sessions -> MapSet.put(sessions, session_id)
    end)
    {:noreply, new_state}
  end

  def handle_cast({:remove_session, agent_id, session_id}, state) do
    new_state =
      state
      |> update_in([:agents, agent_id, :sessions], &MapSet.delete(&1, session_id))
      |> update_in([:agents, agent_id, :event_queues], &Map.delete(&1, session_id))
      |> update_in([:agents, agent_id, :waiters], &Map.delete(&1, session_id))
      |> update_in([:agents, agent_id, :pending_permissions], &Map.delete(&1, session_id))

    {:noreply, new_state}
  end

  def handle_cast({:set_status, agent_id, text}, state) do
    new_state = put_in(state, [:agents, agent_id, :status_text], text)
    {:noreply, new_state}
  end

  def handle_cast({:push_event, agent_id, session_id, event}, state) do
    case get_in(state, [:agents, agent_id]) do
      nil ->
        {:noreply, state}

      entry ->
        # Check for waiters first
        case get_in(entry, [:waiters, session_id]) do
          [] ->
            # No waiters, just append to queue
            new_state = update_in(state, [:agents, agent_id, :event_queues, session_id], fn
              nil -> [event]
              events -> events ++ [event]
            end)
            {:noreply, new_state}

          nil ->
            # No waiters, just append to queue
            new_state = update_in(state, [:agents, agent_id, :event_queues, session_id], fn
              nil -> [event]
              events -> events ++ [event]
            end)
            {:noreply, new_state}

          [{from, timer_ref} | rest_waiters] ->
            # Pop the first waiter, cancel timer, and reply with accumulated events + new event
            Process.cancel_timer(timer_ref)

            existing_events = get_in(entry, [:event_queues, session_id]) || []
            all_events = existing_events ++ [event]

            GenServer.reply(from, {:ok, all_events})

            new_state =
              state
              |> put_in([:agents, agent_id, :event_queues, session_id], [])
              |> put_in([:agents, agent_id, :waiters, session_id], rest_waiters)

            {:noreply, new_state}
        end
    end
  end

  def handle_cast({:set_pending_permission, agent_id, session_id, tool_call, options, from}, state) do
    new_state = put_in(state, [:agents, agent_id, :pending_permissions, session_id], {tool_call, options, from})
    {:noreply, new_state}
  end

  @impl true
  def handle_info({:timeout, session_id, agent_id}, state) do
    case get_in(state, [:agents, agent_id, :waiters, session_id]) do
      nil ->
        {:noreply, state}

      waiters ->
        # Find and remove the timed-out waiter
        case Enum.split_with(waiters, fn {_from, timer_ref} ->
          # The timer that triggered this will already be expired
          Process.read_timer(timer_ref) == false
        end) do
          {[], _} ->
            {:noreply, state}

          {[{from, _timer_ref}], rest_waiters} ->
            GenServer.reply(from, {:ok, []})
            new_state = put_in(state, [:agents, agent_id, :waiters, session_id], rest_waiters)
            {:noreply, new_state}
        end
    end
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, state) do
    case Map.get(state.monitors, ref) do
      nil ->
        {:noreply, state}

      agent_id ->
        # Agent process died, clean up
        entry = Map.get(state.agents, agent_id)

        if entry do
          # Cancel any waiters
          Enum.each(entry.waiters, fn {_session_id, waiters} ->
            Enum.each(waiters, fn {from, timer_ref} ->
              Process.cancel_timer(timer_ref)
              GenServer.reply(from, {:ok, []})
            end)
          end)
        end

        new_state =
          state
          |> update_in([:agents], &Map.delete(&1, agent_id))
          |> update_in([:monitors], &Map.delete(&1, ref))

        {:noreply, new_state}
    end
  end
end
