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
  - `terminal_waiters` — map of `session_id => {from, timer_ref}` for waiting on terminal events
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

  def wait_for_terminal_event(agent_id, session_id, timeout_ms) do
    GenServer.call(__MODULE__, {:wait_for_terminal_event, agent_id, session_id, timeout_ms}, timeout_ms + 10_000)
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

  def collect_events(agent_id, session_id, collect_ms, idle_ms) do
    GenServer.call(__MODULE__, {:collect_events, agent_id, session_id, collect_ms, idle_ms}, collect_ms + 10_000)
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
        terminal_waiters: %{},
        collect_waiters: %{},
        pending_permissions: %{},
        tool_call_seen: %{}
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
            GenServer.reply(from, {:ok, merge_chunks([])})
          end)
        end)

        # Cancel any terminal waiters (skip nil entries — already consumed)
        Enum.each(entry.terminal_waiters, fn
          {_session_id, nil} -> :ok
          {_session_id, {from, timer_ref}} ->
            Process.cancel_timer(timer_ref)
            GenServer.reply(from, {:ok, merge_chunks([])})
        end)

        # Cancel any collect waiters
        Enum.each(entry.collect_waiters, fn
          {_session_id, nil} -> :ok
          {_session_id, {from, collect_ref, idle_ref, _idle_ms, acc}} ->
            Process.cancel_timer(collect_ref)
            Process.cancel_timer(idle_ref)
            GenServer.reply(from, {:ok, merge_chunks(acc)})
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
        {:reply, {:ok, merge_chunks(events)}, new_state}
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
            {:reply, {:ok, merge_chunks(events)}, new_state}

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

  def handle_call({:collect_events, agent_id, session_id, collect_ms, idle_ms}, from, state) do
    case get_in(state, [:agents, agent_id]) do
      nil ->
        {:reply, {:error, :not_found}, state}

      _entry ->
        existing = get_in(state, [:agents, agent_id, :event_queues, session_id]) || []

        if has_terminal_event?(existing) do
          new_state = put_in(state, [:agents, agent_id, :event_queues, session_id], [])
          {:reply, {:ok, merge_chunks(existing)}, new_state}
        else
          collect_ref = Process.send_after(self(), {:collect_timeout, session_id, agent_id}, collect_ms)
          idle_ref = Process.send_after(self(), {:idle_timeout, session_id, agent_id}, idle_ms)

          new_state =
            state
            |> put_in([:agents, agent_id, :collect_waiters, session_id], {from, collect_ref, idle_ref, idle_ms, existing})
            |> put_in([:agents, agent_id, :event_queues, session_id], [])

          {:noreply, new_state}
        end
    end
  end

  def handle_call({:wait_for_terminal_event, agent_id, session_id, timeout_ms}, from, state) do
    case get_in(state, [:agents, agent_id]) do
      nil ->
        {:reply, {:error, :not_found}, state}

      entry ->
        existing_events = get_in(entry, [:event_queues, session_id]) || []

        # Check if there's already a terminal event in the queue
        if has_terminal_event?(existing_events) do
          new_state = put_in(state, [:agents, agent_id, :event_queues, session_id], [])
          {:reply, {:ok, merge_chunks(existing_events)}, new_state}
        else
          # Register a terminal waiter
          timer_ref = Process.send_after(self(), {:terminal_timeout, session_id, agent_id}, timeout_ms)
          new_state = put_in(state, [:agents, agent_id, :terminal_waiters, session_id], {from, timer_ref})
          {:noreply, new_state}
        end
    end
  end

  defp has_terminal_event?(events) do
    Enum.any?(events, fn event ->
      event.type in ["complete", "error"]
    end)
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
      |> update_in([:agents, agent_id, :collect_waiters], &Map.delete(&1, session_id))
      |> update_in([:agents, agent_id, :pending_permissions], &Map.delete(&1, session_id))
      |> update_in([:agents, agent_id, :tool_call_seen], &Map.delete(&1, session_id))

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
        # Step 0: deduplicate tool_call_update events
        {event, state} = maybe_dedup_tool_call(event, session_id, state, agent_id)

        # Step 1: handle regular waiters → produce state1
        state1 =
          case get_in(entry, [:waiters, session_id]) do
            waiters when waiters in [nil, []] ->
              # No waiters — append event to queue
              update_in(state, [:agents, agent_id, :event_queues, session_id], fn
                nil -> [event]
                events -> events ++ [event]
              end)

            [{from, timer_ref} | rest_waiters] ->
              # Satisfy the first waiter immediately
              Process.cancel_timer(timer_ref)
              existing_events = get_in(entry, [:event_queues, session_id]) || []
              all_events = existing_events ++ [event]
              GenServer.reply(from, {:ok, merge_chunks(all_events)})

              state
              |> put_in([:agents, agent_id, :event_queues, session_id], [])
              |> put_in([:agents, agent_id, :waiters, session_id], rest_waiters)
          end

        # Step 2: handle terminal waiters using state1 → produce state2
        state2 =
          if is_terminal_event?(event) do
            case get_in(state1, [:agents, agent_id, :terminal_waiters, session_id]) do
              nil ->
                state1

              {from, timer_ref} ->
                Process.cancel_timer(timer_ref)
                # Drain the full queue from state1 (already includes this event if no regular waiter took it)
                queued = get_in(state1, [:agents, agent_id, :event_queues, session_id]) || []
                GenServer.reply(from, {:ok, merge_chunks(queued)})

                state1
                |> put_in([:agents, agent_id, :event_queues, session_id], [])
                |> put_in([:agents, agent_id, :terminal_waiters, session_id], nil)
            end
          else
            state1
          end

        {:noreply, state2}
    end
  end

  def handle_cast({:set_pending_permission, agent_id, session_id, tool_call, options, from}, state) do
    new_state = put_in(state, [:agents, agent_id, :pending_permissions, session_id], {tool_call, options, from})
    {:noreply, new_state}
  end

  defp is_terminal_event?(event) do
    Map.get(event, :type) in ["complete", "error"]
  end

  # Deduplicate tool_call_update events by stripping rawInput/rawOutput on repeats
  defp maybe_dedup_tool_call(event, session_id, state, agent_id) do
    tool_call_id = event[:update]["toolCallId"]

    if tool_call_id != nil do
      seen = get_in(state, [:agents, agent_id, :tool_call_seen, session_id]) || MapSet.new()

      if MapSet.member?(seen, tool_call_id) do
        # Already seen — strip rawInput and rawOutput
        stripped_update =
          event.update
          |> maybe_delete_in(["rawInput"])
          |> maybe_delete_in(["rawOutput"])

        event = put_in(event, [:update], stripped_update)
        {event, state}
      else
        # First time seeing this tool_call_id — add to seen set
        new_seen = MapSet.put(seen, tool_call_id)
        state = put_in(state, [:agents, agent_id, :tool_call_seen, session_id], new_seen)
        {event, state}
      end
    else
      {event, state}
    end
  end

  defp maybe_delete_in(map, [key]) when is_map(map), do: Map.delete(map, key)
  defp maybe_delete_in(map, _keys), do: map

  # Merge consecutive chunk events of the same type
  defp chunk_text(event) do
    case event do
      %{update: %{"content" => %{"text" => text}}} when is_binary(text) -> text
      _ -> nil
    end
  end

  defp chunk_type(event) do
    case event do
      %{update: %{"sessionUpdate" => type}} -> type
      _ -> nil
    end
  end

  defp merge_text(event, extra_text) do
    update_in(event, [:update], fn update ->
      Map.update!(update, "content", fn content ->
        Map.update!(content, "text", &(&1 <> extra_text))
      end)
    end)
  end

  defp merge_chunks(events) when is_list(events) do
    events
    |> Enum.reduce([], fn event, acc ->
      text = chunk_text(event)
      session_update = chunk_type(event)

      if is_binary(text) and session_update in ["agent_thought_chunk", "agent_message_chunk"] do
        case acc do
          [last | rest] ->
            last_text = chunk_text(last)
            last_type = chunk_type(last)

            if last_type == session_update and is_binary(last_text) do
              [merge_text(last, text) | rest]
            else
              [event | acc]
            end

          _ ->
            [event | acc]
        end
      else
        [event | acc]
      end
    end)
    |> Enum.reverse()
  end

  defp merge_chunks(events), do: events


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
            GenServer.reply(from, {:ok, merge_chunks([])})
            new_state = put_in(state, [:agents, agent_id, :waiters, session_id], rest_waiters)
            {:noreply, new_state}
        end
    end
  end

  def handle_info({:terminal_timeout, session_id, agent_id}, state) do
    case get_in(state, [:agents, agent_id, :terminal_waiters, session_id]) do
      nil ->
        {:noreply, state}

      {from, _timer_ref} ->
        # Drain whatever events are in the queue (may be partial)
        events = get_in(state, [:agents, agent_id, :event_queues, session_id]) || []
        GenServer.reply(from, {:ok, merge_chunks(events)})

        new_state =
          state
          |> put_in([:agents, agent_id, :event_queues, session_id], [])
          |> put_in([:agents, agent_id, :terminal_waiters, session_id], nil)

        {:noreply, new_state}
    end
  end

  def handle_info({:collect_timeout, session_id, agent_id}, state) do
    case get_in(state, [:agents, agent_id, :collect_waiters, session_id]) do
      nil ->
        {:noreply, state}

      {from, _collect_ref, idle_ref, _idle_ms, acc} ->
        Process.cancel_timer(idle_ref)
        GenServer.reply(from, {:ok, merge_chunks(acc)})
        new_state = put_in(state, [:agents, agent_id, :collect_waiters, session_id], nil)
        {:noreply, new_state}
    end
  end

  def handle_info({:idle_timeout, session_id, agent_id}, state) do
    case get_in(state, [:agents, agent_id, :collect_waiters, session_id]) do
      nil ->
        {:noreply, state}

      {from, collect_ref, _idle_ref, _idle_ms, acc} ->
        Process.cancel_timer(collect_ref)
        GenServer.reply(from, {:ok, merge_chunks(acc)})
        new_state = put_in(state, [:agents, agent_id, :collect_waiters, session_id], nil)
        {:noreply, new_state}
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
              GenServer.reply(from, {:ok, merge_chunks([])})
            end)
          end)

          # Cancel any terminal waiters (skip nil entries — already consumed)
          Enum.each(entry.terminal_waiters, fn
            {_session_id, nil} ->
              :ok
            {_session_id, {from, timer_ref}} ->
              Process.cancel_timer(timer_ref)
              GenServer.reply(from, {:ok, merge_chunks([])})
          end)

          # Cancel any collect waiters
          Enum.each(entry.collect_waiters, fn
            {_session_id, nil} -> :ok
            {_session_id, {from, collect_ref, idle_ref, _idle_ms, acc}} ->
              Process.cancel_timer(collect_ref)
              Process.cancel_timer(idle_ref)
              GenServer.reply(from, {:ok, merge_chunks(acc)})
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
