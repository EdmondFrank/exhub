defmodule Exhub.Sagents.Hub do
  @moduledoc """
  Conversation lifecycle for the Agent Hub.

  One conversation is one `Sagents` agent process. A conversation is a row in
  `Exhub.Sagents.Store`; its agent starts on demand (first chat, explicit
  start) and stops again on the sagents inactivity timeout, restoring its
  messages and middleware state from SQLite when it comes back.

  Chat calls run in the caller's process, not in this GenServer: the caller
  subscribes to the agent's event channel and blocks on it, so one long turn
  never blocks conversation management, and events for different conversations
  cannot be confused with each other.
  """

  use GenServer
  require Logger

  alias Exhub.Sagents.{DisplayMessages, Factory, Markdown, Persistence, Store}

  @chat_timeout 300_000
  @stream_timeout 120_000
  # A browser EventSource reconnects on its own, so a follow stream only needs
  # to be long enough to be useful; the client re-snapshots when it reopens.
  @follow_timeout 1_800_000
  @heartbeat 15_000
  @default_inactivity 3_600_000

  # --- Client API ---

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc "Registered agent profiles, each flagged with whether any conversation is live."
  def list_agents do
    running = running_map()

    Factory.agents()
    |> Enum.map(fn {name, _config} ->
      %{name: name, running: Enum.any?(Map.values(running), &(&1 == name))}
    end)
    |> Enum.sort_by(& &1.name)
  end

  @doc "Conversations, most recently active first, each with a `:running` flag."
  @spec list_conversations(keyword()) :: {:ok, [map()]} | {:error, term()}
  def list_conversations(opts \\ []) do
    running = running_map()

    case Store.list_conversations(opts) do
      {:ok, conversations} ->
        {:ok, Enum.map(conversations, &Map.put(&1, :running, Map.has_key?(running, &1.id)))}

      other ->
        other
    end
  end

  @spec create_conversation(String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def create_conversation(agent_name, opts \\ []) do
    if Map.has_key?(Factory.agents(), agent_name) do
      Store.create_conversation(agent_name, opts)
    else
      {:error, :unknown_agent}
    end
  end

  @spec get_conversation(String.t()) :: {:ok, map()} | {:error, term()}
  def get_conversation(id) do
    case Store.get_conversation(id) do
      {:ok, conversation} ->
        {:ok, Map.put(conversation, :running, Map.has_key?(running_map(), id))}

      other ->
        other
    end
  end

  @spec rename_conversation(String.t(), String.t()) :: {:ok, map()} | {:error, term()}
  def rename_conversation(id, title), do: Store.update_conversation(id, %{title: title})

  @spec delete_conversation(String.t()) :: :ok | {:error, term()}
  def delete_conversation(id) do
    _ = stop_conversation(id)
    Store.delete_conversation(id)
  end

  @doc "Drop the transcript and agent state so the conversation starts empty."
  @spec reset_conversation(String.t()) :: :ok | {:error, term()}
  def reset_conversation(id) do
    _ = stop_conversation(id)

    with :ok <- Store.clear_messages(id),
         :ok <- Store.delete_state(id) do
      :ok
    end
  end

  @spec list_messages(String.t()) :: {:ok, [map()]} | {:error, term()}
  def list_messages(id), do: Store.list_messages(id)

  @doc """
  Status for a conversation, or an aggregate for an agent profile name.

  A conversation id reports that thread's live sagents status. An agent
  profile name (the MCP tool and legacy URLs pass one) reports how many of
  that agent's conversations are live.
  """
  @spec get_status(String.t()) :: map()
  def get_status(id) do
    case Store.get_conversation(id) do
      {:ok, _conversation} -> conversation_status(id)
      {:error, _reason} -> agent_status(id)
    end
  end

  defp conversation_status(id) do
    running = Map.has_key?(running_map(), id)

    status =
      if running do
        try do
          Sagents.AgentServer.get_status(id)
        rescue
          _ -> :unknown
        catch
          :exit, _ -> :unknown
        end
      else
        :not_running
      end

    %{id: id, running: running, status: status, has_interrupt: status == :interrupted}
  end

  defp agent_status(name) do
    if Map.has_key?(Factory.agents(), name) do
      conversations =
        case Store.list_conversations(agent_name: name, limit: 100) do
          {:ok, list} -> list
          _ -> []
        end

      live_ids = running_map()
      live = Enum.count(conversations, &Map.has_key?(live_ids, &1.id))

      %{
        id: name,
        agent: name,
        running: live > 0,
        status: if(live > 0, do: :running, else: :not_running),
        conversations: length(conversations),
        running_conversations: live
      }
    else
      %{id: name, running: false, status: :not_running, has_interrupt: false}
    end
  end

  @doc "Start the conversation's agent without sending a message."
  @spec start_conversation(String.t()) :: {:ok, pid()} | {:error, term()}
  def start_conversation(id) do
    with {:ok, conversation} <- Store.get_conversation(id) do
      ensure_running(conversation)
    end
  end

  @doc """
  Newest conversation for an agent profile, creating one when it has none.

  Used by the legacy `/agent-hub/agents/:name/chat` URL so old links keep
  landing on that agent's current thread.
  """
  @spec latest_conversation(String.t()) :: {:ok, map()} | {:error, term()}
  def latest_conversation(agent_name), do: latest_or_create_conversation(agent_name)

  @spec stop_conversation(String.t()) :: :ok
  def stop_conversation(id) do
    GenServer.call(__MODULE__, {:stop, id})
  end

  @doc """
  Send a message and wait for the turn to finish.

  Accepts a conversation id or an agent profile name. A profile name resolves
  to that agent's most recent conversation, creating one when it has none —
  this is what the MCP `agent_hub_chat` tool relies on.
  """
  @spec chat(String.t(), String.t()) :: {:ok, String.t()} | {:error, term()}
  def chat(name_or_id, message) do
    with {:ok, conversation} <- resolve_conversation(name_or_id),
         {:ok, _pid} <- ensure_running(conversation) do
      do_chat(conversation.id, message)
    end
  end

  @doc "Send a message and return an enumerable of stream events."
  @spec chat_stream(String.t(), String.t()) :: {:ok, Enumerable.t()} | {:error, term()}
  def chat_stream(name_or_id, message) do
    with {:ok, conversation} <- resolve_conversation(name_or_id),
         {:ok, _pid} <- ensure_running(conversation) do
      {:ok, build_stream(conversation.id, :turn, message)}
    end
  end

  @doc """
  Queue a user message and return immediately.

  Unlike `chat_stream/2`, this does not hold the request open for the turn: the
  caller follows progress on `event_stream/1`. Returns `{:error, reason}` when
  the agent refuses the message (sagents before 0.10 rejects a send while a run
  is in flight).
  """
  @spec send_message(String.t(), String.t()) :: :ok | {:error, term()}
  def send_message(name_or_id, message) when is_binary(message) do
    with {:ok, conversation} <- resolve_conversation(name_or_id),
         {:ok, _pid} <- ensure_running(conversation) do
      Sagents.AgentServer.add_message(conversation.id, LangChain.Message.new_user!(message))
    end
  rescue
    error -> {:error, error}
  catch
    :exit, reason -> {:error, reason}
  end

  @doc """
  Long-lived event stream for one conversation.

  Emits `%{event: name, data: payload}` maps and `:heartbeat` atoms (written as
  SSE comments by the router). The stream starts with a `snapshot` carrying the
  persisted transcript, the live status, and any pending interrupt, then carries
  display-message rows, tool status changes, LLM deltas, and status changes.
  Starts the conversation's agent when it is not running.
  """
  @spec event_stream(String.t()) :: {:ok, Enumerable.t()} | {:error, term()}
  def event_stream(conversation_id) do
    with {:ok, conversation} <- Store.get_conversation(conversation_id),
         {:ok, _pid} <- ensure_running(conversation) do
      {:ok, build_stream(conversation.id, :follow)}
    end
  rescue
    error -> {:error, error}
  catch
    :exit, reason -> {:error, reason}
  end

  @doc """
  Pending approval payload for a conversation, if one is waiting.

  Starts the agent when its interrupted state was restored from SQLite, so a
  browser reloaded after a timeout can still answer the prompt.
  """
  @spec pending_interrupt(String.t()) :: {:ok, map()} | :none
  def pending_interrupt(id) do
    with {:ok, conversation} <- Store.get_conversation(id),
         true <- Map.has_key?(running_map(), id) or conversation.status == "interrupted",
         {:ok, _pid} <- ensure_running(conversation),
         %{interrupt_data: data} when not is_nil(data) <- Sagents.AgentServer.get_state(id) do
      {:ok, interrupt_summary(data)}
    else
      _other -> :none
    end
  rescue
    _error -> :none
  catch
    :exit, _reason -> :none
  end

  @doc "Answer a pending human-in-the-loop interrupt."
  @spec resume(String.t(), term()) :: :ok | {:error, term()}
  def resume(id, decisions) do
    Sagents.AgentServer.resume(id, decisions)
  rescue
    error -> {:error, error}
  end

  # --- Legacy agent-level helpers (MCP tools, old UI URLs) ---

  def start_agent(name) do
    with {:ok, conversation} <- resolve_conversation(name) do
      ensure_running(conversation)
    end
  end

  def stop_agent(name) do
    for {id, agent_name} <- running_map(), agent_name == name do
      stop_conversation(id)
    end

    :ok
  end

  def agent_running?(name) do
    Enum.any?(Map.values(running_map()), &(&1 == name))
  end

  def reset(name) do
    with {:ok, conversation} <- resolve_conversation(name) do
      reset_conversation(conversation.id)
    end
  end

  # --- Server callbacks ---

  @impl true
  def init(_opts) do
    {:ok, %{running: %{}}}
  end

  @impl true
  def handle_call(:running_map, _from, state) do
    {running, state} = prune(state)
    {:reply, Map.new(running, fn {id, entry} -> {id, entry.agent_name} end), state}
  end

  def handle_call({:ensure_running, conversation}, _from, state) do
    case live_pid(state, conversation.id) do
      {:ok, pid} ->
        {:reply, {:ok, pid}, state}

      :error ->
        case start_agent_process(conversation) do
          {:ok, pid} ->
            entry = %{pid: pid, agent_name: conversation.agent_name}

            {:reply, {:ok, pid},
             %{state | running: Map.put(state.running, conversation.id, entry)}}

          {:error, reason} ->
            {:reply, {:error, reason}, state}
        end
    end
  end

  def handle_call({:stop, id}, _from, state) do
    case Map.get(state.running, id) do
      nil ->
        {:reply, :ok, state}

      %{pid: _pid} ->
        _ = stop_agent_process(id)
        {:reply, :ok, %{state | running: Map.delete(state.running, id)}}
    end
  end

  # --- Process management ---

  defp ensure_running(conversation) do
    GenServer.call(__MODULE__, {:ensure_running, conversation}, 60_000)
  end

  defp start_agent_process(conversation, retry? \\ true) do
    case Map.get(Factory.agents(), conversation.agent_name) do
      nil ->
        {:error, {:unknown_agent, conversation.agent_name}}

      config ->
        with {:ok, agent, session_opts} <- Factory.create_agent(conversation.id, config) do
          opts =
            [
              agent_id: conversation.id,
              agent: agent,
              initial_state: Sagents.State.new!(%{}),
              conversation_id: conversation.id,
              agent_persistence: Persistence,
              display_message_persistence: DisplayMessages,
              inactivity_timeout:
                Keyword.get(session_opts, :inactivity_timeout, @default_inactivity)
            ] ++ Keyword.take(session_opts, [:pubsub, :presence_tracking, :shutdown_delay])

          case Sagents.AgentsDynamicSupervisor.start_agent_sync(opts) do
            {:ok, _supervisor_pid} ->
              case Sagents.AgentServer.get_pid(conversation.id) do
                pid when is_pid(pid) ->
                  Logger.info("[Sagents.Hub] started conversation #{conversation.id}")
                  {:ok, pid}

                nil when retry? ->
                  # A stopped AgentServer exits normally, and a `:transient`
                  # child is not restarted — leaving a registered supervisor
                  # with no server behind it. Tear it down and try once more.
                  Logger.warning(
                    "[Sagents.Hub] stale supervisor for #{conversation.id}, restarting"
                  )

                  _ = Sagents.AgentsDynamicSupervisor.stop_agent(conversation.id)
                  start_agent_process(conversation, false)

                nil ->
                  {:error, :agent_not_registered}
              end

            {:error, reason} ->
              Logger.error("[Sagents.Hub] failed to start #{conversation.id}: #{inspect(reason)}")

              {:error, reason}
          end
        end
    end
  end

  # Stop the whole agent supervisor, not just the AgentServer: stopping the
  # server alone leaves an empty supervisor registered, and a later start then
  # finds it "already running" and never boots a server.
  defp stop_agent_process(id) do
    case Sagents.AgentsDynamicSupervisor.stop_agent(id) do
      :ok -> :ok
      {:error, :not_found} -> :ok
    end
  rescue
    _error -> :ok
  catch
    :exit, _reason -> :ok
  end

  defp running_map do
    GenServer.call(__MODULE__, :running_map)
  end

  defp live_pid(state, id) do
    case Map.get(state.running, id) do
      %{pid: pid} -> if Process.alive?(pid), do: {:ok, pid}, else: :error
      _ -> :error
    end
  end

  defp prune(state) do
    running =
      state.running
      |> Enum.filter(fn {_id, %{pid: pid}} -> Process.alive?(pid) end)
      |> Map.new()

    {running, %{state | running: running}}
  end

  defp resolve_conversation(name_or_id) do
    case Store.get_conversation(name_or_id) do
      {:ok, conversation} ->
        {:ok, conversation}

      {:error, :not_found} ->
        latest_or_create_conversation(name_or_id)

      other ->
        other
    end
  end

  defp latest_or_create_conversation(agent_name) do
    if Map.has_key?(Factory.agents(), agent_name) do
      case Store.list_conversations(agent_name: agent_name, limit: 1) do
        {:ok, [conversation | _]} -> {:ok, conversation}
        _ -> Store.create_conversation(agent_name, title: default_title(agent_name))
      end
    else
      {:error, :not_found}
    end
  end

  defp default_title(agent_name) do
    "#{String.capitalize(agent_name)} chat"
  end

  # --- Turn execution (caller process) ---

  defp do_chat(id, message) do
    task =
      Task.async(fn ->
        Sagents.AgentServer.subscribe(id)

        case Sagents.AgentServer.add_message(id, LangChain.Message.new_user!(message)) do
          :ok -> wait_for_completion(id, @chat_timeout)
          {:error, reason} -> {:error, reason}
        end
      end)

    case Task.yield(task, @chat_timeout + 5_000) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} -> result
      {:exit, reason} -> {:error, reason}
      nil -> {:error, :timeout}
    end
  end

  # A freshly started agent broadcasts `{:status_changed, :idle, nil}` as soon
  # as it is subscribed. That initial idle is not the end of a turn, so ignore
  # idles until a `:running` status has been seen for this turn.
  defp wait_for_completion(id, timeout), do: wait_for_completion(id, timeout, false)

  defp wait_for_completion(id, timeout, saw_running?) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_wait_for_completion(id, deadline, timeout, saw_running?)
  end

  defp do_wait_for_completion(id, deadline, timeout, saw_running?) do
    receive do
      {:agent, {:status_changed, :running, _}} ->
        do_wait_for_completion(id, deadline, timeout, true)

      {:agent, {:status_changed, :idle, _}} when not saw_running? ->
        do_wait_for_completion(id, deadline, timeout, false)

      {:agent, {:status_changed, :idle, _}} ->
        Logger.info("[Sagents.Hub] conversation #{id} completed")
        extract_last_response(id)

      {:agent, {:status_changed, :error, reason}} ->
        Logger.error("[Sagents.Hub] conversation #{id} error: #{inspect(reason)}")
        {:error, reason}

      {:agent, {:status_changed, :interrupted, data}} ->
        {:error, {:interrupted, interrupt_summary(data)}}

      {:agent, {:status_changed, :cancelled, _}} ->
        {:error, :cancelled}

      {:agent, {:status_changed, :paused, reason}} ->
        {:error, {:paused, reason}}

      {:agent, {:tool_execution_update, status, info}} ->
        Logger.debug("[Sagents.Hub] tool #{Map.get(info, :name)} -> #{status}")
        do_wait_for_completion(id, deadline, timeout, saw_running?)

      {:agent, _other} ->
        do_wait_for_completion(id, deadline, timeout, saw_running?)
    after
      remaining(deadline) ->
        Logger.warning("[Sagents.Hub] conversation #{id} timeout after #{timeout}ms")
        {:error, :timeout}
    end
  end

  defp remaining(deadline) do
    max(deadline - System.monotonic_time(:millisecond), 0)
  end

  defp extract_last_response(id) do
    state = Sagents.AgentServer.get_state(id)

    text =
      state.messages
      |> Enum.reverse()
      |> Enum.filter(&(&1.role == :assistant))
      |> Enum.map(fn %LangChain.Message{content: content} -> message_text(content) end)
      |> Enum.find("", fn text -> text != "" end)

    {:ok, text}
  end

  defp message_text(content) when is_binary(content), do: content

  defp message_text(content) when is_list(content) do
    Enum.map_join(content, "", fn
      text when is_binary(text) -> text
      %LangChain.Message.ContentPart{type: :text, content: text} -> text || ""
      %LangChain.Message.ContentPart{content: text} when is_binary(text) -> text
      _other -> ""
    end)
  end

  defp message_text(_content), do: ""

  # --- Streaming ---

  # Both SSE endpoints share one state machine.
  #
  #   * `:turn`   — backs `chat_stream/2` (POST /chat). It subscribes, sends the
  #     message, and ends when the run ends (idle, interrupt, cancel, error),
  #     keeping the legacy `%{type: ...}` event shape.
  #   * `:follow` — backs `event_stream/1` (GET /events). It keeps running:
  #     transcript rows, tool status, deltas, and status changes are forwarded,
  #     a heartbeat makes a dead connection or stopped agent visible, and the
  #     client reconnects (getting a fresh snapshot) when the stream ends.
  #
  # The stream runs in the process that consumes it (the HTTP request), so each
  # event is written to the socket as it arrives instead of at the end of the
  # turn.
  defp build_stream(id, mode, message \\ nil) do
    Stream.resource(
      fn ->
        Sagents.AgentServer.subscribe(id)

        state = %{
          id: id,
          mode: mode,
          deadline: System.monotonic_time(:millisecond) + stream_timeout(mode),
          saw_running: false,
          pending: [],
          done: false
        }

        case mode do
          :turn ->
            case Sagents.AgentServer.add_message(id, LangChain.Message.new_user!(message)) do
              :ok -> state
              {:error, reason} -> %{state | done: true, pending: [{:error, inspect(reason)}]}
            end

          :follow ->
            # Subscribe before reading the snapshot so nothing between the two
            # is lost. Rows that land in both are de-duplicated by id in the UI.
            %{state | pending: [{:snapshot, snapshot(id)}]}
        end
      end,
      &next_event/1,
      fn state -> Sagents.AgentServer.unsubscribe(state.id) end
    )
  end

  defp stream_timeout(:turn), do: @stream_timeout
  defp stream_timeout(:follow), do: @follow_timeout

  defp next_event(state) do
    case step(state) do
      {:halt, state} ->
        {:halt, state}

      {events, state} ->
        events =
          Enum.flat_map(events, fn
            # Heartbeats are written as SSE comments by the router, not JSON.
            :heartbeat -> [:heartbeat]
            event -> List.wrap(present_event(state.mode, event))
          end)

        {events, state}
    end
  end

  defp step(%{pending: [event | rest]} = state) do
    {[event], %{state | pending: rest}}
  end

  defp step(%{pending: [], done: true} = state), do: {:halt, state}

  defp step(state) do
    remaining = max(state.deadline - System.monotonic_time(:millisecond), 0)

    if remaining == 0 do
      deadline_reached(state)
    else
      wait = if state.mode == :follow, do: min(remaining, @heartbeat), else: remaining

      receive do
        {:agent, event} -> advance(state, event)
      after
        wait -> on_wait(state)
      end
    end
  end

  defp deadline_reached(%{mode: :turn} = state),
    do: {[{:error, "timeout"}], %{state | done: true}}

  defp deadline_reached(state), do: {:halt, state}

  defp on_wait(%{mode: :turn} = state),
    do: {[{:error, "timeout"}], %{state | done: true}}

  defp on_wait(%{mode: :follow} = state) do
    # Heartbeat tick: keep the connection open, and notice an agent that went
    # away (inactivity timeout, stop, crash). Closing lets EventSource
    # reconnect, which starts the agent again and sends a fresh snapshot.
    if agent_alive?(state.id) do
      {[:heartbeat], state}
    else
      {[{:status, :not_running, nil}], %{state | done: true}}
    end
  end

  defp advance(state, {:status_changed, :running, _}) do
    state = %{state | saw_running: true}

    case state.mode do
      :turn -> step(state)
      :follow -> {[{:status, :running, nil}], state}
    end
  end

  defp advance(state, {:llm_deltas, deltas}) do
    {Enum.map(deltas, &{:delta, delta_text(&1)}), state}
  end

  defp advance(state, {:tool_execution_update, :executing, info}) do
    {[{:tool, :executing, tool_info(info)}], state}
  end

  defp advance(state, {:tool_execution_update, status, info})
       when status in [:completed, :failed, :interrupted, :cancelled] do
    {[{:tool, status, tool_info(info)}], state}
  end

  defp advance(state, {:display_message_saved, row}) do
    {[{:display_message, row}], state}
  end

  # sagents 0.10+ queues a message typed while a run is in flight.
  defp advance(state, {:message_queued, _message}) do
    {[:queued], state}
  end

  # The initial idle a fresh agent broadcasts is not a finished turn.
  defp advance(state, {:status_changed, :idle, _}) do
    cond do
      not state.saw_running ->
        step(state)

      state.mode == :turn ->
        {[{:complete, last_response(state.id)}], %{state | done: true}}

      true ->
        {[{:status, :idle, nil}], state}
    end
  end

  defp advance(state, {:status_changed, :interrupted, data}) do
    case state.mode do
      :turn -> {[{:interrupted, interrupt_summary(data)}], %{state | done: true}}
      :follow -> {[{:status, :interrupted, interrupt_summary(data)}], state}
    end
  end

  # A cancel can come from another tab or `stop_conversation/1`; without this
  # clause the stream waited for its deadline instead of ending.
  defp advance(state, {:status_changed, :cancelled, _reason}) do
    case state.mode do
      :turn -> {[{:cancelled, nil}], %{state | done: true}}
      :follow -> {[{:status, :cancelled, nil}], state}
    end
  end

  defp advance(state, {:status_changed, :paused, reason}) do
    case state.mode do
      :turn -> {[{:paused, reason}], %{state | done: true}}
      :follow -> {[{:status, :paused, reason}], state}
    end
  end

  defp advance(state, {:status_changed, :error, reason}) do
    case state.mode do
      :turn -> {[{:error, inspect(reason)}], %{state | done: true}}
      :follow -> {[{:status, :error, reason}], state}
    end
  end

  defp advance(state, _other), do: step(state)

  defp last_response(id) do
    case extract_last_response(id) do
      {:ok, text} -> text
      _other -> ""
    end
  end

  defp tool_info(info) do
    %{
      call_id: Map.get(info, :call_id),
      name: Map.get(info, :name),
      display_text: Map.get(info, :display_text)
    }
  end

  # First event of a follow stream: everything a freshly loaded page needs.
  defp snapshot(id) do
    messages =
      case Store.list_messages(id) do
        {:ok, messages} -> Enum.map(messages, &present_row/1)
        {:error, _reason} -> []
      end

    status = conversation_status(id)

    interrupt =
      case pending_interrupt(id) do
        {:ok, interrupt} -> interrupt
        :none -> nil
      end

    %{
      messages: messages,
      status: status.status,
      # `alive` tracks the agent process; `running` inside a status event tracks
      # an executing turn. The two differ between turns, so send both.
      alive: status.running,
      running: status.status == :running,
      interrupt: interrupt
    }
  end

  defp agent_alive?(id) do
    case Sagents.AgentServer.get_pid(id) do
      pid when is_pid(pid) -> Process.alive?(pid)
      _other -> false
    end
  rescue
    _error -> false
  catch
    :exit, _reason -> false
  end

  @doc false
  # Maps an internal event to the wire shape for a stream mode. `nil` drops it;
  # turn streams carry only turn events, follow streams only transcript/status
  # events. Exposed for tests.
  def present_event(:turn, {:delta, text}), do: %{type: :delta, text: text}

  def present_event(:turn, {:tool, :executing, info}),
    do: %{type: :tool_call, tool: info.name, call_id: info.call_id}

  def present_event(:turn, {:tool, status, info}),
    do: %{type: :tool_result, tool: info.name, call_id: info.call_id, status: status}

  def present_event(:turn, {:complete, text}), do: %{type: :complete, text: text}
  def present_event(:turn, {:interrupted, data}), do: %{type: :interrupted, interrupt: data}
  def present_event(:turn, {:cancelled, _reason}), do: %{type: :cancelled}
  def present_event(:turn, {:paused, reason}), do: %{type: :paused, reason: inspect(reason)}
  def present_event(:turn, {:error, reason}), do: %{type: :error, error: reason}
  def present_event(:turn, _event), do: nil

  def present_event(:follow, {:snapshot, data}), do: %{event: "snapshot", data: data}

  def present_event(:follow, {:display_message, row}),
    do: %{event: "display_message", data: present_row(row)}

  def present_event(:follow, {:tool, status, info}),
    do: %{event: "tool_status", data: Map.put(info, :status, status)}

  def present_event(:follow, {:delta, text}), do: %{event: "delta", data: %{text: text}}

  def present_event(:follow, {:status, status, payload}),
    do: %{
      event: "status",
      data: %{
        status: status,
        running: status == :running,
        alive: status != :not_running,
        payload: payload
      }
    }

  def present_event(:follow, :queued), do: %{event: "queued", data: %{}}
  def present_event(:follow, _event), do: nil

  # Rows travel as raw content while the transcript renders Markdown, so the HTML
  # is pre-rendered here: snapshots and live rows then look identical to the
  # browser, and the client needs no Markdown parser of its own.
  defp present_row(%{content_type: "text"} = row) do
    content = Map.get(row, :content)
    text = if is_map(content), do: content["text"], else: nil
    Map.put(row, :html, Markdown.to_html(text))
  end

  defp present_row(row), do: row

  defp delta_text(delta) do
    case Map.get(delta, :content) do
      nil -> ""
      text when is_binary(text) -> text
      list when is_list(list) -> Enum.map_join(list, "", &to_string/1)
      other -> to_string(other)
    end
  end

  # Interrupt payloads carry structs and atom-keyed maps; flatten them to
  # JSON-friendly values so the SSE encoder never fails on an approval prompt.
  defp interrupt_summary(data) when is_map(data) do
    requests =
      data
      |> fetch_any(:action_requests, [])
      |> Enum.map(fn request ->
        %{
          tool_call_id: fetch_any(request, :tool_call_id),
          tool_name: fetch_any(request, :tool_name),
          arguments: fetch_any(request, :arguments, %{})
        }
      end)

    %{
      action_requests: requests,
      review_configs: fetch_any(data, :review_configs, %{})
    }
  end

  defp interrupt_summary(_data), do: %{action_requests: [], review_configs: %{}}

  defp fetch_any(map, key, default \\ nil)

  defp fetch_any(map, key, default) when is_map(map) do
    case Map.fetch(map, key) do
      {:ok, value} -> value
      :error -> Map.get(map, to_string(key), default)
    end
  end

  defp fetch_any(_map, _key, default), do: default
end
