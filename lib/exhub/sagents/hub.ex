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

  alias Exhub.Sagents.{DisplayMessages, Factory, Persistence, Store}

  @chat_timeout 300_000
  @stream_timeout 120_000
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
      {:ok, build_event_stream(conversation.id, message)}
    end
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

  # The stream runs in the process that consumes it (the HTTP request), so each
  # event is written to the socket as it arrives instead of at the end of the
  # turn. The request subscribes itself before the message is queued, so no
  # event from this turn can be missed.
  defp build_event_stream(id, message) do
    Stream.resource(
      fn ->
        Sagents.AgentServer.subscribe(id)

        case Sagents.AgentServer.add_message(id, LangChain.Message.new_user!(message)) do
          :ok ->
            %{
              id: id,
              deadline: System.monotonic_time(:millisecond) + @stream_timeout,
              done: false,
              saw_running: false
            }

          {:error, reason} ->
            %{id: id, done: true, pending: [%{type: :error, error: inspect(reason)}]}
        end
      end,
      &next_event/1,
      fn state -> Sagents.AgentServer.unsubscribe(state.id) end
    )
  end

  defp next_event(%{pending: [event | rest]} = state) do
    {[event], %{state | pending: rest, done: rest == []}}
  end

  defp next_event(%{done: true} = state), do: {:halt, state}

  defp next_event(state) do
    remaining = max(state.deadline - System.monotonic_time(:millisecond), 0)

    if remaining == 0 do
      {[%{type: :error, error: "timeout"}], %{state | done: true}}
    else
      receive do
        {:agent, {:status_changed, :running, _}} ->
          next_event(%{state | saw_running: true})

        {:agent, {:llm_deltas, deltas}} ->
          {Enum.map(deltas, fn delta -> %{type: :delta, text: delta_text(delta)} end), state}

        {:agent, {:tool_execution_update, :executing, info}} ->
          event = %{
            type: :tool_call,
            tool: Map.get(info, :name),
            call_id: Map.get(info, :call_id)
          }

          {[event], state}

        {:agent, {:tool_execution_update, status, info}} when status in [:completed, :failed] ->
          event = %{
            type: :tool_result,
            tool: Map.get(info, :name),
            call_id: Map.get(info, :call_id),
            status: status
          }

          {[event], state}

        # The initial idle a fresh agent broadcasts is not a finished turn.
        {:agent, {:status_changed, :idle, _}} when not state.saw_running ->
          next_event(state)

        {:agent, {:status_changed, :idle, _}} ->
          {:ok, final_text} = extract_last_response(state.id)
          {[%{type: :complete, text: final_text}], %{state | done: true}}

        {:agent, {:status_changed, :error, reason}} ->
          {[%{type: :error, error: inspect(reason)}], %{state | done: true}}

        {:agent, {:status_changed, :interrupted, data}} ->
          {[%{type: :interrupted, interrupt: interrupt_summary(data)}], %{state | done: true}}

        {:agent, _other} ->
          next_event(%{state | deadline: state.deadline})
      after
        remaining ->
          {[%{type: :error, error: "timeout"}], %{state | done: true}}
      end
    end
  end

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
