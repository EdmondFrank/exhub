defmodule Exhub.Sagents.Hub do
  @moduledoc """
  GenServer managing the lifecycle of sagents agents.

  Lazy-starts agents on first chat message. Provides chat (blocking)
  and chat_stream (returns event stream) interfaces.
  """

  use GenServer
  require Logger

  alias Exhub.Sagents.Factory

  # --- Client API ---

  def start_link(opts \\ []) do
    name = Keyword.get(opts, :name, __MODULE__)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @doc "List all registered agent profiles with running status."
  def list_agents do
    GenServer.call(__MODULE__, :list_agents)
  end

  @doc "Start an agent by name. Returns {:ok, pid} or {:error, reason}."
  def start_agent(name) do
    GenServer.call(__MODULE__, {:start_agent, name}, 30_000)
  end

  @doc "Stop a running agent."
  def stop_agent(name) do
    GenServer.call(__MODULE__, {:stop_agent, name})
  end

  @doc "Check if an agent is currently running."
  def agent_running?(name) do
    GenServer.call(__MODULE__, {:agent_running?, name})
  end

  @doc "Get agent status info."
  def get_status(name) do
    GenServer.call(__MODULE__, {:get_status, name})
  end

  @doc """
  Send a chat message to an agent and get the response.
  Lazy-starts the agent if not running. Blocks until the agent completes.
  """
  def chat(name, message) do
    GenServer.call(__MODULE__, {:chat, name, message}, :infinity)
  end

  @doc """
  Send a chat message and return an enumerable stream of events.
  """
  def chat_stream(name, message) do
    GenServer.call(__MODULE__, {:chat_stream, name, message}, 30_000)
  end

  @doc "Reset an agent's conversation state."
  def reset(name) do
    GenServer.call(__MODULE__, {:reset, name})
  end

  # --- Server Callbacks ---

  @impl true
  def init(_opts) do
    {:ok, %{running: %{}}}
  end

  @impl true
  def handle_call(:list_agents, _from, state) do
    agents =
      Factory.agents()
      |> Enum.map(fn {name, _config} ->
        %{name: name, running: Map.has_key?(state.running, name)}
      end)

    {:reply, agents, state}
  end

  @impl true
  def handle_call({:start_agent, name}, _from, state) do
    case Map.get(state.running, name) do
      nil ->
        do_start_agent(name, state)

      pid when is_pid(pid) ->
        if Process.alive?(pid) do
          {:reply, {:ok, pid}, state}
        else
          do_start_agent(name, %{state | running: Map.delete(state.running, name)})
        end
    end
  end

  @impl true
  def handle_call({:stop_agent, name}, _from, state) do
    case Map.get(state.running, name) do
      nil ->
        {:reply, :ok, state}

      pid when is_pid(pid) ->
        if Process.alive?(pid) do
          Sagents.AgentServer.stop(name)
        end

        {:reply, :ok, %{state | running: Map.delete(state.running, name)}}
    end
  end

  @impl true
  def handle_call({:agent_running?, name}, _from, state) do
    running =
      case Map.get(state.running, name) do
        nil -> false
        pid -> Process.alive?(pid)
      end

    {:reply, running, state}
  end

  @impl true
  def handle_call({:get_status, name}, _from, state) do
    status =
      case Map.get(state.running, name) do
        nil ->
          %{name: name, status: :not_running}

        pid when is_pid(pid) ->
          if Process.alive?(pid) do
            try do
              info = Sagents.AgentServer.get_info(name)
              %{name: name, status: info.status, pid: inspect(pid)}
            rescue
              _ -> %{name: name, status: :unknown, pid: inspect(pid)}
            end
          else
            %{name: name, status: :not_running}
          end
      end

    {:reply, status, state}
  end

  @impl true
  def handle_call({:chat, name, message}, _from, state) do
    with {:ok, _pid, state} <- ensure_running(name, state) do
      result = do_chat(name, message)
      {:reply, result, state}
    else
      {:error, reason, state} -> {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:chat_stream, name, message}, _from, state) do
    with {:ok, _pid, state} <- ensure_running(name, state) do
      stream = build_event_stream(name, message)
      {:reply, {:ok, stream}, state}
    else
      {:error, reason, state} -> {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:reset, name}, _from, state) do
    case Map.get(state.running, name) do
      nil ->
        {:reply, :ok, state}

      pid when is_pid(pid) ->
        if Process.alive?(pid) do
          Sagents.AgentServer.reset(name)
        end

        {:reply, :ok, state}
    end
  end

  @impl true
  def handle_info({:agent, {:status_changed, :idle, _}}, state) do
    # Startup idle — ignore (handled in wait_for_completion receive block)
    {:noreply, state}
  end

  def handle_info({:agent, event}, state) do
    Logger.debug("[Sagents.Hub] handle_info: agent event: #{inspect(event)}")
    {:noreply, state}
  end

  # --- Private Functions ---

  defp do_start_agent(name, state) do
    case start_agent_internal(name, state) do
      {:ok, pid, new_state} ->
        {:reply, {:ok, pid}, new_state}

      {:error, reason, new_state} ->
        {:reply, {:error, reason}, new_state}
    end
  end

  defp start_agent_internal(name, state) do
    agents = Factory.agents()

    case Map.get(agents, name) do
      nil ->
        {:error, :not_found, state}

      config ->
        case Factory.create_agent(name, config) do
          {:ok, agent, session_opts} ->
            initial_state = Sagents.State.new!(%{})

            opts = [
              agent: agent,
              initial_state: initial_state,
              inactivity_timeout: Keyword.get(session_opts, :inactivity_timeout, 3_600_000)
            ]

            case Sagents.AgentServer.start_link(opts) do
              {:ok, pid} ->
                Sagents.AgentServer.subscribe(name)
                Logger.info("[Sagents.Hub] Started agent: #{name}")
                {:ok, pid, %{state | running: Map.put(state.running, name, pid)}}

              {:error, reason} ->
                Logger.error("[Sagents.Hub] Failed to start agent #{name}: #{inspect(reason)}")
                {:error, reason, state}
            end

          {:error, reason} ->
            {:error, reason, state}
        end
    end
  end

  defp ensure_running(name, state) do
    case Map.get(state.running, name) do
      pid when is_pid(pid) ->
        if Process.alive?(pid) do
          {:ok, pid, state}
        else
          new_state = %{state | running: Map.delete(state.running, name)}
          start_agent_internal(name, new_state)
        end

      nil ->
        start_agent_internal(name, state)
    end
  end

  defp do_chat(name, message) do
    # Reset GenClaw session directory for each new chat
    Exhub.Genclaw.Session.reset_session_dir()

    user_message = LangChain.Message.new_user!(message)
    Logger.info("[Sagents.Hub] do_chat: agent='#{name}'")

    case Sagents.AgentServer.add_message(name, user_message) do
      :ok ->
        case wait_for_completion(name, 300_000) do
          {:error, :timeout} = err ->
            # Agent is stuck in running state after timeout — cancel to recover
            Logger.warning("[Sagents.Hub] agent='#{name}' timed out, cancelling to reset state")
            try do
              Sagents.AgentServer.cancel(name)
            rescue
              _ -> :ok
            end
            err

          other ->
            other
        end

      {:error, reason} ->
        Logger.error("[Sagents.Hub] do_chat: add_message failed: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp wait_for_completion(name, timeout) do
    deadline = System.monotonic_time(:millisecond) + timeout

    receive do
      {:agent, {:status_changed, :running, _}} ->
        wait_for_completion(name, deadline - System.monotonic_time(:millisecond))

      {:agent, {:status_changed, :idle, _}} ->
        Logger.info("[Sagents.Hub] agent='#{name}' completed")
        extract_last_response(name)

      {:agent, {:status_changed, :error, reason}} ->
        Logger.error("[Sagents.Hub] agent='#{name}' error: #{inspect(reason)}")
        {:error, reason}

      {:agent, {:status_changed, :interrupted, _data}} ->
        {:error, :interrupted}

      {:agent, {:tool_execution_update, status, info}} ->
        Logger.info("[Sagents.Hub] tool #{info.name} -> #{status}")
        wait_for_completion(name, deadline - System.monotonic_time(:millisecond))

      {:agent, _other} ->
        remaining = deadline - System.monotonic_time(:millisecond)

        if remaining > 0 do
          wait_for_completion(name, remaining)
        else
          {:error, :timeout}
        end
    after
      timeout ->
        Logger.warning("[Sagents.Hub] agent='#{name}' timeout after #{timeout}ms")
        {:error, :timeout}
    end
  end

  defp extract_last_response(name) do
    state = Sagents.AgentServer.get_state(name)
    Logger.info("[Sagents.Hub] extract: agent='#{name}' messages=#{length(state.messages)}")

    case List.last(state.messages) do
      %LangChain.Message{role: :assistant, content: content} when is_binary(content) ->
        {:ok, content}

      %LangChain.Message{role: :assistant, content: content} when is_list(content) ->
        text = content |> Enum.map_join("", fn
          s when is_binary(s) -> s
          %LangChain.Message.ContentPart{type: :text, content: c} -> c || ""
          %LangChain.Message.ContentPart{content: c} when is_binary(c) -> c
          _ -> ""
        end)
        {:ok, text}

      _ ->
        # Last message is not assistant (e.g. system-reminder from completion guard).
        # Find the last assistant message.
        result =
          state.messages
          |> Enum.reverse()
          |> Enum.find(fn msg -> msg.role == :assistant end)

        case result do
          %LangChain.Message{content: content} when is_binary(content) ->
            {:ok, content}

          %LangChain.Message{content: content} when is_list(content) ->
            text = content |> Enum.map_join("", fn
              s when is_binary(s) -> s
              %LangChain.Message.ContentPart{type: :text, content: c} -> c || ""
              %LangChain.Message.ContentPart{content: c} when is_binary(c) -> c
              _ -> ""
            end)
            {:ok, text}

          _ ->
            Logger.warning("[Sagents.Hub] extract: no assistant message found at all")
            {:ok, ""}
        end
    end
  end

  defp build_event_stream(name, message) do
    Stream.resource(
      fn ->
        task =
          Task.async(fn ->
            # Subscribe this task process to agent events
            Sagents.AgentServer.subscribe(name)

            user_message = LangChain.Message.new_user!(message)
            Sagents.AgentServer.add_message(name, user_message)
            collect_events(name, 120_000)
          end)

        {task, false}
      end,
      fn
        {_task, true} ->
          {:halt, nil}

        {task, false} ->
          case Task.yield(task, :infinity) do
            {:ok, events} -> {events, {task, true}}
            {:exit, reason} -> {[%{type: :error, error: inspect(reason)}], {task, true}}
          end
      end,
      fn _task -> :ok end
    )
  end

  defp collect_events(name, timeout) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_collect_events(name, deadline, [])
  end

  defp do_collect_events(name, deadline, acc) do
    remaining = deadline - System.monotonic_time(:millisecond)

    if remaining <= 0 do
      Enum.reverse([%{type: :error, error: "timeout"} | acc])
    else
      receive do
        {:agent, {:llm_deltas, deltas}} ->
          events =
            Enum.map(deltas, fn delta -> %{type: :delta, text: extract_delta_text(delta)} end)

          do_collect_events(name, deadline, Enum.reverse(events) ++ acc)

        {:agent, {:tool_execution_update, :executing, info}} ->
          event = %{type: :tool_call, tool: info.name, call_id: info.call_id}
          do_collect_events(name, deadline, [event | acc])

        {:agent, {:tool_execution_update, :completed, info}} ->
          event = %{type: :tool_result, tool: info.name, call_id: info.call_id}
          do_collect_events(name, deadline, [event | acc])

        {:agent, {:status_changed, :idle, _}} ->
          {:ok, final_text} = extract_last_response(name)
          Enum.reverse([%{type: :complete, text: final_text} | acc])

        {:agent, {:status_changed, :error, reason}} ->
          Enum.reverse([%{type: :error, error: inspect(reason)} | acc])

        {:agent, {:status_changed, :interrupted, _data}} ->
          Enum.reverse([%{type: :error, error: "interrupted"} | acc])

        {:agent, _other} ->
          do_collect_events(name, deadline, acc)
      after
        remaining ->
          Enum.reverse([%{type: :error, error: "timeout"} | acc])
      end
    end
  end

  defp extract_delta_text(delta) do
    case Map.get(delta, :content) do
      nil -> ""
      text when is_binary(text) -> text
      list when is_list(list) -> Enum.map_join(list, "", &to_string/1)
      other -> to_string(other)
    end
  end
end
