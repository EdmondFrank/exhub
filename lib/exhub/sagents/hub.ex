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
              %{name: name, status: info.status, pid: pid}
            rescue
              _ -> %{name: name, status: :unknown, pid: pid}
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

  # --- Private Functions ---

  defp do_start_agent(name, state) do
    agents = Factory.agents()

    case Map.get(agents, name) do
      nil ->
        {:reply, {:error, :not_found}, state}

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
                {:reply, {:ok, pid}, %{state | running: Map.put(state.running, name, pid)}}

              {:error, reason} ->
                Logger.error("[Sagents.Hub] Failed to start agent #{name}: #{inspect(reason)}")
                {:reply, {:error, reason}, state}
            end

          {:error, reason} ->
            {:reply, {:error, reason}, state}
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
          do_start_agent(name, new_state)
        end

      nil ->
        do_start_agent(name, state)
    end
  end

  defp do_chat(name, message) do
    user_message = LangChain.Message.new_user!(message)

    case Sagents.AgentServer.add_message(name, user_message) do
      :ok ->
        wait_for_completion(name, 120_000)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp wait_for_completion(name, timeout) do
    deadline = System.monotonic_time(:millisecond) + timeout

    receive do
      {:agent, {:status_changed, :idle, _}} ->
        extract_last_response(name)

      {:agent, {:status_changed, :error, reason}} ->
        {:error, reason}

      {:agent, {:status_changed, :interrupted, _data}} ->
        {:error, :interrupted}

      {:agent, _event} ->
        remaining = deadline - System.monotonic_time(:millisecond)

        if remaining > 0 do
          wait_for_completion(name, remaining)
        else
          {:error, :timeout}
        end
    after
      timeout -> {:error, :timeout}
    end
  end

  defp extract_last_response(name) do
    state = Sagents.AgentServer.get_state(name)

    case List.last(state.messages) do
      %LangChain.Message{role: :assistant, content: content} when is_binary(content) ->
        {:ok, content}

      %LangChain.Message{role: :assistant, content: content} when is_list(content) ->
        text = content |> Enum.filter(&is_binary/1) |> Enum.join("")
        {:ok, text}

      _ ->
        {:ok, ""}
    end
  end

  defp build_event_stream(name, message) do
    Stream.resource(
      fn ->
        task =
          Task.async(fn ->
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
          events = Enum.map(deltas, fn delta -> %{type: :delta, text: extract_delta_text(delta)} end)
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
