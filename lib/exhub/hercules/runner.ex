defmodule Exhub.Hercules.Runner do
  @moduledoc """
  Manages Hercules test run lifecycle using Sagents AgentServer.

  Phase 1: treats entire input as a single scenario (no Gherkin parsing),
  returns raw result maps (no JUnit XML).

  ## Usage

      # Synchronous run (blocks until complete)
      {:ok, result} = Exhub.Hercules.Runner.run("Navigate to example.com and verify title")

      # With options
      {:ok, result} = Exhub.Hercules.Runner.run(input, planner_model: "kimi-k2.6", timeout: 300_000)

      # Query results
      Exhub.Hercules.Runner.last_results()
      Exhub.Hercules.Runner.list_runs()
  """

  use GenServer
  require Logger

  alias Exhub.Hercules.Factory
  alias Sagents.AgentServer
  alias Sagents.State

  @default_timeout 600_000

  # ─── Client API ──────────────────────────────────────────────────────────

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Run a test synchronously. Blocks until the planner agent completes.

  ## Options

    * `:planner_model` — LLM config name for the planner
    * `:nav_model` — LLM config name for nav agents
    * `:max_planner_rounds` — Max planner LLM calls (default: 500)
    * `:max_nav_rounds` — Max nav agent LLM calls (default: 50)
    * `:test_data` — Test data string to inject
    * `:timeout` — Max wait time in ms (default: 600_000)

  """
  def run(feature_input, opts \\ []) do
    GenServer.call(__MODULE__, {:run, feature_input, opts}, :infinity)
  end

  @doc "Get results from the most recent test run."
  def last_results do
    GenServer.call(__MODULE__, :last_results)
  end

  @doc "List all completed test runs (newest first)."
  def list_runs do
    GenServer.call(__MODULE__, :list_runs)
  end

  # ─── Server Callbacks ────────────────────────────────────────────────────

  @impl true
  def init(_opts) do
    {:ok, %{runs: []}}
  end

  @impl true
  def handle_call({:run, input, opts}, _from, state) do
    result = do_run(input, opts)
    {:reply, result, %{state | runs: [result | state.runs]}}
  end

  @impl true
  def handle_call(:last_results, _from, state) do
    {:reply, List.first(state.runs), state}
  end

  @impl true
  def handle_call(:list_runs, _from, state) do
    summaries =
      Enum.map(state.runs, fn run ->
        %{
          run_id: run.run_id,
          status: run.status,
          started_at: run.started_at,
          duration_ms: run.duration_ms,
          is_passed: run.is_passed
        }
      end)

    {:reply, summaries, state}
  end

  # ─── Core Execution ──────────────────────────────────────────────────────

  defp do_run(input, opts) do
    run_id = "hercules_#{System.unique_integer([:positive])}"
    started_at = DateTime.utc_now()
    start_mono = System.monotonic_time(:millisecond)

    Logger.info("[Hercules.Runner] Starting run #{run_id}")

    result =
      case Factory.create_planner_agent(opts) do
        {:ok, agent} ->
          run_agent(agent, input, opts)

        {:error, reason} ->
          Logger.error("[Hercules.Runner] Failed to create planner agent: #{inspect(reason)}")
          %{error: reason, is_passed: false}
      end

    duration_ms = System.monotonic_time(:millisecond) - start_mono

    run_result = %{
      run_id: run_id,
      started_at: started_at,
      duration_ms: duration_ms,
      status: if(result[:is_passed], do: :passed, else: :failed),
      is_passed: result[:is_passed] || false,
      assert_summary: result[:assert_summary] || "",
      final_response: result[:final_response] || "",
      terminate: result[:terminate] || "yes",
      plan: result[:plan] || "",
      metadata: result[:metadata] || %{},
      error: result[:error]
    }

    Logger.info("[Hercules.Runner] Run #{run_id} completed: #{run_result.status} (#{duration_ms}ms)")

    Exhub.Metrics.PerformanceTracker.record_hercules_run(
      run_id,
      duration_ms,
      status: if(result[:is_passed], do: :success, else: :error),
      error_message: if(result[:error], do: inspect(result[:error]))
    )

    run_result
  end

  defp run_agent(agent, input, opts) do
    timeout = opts[:timeout] || @default_timeout
    agent_id = agent.agent_id

    # Start AgentServer for this run
    initial_state = State.new!(%{})

    case AgentServer.start_link(
           agent: agent,
           initial_state: initial_state,
           inactivity_timeout: timeout + 60_000
         ) do
      {:ok, _pid} ->
        AgentServer.subscribe(agent_id)

        # Send the test task as a user message
        user_message = LangChain.Message.new_user!(input)

        case AgentServer.add_message(agent_id, user_message) do
          :ok ->
            completion = wait_for_completion(agent_id, timeout)
            final_state = AgentServer.get_state(agent_id)
            AgentServer.stop(agent_id)
            build_result(final_state, completion)

          {:error, reason} ->
            Logger.error("[Hercules.Runner] add_message failed: #{inspect(reason)}")
            try_stop(agent_id)
            %{error: reason, is_passed: false}
        end

      {:error, reason} ->
        Logger.error("[Hercules.Runner] AgentServer start failed: #{inspect(reason)}")
        %{error: reason, is_passed: false}
    end
  end

  defp wait_for_completion(agent_id, timeout) do
    deadline = System.monotonic_time(:millisecond) + timeout
    do_wait(agent_id, deadline)
  end

  defp do_wait(agent_id, deadline) do
    remaining = deadline - System.monotonic_time(:millisecond)

    if remaining <= 0 do
      Logger.warning("[Hercules.Runner] agent='#{agent_id}' timeout")
      {:error, :timeout}
    else
      receive do
        {:agent, {:status_changed, :running, _}} ->
          do_wait(agent_id, deadline)

        {:agent, {:status_changed, :idle, _}} ->
          Logger.info("[Hercules.Runner] agent='#{agent_id}' completed (idle)")
          :completed

        {:agent, {:status_changed, :error, reason}} ->
          Logger.error("[Hercules.Runner] agent='#{agent_id}' error: #{inspect(reason)}")
          {:error, reason}

        {:agent, {:status_changed, :interrupted, _data}} ->
          {:error, :interrupted}

        {:agent, {:tool_execution_update, status, info}} ->
          Logger.debug("[Hercules.Runner] tool #{info.name} -> #{status}")
          do_wait(agent_id, deadline)

        {:agent, _other} ->
          do_wait(agent_id, deadline)
      after
        remaining ->
          Logger.warning("[Hercules.Runner] agent='#{agent_id}' timeout after #{remaining}ms")
          {:error, :timeout}
      end
    end
  end

  defp build_result(state, completion) do
    metadata = state.metadata || %{}

    %{
      is_passed: metadata["hercules_is_passed"] || false,
      assert_summary: metadata["hercules_assert_summary"] || "",
      final_response: metadata["hercules_final_response"] || extract_last_text(state),
      terminate: metadata["hercules_terminate"] || "yes",
      plan: metadata["hercules_plan"] || "",
      metadata: metadata,
      completion: completion
    }
  end

  defp extract_last_text(state) do
    case List.last(state.messages) do
      %LangChain.Message{role: :assistant, content: content} when is_binary(content) ->
        content

      %LangChain.Message{role: :assistant, content: content} when is_list(content) ->
        Enum.map_join(content, "", fn
          s when is_binary(s) -> s
          %LangChain.Message.ContentPart{type: :text, content: c} -> c || ""
          _ -> ""
        end)

      _ ->
        ""
    end
  end

  defp try_stop(agent_id) do
    try do
      AgentServer.stop(agent_id)
    rescue
      _ -> :ok
    end
  end
end
