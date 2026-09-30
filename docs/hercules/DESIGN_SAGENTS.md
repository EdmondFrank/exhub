# Design Doc Addendum: Sagents-Based Architecture

**Version:** 2.0  
**Date:** 2026-07-21  
**Status:** Draft  
**Supersedes:** DESIGN.md §5–§8 (Orchestrator, Planner, NavAgents, Runner)

---

## 1. Design Rationale

The original design used a custom Task-based orchestrator with raw LangChain.ex calls. After deeper analysis of the Sagents framework, we can achieve a **much cleaner architecture** by modeling Hercules as:

> **One Planner Agent + N SubAgents (nav agents) via the SubAgent middleware.**

This eliminates ~400 lines of custom orchestration code and gains:
- Built-in multi-turn tool-calling loop (`AgentExecution` mode)
- Process isolation & supervision (SubAgentServer under DynamicSupervisor)
- Context overflow handling (Summarization middleware)
- HITL interrupt propagation (HumanInTheLoop middleware)
- Real-time event streaming (AgentServer pub/sub)
- Token-efficient delegation (parent only sees SubAgent's final result)

---

## 2. Concept Mapping

| Hercules (Python) | Sagents-Based (Elixir) |
|---|---|
| `SimpleHercules` (LangGraph state machine) | `Sagents.Agent` (Planner) + `SubAgent` middleware |
| `PlannerAgent` (LLM + JSON output) | Planner Agent's `base_system_prompt` + structured output |
| `target_helper` routing | SubAgent `task` tool's `task_name` enum |
| `BrowserNavAgent` multi-turn tool loop | `SubAgent.Config` → `AgentExecution` mode (built-in loop) |
| `##TERMINATE TASK##` sentinel | SubAgent completes when LLM stops calling tools |
| `max_nav_rounds` | `SubAgent.Config.max_runs` |
| `max_planner_rounds` | Planner Agent's `max_runs` |
| `_compress_messages()` | `Sagents.Middleware.Summarization` |
| `tool_registry` + `@tool` decorator | `McpAdapter.build_tools([:browser_use])` → `LangChain.Function` |
| `PlaywrightManager` | `KuriDaemon` + `Exhub.MCP.Tools.BrowserUse.*` |
| `BaseRunner` | `Exhub.Hercules.Runner` (GenServer wrapping AgentServer) |
| Agent inner thoughts log | AgentServer pub/sub events + State.messages |

---

## 3. Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────────┐
│                     Exhub.Hercules.Runner (GenServer)                    │
│  • Manages test run lifecycle                                            │
│  • Parses Gherkin → scenarios                                           │
│  • Starts AgentServer per run                                           │
│  • Collects results, generates JUnit XML                                │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │ AgentServer.start_link / add_message
                                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│              Planner Agent (Sagents.Agent + AgentServer)                 │
│                                                                          │
│  base_system_prompt: "You are a test execution planner..."              │
│  max_runs: 500                                                           │
│  middleware:                                                             │
│    ├── SubAgent (nav agents as pre-configured subagents)                 │
│    ├── Summarization (context overflow → auto-compress)                  │
│    ├── PatchToolCalls (fix dangling tool calls)                          │
│    └── Hercules.TestTracker (custom: parse JSON, track assertions)       │
│                                                                          │
│  The Planner calls the `task` tool to delegate steps:                    │
│    task(instructions: "Navigate to...", task_name: "browser")            │
│    task(instructions: "Send POST...", task_name: "api")                  │
│    task(instructions: "Wait 5s", task_name: "time_keeper")              │
│                                                                          │
│  After each task result, Planner decides:                                │
│    → next step (call task again)                                         │
│    → assert (output JSON with is_assert=true)                            │
│    → terminate (output JSON with terminate=yes)                          │
└────────────────────────────────┬────────────────────────────────────────┘
                                 │ task tool → SubAgentServer
                                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│                    SubAgents (isolated processes)                         │
│                                                                          │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────────────┐  │
│  │ "browser"       │  │ "api"           │  │ "time_keeper"           │  │
│  │                 │  │                 │  │                         │  │
│  │ system_prompt:  │  │ system_prompt:  │  │ system_prompt:          │  │
│  │ "Web Nav Agent" │  │ "API Agent"     │  │ "Wait/sleep agent"      │  │
│  │                 │  │                 │  │                         │  │
│  │ tools:          │  │ tools:          │  │ tools:                  │  │
│  │ • navigate      │  │ • http_request  │  │ • wait                  │  │
│  │ • click         │  │                 │  │                         │  │
│  │ • fill          │  │ max_runs: 20    │  │ max_runs: 5             │  │
│  │ • snapshot      │  └─────────────────┘  └─────────────────────────┘  │
│  │ • screenshot    │                                                    │
│  │ • get_text      │  ┌─────────────────┐  ┌─────────────────────────┐  │
│  │ • press_key     │  │ "sql"           │  │ "mcp"                   │  │
│  │ • hover         │  │                 │  │                         │  │
│  │ • select        │  │ tools:          │  │ tools:                  │  │
│  │                 │  │ • execute_query │  │ • call_mcp_tool         │  │
│  │ max_runs: 50    │  │                 │  │ • list_mcp_tools        │  │
│  └─────────────────┘  │ max_runs: 20    │  │                         │  │
│                        └─────────────────┘  │ max_runs: 30            │  │
│                                             └─────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────┘
                                 │
                                 │ LangChain.Function calls
                                 ▼
┌─────────────────────────────────────────────────────────────────────────┐
│              Existing ExHub Infrastructure                               │
│                                                                          │
│  ┌──────────────────────┐  ┌──────────────────┐  ┌──────────────────┐   │
│  │ KuriDaemon (CDP)     │  │ Req / Finch      │  │ MCP Hub          │   │
│  │ + BrowserUse tools   │  │ (HTTP client)    │  │ (upstream MCP)   │   │
│  └──────────────────────┘  └──────────────────┘  └──────────────────┘   │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## 4. Implementation

### 4.1 Factory: Building the Hercules Agent

```elixir
defmodule Exhub.Hercules.Factory do
  @moduledoc """
  Builds the Hercules Planner agent with nav-agent subagents.
  """

  alias Sagents.Agent
  alias Sagents.SubAgent
  alias Exhub.Sagents.McpAdapter
  alias Exhub.Llm.LlmConfigServer

  @doc "Create the Hercules Planner agent struct."
  def create_planner_agent(opts \\ []) do
    {:ok, planner_model} = build_model(opts[:planner_model] || "openai/gpt-4o")
    {:ok, nav_model} = build_model(opts[:nav_model] || "openai/gpt-4o")

    # Build nav agent subagents
    subagents = [
      browser_subagent(nav_model, opts),
      api_subagent(nav_model, opts),
      time_keeper_subagent(nav_model, opts),
      sql_subagent(nav_model, opts),
      mcp_subagent(nav_model, opts)
    ]

    Agent.new!(
      %{
        agent_id: "hercules_planner_#{System.unique_integer([:positive])}",
        model: planner_model,
        base_system_prompt: planner_system_prompt(opts),
        max_runs: opts[:max_planner_rounds] || 500,
        middleware: [
          {Sagents.Middleware.SubAgent, [
            model: nav_model,
            subagents: subagents,
            block_middleware: [Sagents.Middleware.ConversationTitle]
          ]},
          {Sagents.Middleware.Summarization, [
            model: planner_model,
            max_tokens_before_summary: 100_000,
            messages_to_keep: 10
          ]},
          {Sagents.Middleware.PatchToolCalls, []},
          {Exhub.Hercules.Middleware.TestTracker, []}
        ]
      },
      replace_default_middleware: true
    )
  end

  # --- SubAgent Configs ---

  defp browser_subagent(model, opts) do
    browser_tools = McpAdapter.build_tools([:browser_use])

    SubAgent.Config.new!(%{
      name: "browser",
      description: "Web browser navigation: open URLs, click elements, fill forms, " <>
                   "read page content, take screenshots. Use for all web UI interactions.",
      system_prompt: browser_nav_prompt(),
      tools: browser_tools,
      max_runs: opts[:max_nav_rounds] || 50
    })
  end

  defp api_subagent(model, opts) do
    api_tools = [http_request_tool()]

    SubAgent.Config.new!(%{
      name: "api",
      description: "HTTP API testing: send GET/POST/PUT/DELETE requests, " <>
                   "validate response status codes and body content.",
      system_prompt: api_nav_prompt(),
      tools: api_tools,
      max_runs: opts[:max_nav_rounds] || 20
    })
  end

  defp time_keeper_subagent(model, _opts) do
    SubAgent.Config.new!(%{
      name: "time_keeper",
      description: "Time-related operations: wait/sleep for specified duration.",
      system_prompt: "You are a time keeper. Use the wait tool to pause execution.",
      tools: [wait_tool()],
      max_runs: 5
    })
  end

  defp sql_subagent(model, _opts) do
    SubAgent.Config.new!(%{
      name: "sql",
      description: "Database operations: execute SQL queries and validate results.",
      system_prompt: sql_nav_prompt(),
      tools: [],  # Phase 2: add SQL tools
      max_runs: 20
    })
  end

  defp mcp_subagent(model, _opts) do
    mcp_tools = McpAdapter.build_tools([:hub])

    SubAgent.Config.new!(%{
      name: "mcp",
      description: "MCP server tool execution: call tools on connected MCP servers.",
      system_prompt: mcp_nav_prompt(),
      tools: mcp_tools,
      max_runs: 30
    })
  end

  # --- Model Builder ---

  defp build_model(model_name) do
    case LlmConfigServer.get_llm_config(model_name) do
      {:ok, config} -> {:ok, Exhub.Sagents.Factory.create_langchain_model(config)}
      {:error, _} ->
        # Fallback: parse "provider/model" format
        [provider, model] = String.split(model_name, "/", parts: 2)
        {:ok, Exhub.Sagents.Factory.create_langchain_model(%{
          model: model_name,
          api_key: System.get_env("MODEL_API_KEY"),
          api_base: System.get_env("MODEL_API_BASE") || "https://api.openai.com/v1"
        })}
    end
  end
end
```

### 4.2 Custom Middleware: TestTracker

The TestTracker middleware parses the Planner's JSON output after each LLM call and tracks test state (assertions, pass/fail, plan progress).

```elixir
defmodule Exhub.Hercules.Middleware.TestTracker do
  @moduledoc """
  Custom Sagents middleware that:
  1. Parses the Planner's structured JSON responses
  2. Tracks assertion results (is_assert, is_passed, assert_summary)
  3. Injects test data into the system prompt
  4. Detects termination conditions

  Runs as an after_model hook to inspect the Planner's latest output.
  """

  @behaviour Sagents.Middleware

  @impl true
  def init(opts) do
    test_data = Keyword.get(opts, :test_data, "")
    {:ok, %{test_data: test_data, assertions: []}}
  end

  @impl true
  def system_prompt(config) do
    if config.test_data != "" do
      "\n\n## Available Test Data\n\n#{config.test_data}\n"
    else
      ""
    end
  end

  @impl true
  def tools(_config), do: []

  @impl true
  def after_model(state, config) do
    # Parse the last assistant message for JSON planner output
    case List.last(state.messages) do
      %LangChain.Message{role: :assistant, content: content} when is_binary(content) ->
        case parse_planner_json(content) do
          {:ok, parsed} ->
            # Store assertion data in state metadata
            metadata = Map.merge(state.metadata, %{
              "hercules_plan" => parsed["plan"],
              "hercules_terminate" => parsed["terminate"],
              "hercules_is_assert" => parsed["is_assert"],
              "hercules_is_passed" => parsed["is_passed"],
              "hercules_assert_summary" => parsed["assert_summary"],
              "hercules_final_response" => parsed["final_response"]
            })
            {:ok, %{state | metadata: metadata}}

          :no_json ->
            {:ok, state}
        end

      _ ->
        {:ok, state}
    end
  end

  defp parse_planner_json(content) do
    cleaned = content
      |> String.replace("```json", "")
      |> String.replace("```", "")
      |> String.trim()

    case Jason.decode(cleaned) do
      {:ok, %{"terminate" => _} = map} -> {:ok, map}
      _ -> :no_json
    end
  end
end
```

### 4.3 Runner (GenServer wrapping AgentServer)

```elixir
defmodule Exhub.Hercules.Runner do
  @moduledoc """
  Manages Hercules test run lifecycle using Sagents AgentServer.

  Each test run:
  1. Parses Gherkin input → scenarios
  2. Creates a Planner agent via Factory
  3. Starts an AgentServer for the run
  4. Sends the test task as a user message
  5. Waits for completion (agent goes idle)
  6. Extracts results from state metadata
  7. Generates JUnit XML + proof artifacts
  """

  use GenServer
  require Logger

  alias Exhub.Hercules.{Factory, Gherkin, Reporting}
  alias Sagents.{AgentServer, State}

  # --- Client API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Run a test synchronously. Blocks until complete."
  def run(feature_input, opts \\ []) do
    GenServer.call(__MODULE__, {:run, feature_input, opts}, :infinity)
  end

  @doc "Run a test asynchronously. Returns {:ok, run_id}."
  def run_async(feature_input, opts \\ []) do
    GenServer.call(__MODULE__, {:run_async, feature_input, opts})
  end

  def last_results, do: GenServer.call(__MODULE__, :last_results)
  def list_runs, do: GenServer.call(__MODULE__, :list_runs)

  # --- Server Callbacks ---

  @impl true
  def init(_opts) do
    {:ok, %{runs: [], active: %{}}}
  end

  @impl true
  def handle_call({:run, input, opts}, _from, state) do
    result = do_run(input, opts)
    {:reply, result, %{state | runs: [result | state.runs]}}
  end

  @impl true
  def handle_call({:run_async, input, opts}, from, state) do
    run_id = "run_#{System.unique_integer([:positive])}"
    task = Task.async(fn -> do_run(input, Keyword.put(opts, :run_id, run_id)) end)
    {:reply, {:ok, run_id}, %{state | active: Map.put(state.active, task.ref, {task, from})}}
  end

  @impl true
  def handle_info({ref, result}, state) when is_reference(ref) do
    case Map.pop(state.active, ref) do
      {{_task, from}, rest} ->
        GenServer.reply(from, result)
        {:noreply, %{state | active: rest, runs: [result | state.runs]}}
      {nil, _} ->
        {:noreply, %{state | runs: [result | state.runs]}}
    end
  end

  # --- Core Execution ---

  defp do_run(input, opts) do
    run_id = opts[:run_id] || "run_#{System.unique_integer([:positive])}"
    proof_path = setup_proof_dir(run_id)

    # 1. Parse input into scenarios
    scenarios = Gherkin.Parser.parse(input)
    test_data = load_test_data(opts)

    # 2. Run each scenario
    results = Enum.map(scenarios, fn scenario ->
      run_scenario(scenario, opts, test_data, proof_path)
    end)

    # 3. Generate reports
    junit_path = Reporting.JunitXml.generate(results, proof_path)

    %{
      run_id: run_id,
      status: if(Enum.all?(results, & &1.is_passed), do: :passed, else: :failed),
      scenarios: results,
      proof_path: proof_path,
      junit_xml_path: junit_path
    }
  end

  defp run_scenario(scenario, opts, test_data, proof_path) do
    # Build the task string from scenario steps
    task = scenario_to_task(scenario, test_data)

    # Create the Planner agent
    agent = Factory.create_planner_agent(
      Keyword.merge(opts, test_data: test_data)
    )

    # Start AgentServer for this run
    initial_state = State.new!(%{
      messages: [LangChain.Message.new_user!(task)]
    })

    {:ok, _pid} = AgentServer.start_link(
      agent: agent,
      initial_state: initial_state,
      inactivity_timeout: 600_000  # 10 min
    )

    agent_id = agent.agent_id
    AgentServer.subscribe(agent_id)

    # Trigger execution
    :ok = AgentServer.add_message(agent_id, LangChain.Message.new_user!(task))

    # Wait for completion
    result = wait_for_completion(agent_id, opts[:timeout] || 600_000)

    # Extract test results from state metadata
    final_state = AgentServer.get_state(agent_id)
    AgentServer.stop(agent_id)

    build_scenario_result(scenario, final_state, result)
  end

  defp wait_for_completion(agent_id, timeout) do
    deadline = System.monotonic_time(:millisecond) + timeout

    receive do
      {:agent, {:status_changed, :idle, _}} -> :completed
      {:agent, {:status_changed, :error, reason}} -> {:error, reason}
      {:agent, _} -> wait_for_completion(agent_id, deadline - System.monotonic_time(:millisecond))
    after
      timeout -> {:error, :timeout}
    end
  end

  defp build_scenario_result(scenario, state, completion) do
    metadata = state.metadata || %{}

    %{
      scenario_name: scenario.name,
      is_passed: metadata["hercules_is_passed"] || false,
      assert_summary: metadata["hercules_assert_summary"] || "",
      final_response: metadata["hercules_final_response"] || "",
      terminate: metadata["hercules_terminate"] || "yes",
      completion: completion
    }
  end
end
```

### 4.4 Planner System Prompt

```elixir
defp planner_system_prompt(opts) do
  """
  # Test Execution Task Planner

  You are a test execution task planner that processes Gherkin BDD features
  and executes them through specialized helper agents using the `task` tool.

  ## How You Work

  1. Parse the test into a step-by-step plan
  2. For each step, delegate to the appropriate helper via the `task` tool
  3. After each helper completes, analyze the result
  4. Continue until all steps are done, then terminate

  ## Delegation via `task` Tool

  Use the `task` tool to delegate work to helpers:
  - `task_name: "browser"` — Web navigation, clicking, form filling, screenshots
  - `task_name: "api"` — HTTP requests, response validation
  - `task_name: "sql"` — Database queries
  - `task_name: "mcp"` — MCP server tool execution
  - `task_name: "time_keeper"` — Wait/sleep operations

  ## Response Format

  After each helper result, respond with JSON:
  ```json
  {
    "plan": "Numbered step-by-step plan with (Completed) markers",
    "next_step": "What to delegate next (or empty if done)",
    "terminate": "yes|no",
    "final_response": "Outcome summary (when terminate=yes)",
    "is_assert": true/false,
    "assert_summary": "EXPECTED: x\\nACTUAL: y",
    "is_passed": true/false,
    "target_helper": "browser|api|sql|mcp|time_keeper|not_applicable"
  }
  ```

  ## Critical Rules

  1. Focus on WHAT to accomplish, not HOW (helpers decide implementation)
  2. Include explicit closure conditions in each task instruction
  3. After ALL plan steps show (Completed), set terminate="yes"
  4. If a step fails after multiple attempts, terminate with failure
  5. NEVER invent test steps beyond what the test case requires
  6. Each `task` instruction must be self-contained (helpers have no context)

  ## Termination Logic

  - Set terminate="yes" when all steps are completed
  - Set terminate="yes" on unrecoverable failure
  - Always include a final assertion before terminating
  """
end
```

### 4.5 Browser Nav Agent Prompt

```elixir
defp browser_nav_prompt do
  """
  # Web Navigation Agent

  You are a specialized web navigation agent. Execute precise webpage
  interactions using the available browser tools.

  ## Rules

  1. Analyze the page (snapshot) BEFORE interacting
  2. Use accessibility refs from snapshots to target elements
  3. Execute one action at a time, verify result before next
  4. Handle popups/modals/cookie notices first
  5. After completing the task, summarize what was done

  ## Response Format

  When done:
  ```
  previous_step: [summary]
  current_output: [what was accomplished]
  Data: [extracted values if any]
  ```

  ## Tools Available

  - navigate: Open a URL
  - click: Click an element (by ref or selector)
  - fill: Type text into an input field
  - snapshot: Get accessibility tree of current page
  - screenshot: Capture page screenshot
  - get_page_text: Extract all text content
  - press_key: Press keyboard keys (Enter, Tab, etc.)
  - select_option: Choose dropdown option
  - hover: Hover over an element
  """
end
```

---

## 5. Key Advantages of Sagents-Based Design

| Aspect | Custom Orchestrator (v1) | Sagents-Based (v2) |
|---|---|---|
| **Tool loop** | Manual `do_nav_loop` recursion | Built-in `AgentExecution` mode |
| **Context overflow** | Custom `_compress_messages` | `Summarization` middleware (auto) |
| **Process isolation** | Manual Task spawning | `SubAgentServer` under DynamicSupervisor |
| **HITL** | Not supported | `HumanInTheLoop` middleware (free) |
| **Event streaming** | Custom pub/sub | `AgentServer.subscribe` (built-in) |
| **Middleware composition** | N/A | Composable hooks (before/after model) |
| **Fallback models** | Manual retry logic | `Agent.fallback_models` (built-in) |
| **Max rounds** | Manual counter | `max_runs` on Agent + SubAgent.Config |
| **Code volume** | ~800 lines custom | ~300 lines (Factory + TestTracker + Runner) |
| **Maintenance** | Own codebase | Inherits Sagents improvements |

---

## 6. Execution Flow (Sequence)

```
User → Runner.run(gherkin)
  │
  ├─ Gherkin.Parser.parse() → [Scenario]
  │
  ├─ Factory.create_planner_agent() → Agent struct
  │     ├── SubAgent middleware (browser, api, sql, mcp, time_keeper)
  │     ├── Summarization middleware
  │     ├── PatchToolCalls middleware
  │     └── TestTracker middleware
  │
  ├─ AgentServer.start_link(agent, initial_state)
  │
  ├─ AgentServer.add_message(agent_id, user_msg(task))
  │     │
  │     ▼
  │   AgentExecution mode loop:
  │     │
  │     ├─ LLM call → Planner produces JSON + tool_call(task)
  │     │
  │     ├─ SubAgent middleware intercepts `task` tool call
  │     │     │
  │     │     ├─ Spawns SubAgentServer (e.g., "browser")
  │     │     │     │
  │     │     │     ├─ SubAgent AgentExecution loop:
  │     │     │     │     ├─ LLM → tool_call(navigate)
  │     │     │     │     ├─ Execute: kuri-agent CDP navigate
  │     │     │     │     ├─ LLM → tool_call(snapshot)
  │     │     │     │     ├─ Execute: kuri-agent CDP snapshot
  │     │     │     │     ├─ LLM → tool_call(click)
  │     │     │     │     ├─ Execute: kuri-agent CDP click
  │     │     │     │     └─ LLM → final text (no tools) → done
  │     │     │     │
  │     │     │     └─ Returns final message to parent
  │     │     │
  │     │     └─ ToolResult fed back to Planner
  │     │
  │     ├─ TestTracker.after_model: parse JSON, track assertions
  │     │
  │     ├─ Planner sees helper result → decides next step
  │     │     ├─ More steps? → call task again
  │     │     └─ All done? → output terminate=yes JSON
  │     │
  │     └─ LLM stops (no tool calls, no needs_response) → idle
  │
  ├─ Runner receives :idle event
  │
  ├─ Extract state.metadata (assertions, pass/fail)
  │
  ├─ Reporting.JunitXml.generate()
  │
  └─ Return RunResult
```

---

## 7. MCP Server (unchanged from v1)

The MCP server interface remains the same — it wraps the Runner:

```elixir
defmodule Exhub.Hercules.MCPServer do
  use Anubis.Server, name: "hercules"

  tool "run_test" do
    description "Execute an E2E test from Gherkin or plain English"
    param :feature, :string, required: true
    param :config, :map, default: %{}

    handler fn params ->
      result = Exhub.Hercules.Runner.run(params["feature"], params["config"])
      H.toon_response(resp, result)
    end
  end

  tool "get_test_results" do
    description "Get results from the most recent test run"
    handler fn _ ->
      H.toon_response(resp, Exhub.Hercules.Runner.last_results())
    end
  end

  tool "generate_gherkin" do
    description "Convert plain English to Gherkin"
    param :description, :string, required: true
    handler fn params ->
      gherkin = Exhub.Hercules.Gherkin.Generator.generate(params["description"])
      H.toon_response(resp, %{gherkin: gherkin})
    end
  end
end
```

---

## 8. Supervision Tree

```elixir
# In Exhub.Application children:
{Exhub.Hercules.Runner, name: Exhub.Hercules.Runner},
{Exhub.Hercules.MCPServer,
  transport: :streamable_http,
  request_timeout: 600_000,
  session_idle_timeout: 86_400_000 * 365},
```

The AgentServer and SubAgentServer processes are supervised by `Sagents.Supervisor` (already in the tree).

---

## 9. Module Structure (Revised)

```
lib/exhub/hercules/
├── runner.ex                    # GenServer: test run lifecycle
├── factory.ex                   # Builds Planner Agent + SubAgent configs
├── config.ex                    # Runtime config (models, rounds, paths)
├── test_data.ex                 # Load test data files
│
├── middleware/
│   └── test_tracker.ex          # Custom middleware: parse JSON, track assertions
│
├── prompts/
│   ├── planner.ex               # Planner system prompt
│   ├── browser_nav.ex           # Browser nav agent prompt
│   ├── api_nav.ex               # API nav agent prompt
│   └── mcp_nav.ex               # MCP nav agent prompt
│
├── tools/
│   └── api/
│       └── http_request.ex      # LangChain.Function for HTTP requests
│
├── gherkin/
│   ├── parser.ex                # Parse .feature files
│   ├── generator.ex             # Plain English → Gherkin (LLM)
│   └── types.ex                 # Feature, Scenario, Step structs
│
├── reporting/
│   ├── junit_xml.ex             # JUnit XML output
│   └── proof_logger.ex          # Screenshots + interaction logs
│
├── mcp_server.ex                # Anubis MCP server
└── mcp_tools/
    ├── run_test.ex
    ├── generate_gherkin.ex
    └── get_test_results.ex
```

Note: **No `orchestrator.ex`, no `nav_agents/` directory, no `tools/browser/`** — all handled by Sagents + McpAdapter.

---

## 10. Decision Log (Updated)

| # | Decision | Rationale |
|---|---|---|
| D1 | ~~Task-based orchestrator~~ → **Sagents Agent + SubAgent middleware** | Eliminates ~500 lines of custom loop code; gains HITL, summarization, event streaming for free |
| D2 | Reuse kuri-agent (not Playwright) | Already in ExHub, Zig binary is fast, no Python dependency |
| D3 | ~~Raw LangChain.ex~~ → **Sagents AgentExecution mode** | Built-in multi-turn tool loop with max_runs, fallback models, pause/resume |
| D4 | Anubis for MCP server | Consistent with all other ExHub MCP servers |
| D5 | File-based proof output | Simpler for v1; JUnit XML is the standard interface |
| D6 | Separate planner/nav model configs | Allows cheap model for nav, expensive for planning |
| D7 | ~~`##TERMINATE TASK##` sentinel~~ → **SubAgent natural completion** | SubAgent completes when LLM stops calling tools; cleaner than sentinel parsing |
| D8 | Custom `TestTracker` middleware | Minimal custom code (~80 lines) for assertion tracking; everything else is Sagents |
| D9 | `McpAdapter.build_tools([:browser_use])` for browser tools | Reuses existing MCP tool definitions; no duplicate tool implementations |
| D10 | `Summarization` middleware for context overflow | Replaces Hercules's custom `_compress_messages`; model-aware, tested |
