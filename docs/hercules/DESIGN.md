# Design Doc: ExHub Hercules — Technical Architecture

**Version:** 1.0  
**Date:** 2026-07-21  
**Status:** Draft  
**PRD:** [docs/hercules/PRD.md](./PRD.md)

---

## 1. Overview

This document describes the technical architecture for porting TestZeus Hercules to Elixir/OTP, running inside the ExHub application. The design maximizes reuse of existing ExHub infrastructure while faithfully reproducing Hercules's Planner→Executor→Assertion orchestration model.

---

## 2. System Context

```
┌────────────────────────────────────────────────────────────────────────┐
│                         ExHub BEAM Application                          │
│                                                                        │
│  ┌──────────────────────────────────────────────────────────────────┐  │
│  │                    Exhub.Hercules (NEW)                           │  │
│  │                                                                  │  │
│  │  ┌────────────┐   ┌──────────────┐   ┌───────────────────────┐  │  │
│  │  │   Runner   │──▶│ Orchestrator │──▶│     Nav Agents        │  │  │
│  │  │ (GenServer)│   │  (Task-based │   │  Browser│API│SQL│Time │  │  │
│  │  │            │   │   state mach)│   └───────────┬───────────┘  │  │
│  │  └────────────┘   └──────────────┘               │              │  │
│  │        │                  │                       │              │  │
│  │        │           ┌──────┴──────┐               │              │  │
│  │        │           │   Planner   │               │              │  │
│  │        │           │  (LLM+JSON) │               │              │  │
│  │        │           └─────────────┘               │              │  │
│  │        │                                         │              │  │
│  │  ┌─────┴─────┐   ┌──────────────┐               │              │  │
│  │  │ MCP Server│   │  Reporting   │               │              │  │
│  │  │ (Anubis)  │   │ JUnit/Proof  │               │              │  │
│  │  └───────────┘   └──────────────┘               │              │  │
│  └──────────────────────────────────────────────────┼──────────────┘  │
│                                                     │                  │
│  ┌──────────────────────────────────────────────────┼──────────────┐  │
│  │              Existing ExHub Infrastructure        │              │  │
│  │                                                  ▼              │  │
│  │  ┌─────────────┐  ┌────────────┐  ┌─────────────────────────┐  │  │
│  │  │ LlmConfig   │  │ LangChain  │  │  KuriDaemon (CDP)       │  │  │
│  │  │ Server      │  │ .ex        │  │  + BrowserUse MCP Tools │  │  │
│  │  └─────────────┘  └────────────┘  └─────────────────────────┘  │  │
│  │                                                                 │  │
│  │  ┌─────────────┐  ┌────────────┐  ┌─────────────────────────┐  │  │
│  │  │ MCP Hub     │  │ Desktop    │  │  Web Tools / Brain      │  │  │
│  │  │ (upstream)  │  │ Tools      │  │                         │  │  │
│  │  └─────────────┘  └────────────┘  └─────────────────────────┘  │  │
│  └─────────────────────────────────────────────────────────────────┘  │
└────────────────────────────────────────────────────────────────────────┘
         ▲                                          │
         │ MCP (streamable-http)                    │ CDP (WebSocket)
         │                                          ▼
┌────────┴────────┐                      ┌─────────────────────┐
│  MCP Clients    │                      │   Chrome Browser    │
│  (Claude Code,  │                      │   (via kuri-agent)  │
│   AiderDesk)    │                      └─────────────────────┘
└─────────────────┘
```

---

## 3. Module Structure

```
lib/exhub/hercules/
├── runner.ex                    # GenServer: test run lifecycle management
├── orchestrator.ex              # Core Planner→Executor→Assertion loop
├── planner.ex                   # PlannerAgent: LLM call + JSON parse
├── state.ex                     # AgentState struct (TypedStruct)
├── config.ex                    # Runtime config (rounds, timeouts, models)
├── test_data.ex                 # Load test data (YAML/JSON/txt)
│
├── nav_agents/
│   ├── behaviour.ex             # @callback execute(task, opts) :: String.t()
│   ├── browser.ex               # Browser nav agent (kuri-agent CDP tools)
│   ├── api.ex                   # API nav agent (Req HTTP tools)
│   ├── sql.ex                   # SQL nav agent (phase 2)
│   ├── mcp.ex                   # MCP nav agent (call upstream MCP tools)
│   └── time_keeper.ex           # Wait/sleep agent
│
├── tools/
│   ├── browser/
│   │   ├── navigate.ex          # open_url → kuri navigate
│   │   ├── click.ex             # click element
│   │   ├── fill.ex              # type into input
│   │   ├── select.ex            # dropdown selection
│   │   ├── press_key.ex         # keyboard shortcuts
│   │   ├── snapshot.ex          # accessibility tree / DOM text
│   │   ├── screenshot.ex        # capture page screenshot
│   │   ├── get_text.ex          # extract page text content
│   │   ├── hover.ex             # hover element
│   │   └── upload.ex            # file upload
│   └── api/
│       └── http_request.ex      # GET/POST/PUT/DELETE
│
├── gherkin/
│   ├── parser.ex                # Parse .feature → structured scenarios
│   ├── generator.ex             # Plain English → Gherkin (LLM)
│   └── types.ex                 # Feature, Scenario, Step structs
│
├── reporting/
│   ├── junit_xml.ex             # Generate JUnit XML from results
│   ├── proof_logger.ex          # Screenshot + interaction log
│   └── thought_logger.ex        # Agent inner thoughts JSON
│
├── mcp_server.ex                # Anubis MCP server (run_test, get_results)
└── mcp_tools/
    ├── run_test.ex              # MCP tool: execute test
    ├── generate_gherkin.ex      # MCP tool: English → Gherkin
    ├── get_test_results.ex      # MCP tool: fetch results
    └── list_test_runs.ex        # MCP tool: history
```

---

## 4. Core Data Structures

### 4.1 AgentState

```elixir
defmodule Exhub.Hercules.State do
  @moduledoc "Mutable state threaded through the orchestration loop."

  defstruct [
    # Conversation history (list of LangChain.Message)
    :messages,
    :task,

    # Planner output fields
    :plan,
    :next_step,
    :target_helper,
    :terminate,          # "yes" | "no"
    :final_response,
    :is_assert,
    :assert_summary,
    :is_passed,

    # Counters
    :planner_turn,
    :total_steps,

    # Token accounting
    :total_prompt_tokens,
    :total_completion_tokens,
    :total_cost,

    # Timing
    :step_timings,       # [%{node, turn, duration_ms}]

    # Dedup
    :completed_step_signatures,

    # Context
    :current_url,
    :last_helper_response,

    # Run metadata
    :run_id,
    :proof_path
  ]
end
```

### 4.2 Planner Response Schema

```json
{
  "plan": "string — numbered step-by-step plan",
  "next_step": "string — single instruction for helper",
  "terminate": "yes | no",
  "final_response": "string — outcome summary (when terminate=yes)",
  "is_assert": false,
  "assert_summary": "EXPECTED: ... ACTUAL: ...",
  "is_passed": false,
  "target_helper": "browser | api | sql | mcp | time_keeper | not_applicable"
}
```

### 4.3 Test Run Result

```elixir
defmodule Exhub.Hercules.RunResult do
  defstruct [
    :run_id,
    :feature_name,
    :scenario_name,
    :status,            # :passed | :failed | :error
    :duration_ms,
    :assert_summary,
    :final_response,
    :total_steps,
    :total_tokens,
    :proof_path,        # directory with screenshots
    :thoughts_path,     # agent_inner_thoughts.json
    :junit_xml_path
  ]
end
```

---

## 5. Orchestration State Machine

### 5.1 State Transitions

```
                    ┌─────────────────────────────────────────┐
                    │                                         │
                    ▼                                         │
              ┌──────────┐                                   │
              │  START   │                                   │
              └────┬─────┘                                   │
                   │                                         │
                   ▼                                         │
              ┌──────────┐    terminate=yes    ┌─────────┐  │
              │ PLANNER  │───────────────────▶│   END   │  │
              └────┬─────┘                    └─────────┘  │
                   │                                        │
                   │ terminate=no                           │
                   │                                        │
                   ▼                                        │
              ┌──────────┐    always           ┌────────┐  │
              │ EXECUTOR │───────────────────▶│PLANNER │──┘
              └──────────┘  (helper response)  └────────┘
```

### 5.2 Orchestrator Implementation Strategy

The orchestrator runs as a **synchronous Task** (not a persistent GenServer) spawned per test run. This gives:
- Process isolation (crash in one run doesn't affect others)
- Natural backpressure (caller blocks or monitors the Task)
- Simple state threading (no GenServer state management overhead)

```elixir
defmodule Exhub.Hercules.Orchestrator do
  @moduledoc """
  Runs the Planner→Executor loop for a single test scenario.
  Spawned as a Task per run. Returns RunResult.
  """

  alias Exhub.Hercules.{State, Planner, RunResult}
  alias Exhub.Hercules.NavAgents

  @spec run(String.t(), keyword()) :: RunResult.t()
  def run(task, opts \\ []) do
    config = Exhub.Hercules.Config.new(opts)
    state = %State{
      messages: [LangChain.Message.new_user!(task)],
      task: task,
      terminate: "no",
      planner_turn: 0,
      total_steps: 0,
      run_id: config.run_id,
      proof_path: config.proof_path,
      # ... defaults
    }

    final_state = loop(state, config)
    RunResult.from_state(final_state, config)
  end

  defp loop(%State{terminate: "yes"} = state, _config), do: state

  defp loop(%State{planner_turn: t} = state, %{max_planner_rounds: max}) when t >= max do
    %{state | terminate: "yes", is_passed: false,
              final_response: "Max planner rounds (#{max}) exceeded."}
  end

  defp loop(state, config) do
    # 1. Planner step
    state = Planner.step(state, config)

    case state.terminate do
      "yes" -> state
      _ ->
        # 2. Executor step (nav agent tool loop)
        state = executor_step(state, config)
        # 3. Recurse
        loop(state, config)
    end
  end

  defp executor_step(state, config) do
    agent_module = NavAgents.resolve(state.target_helper)
    start = System.monotonic_time(:millisecond)

    helper_response = agent_module.execute(state.next_step,
      max_rounds: config.nav_max_rounds,
      run_id: state.run_id,
      proof_path: state.proof_path
    )

    duration = System.monotonic_time(:millisecond) - start

    # Feed response back to planner
    msg = LangChain.Message.new_user!("[#{state.target_helper}_agent]: #{helper_response}")

    %{state |
      messages: state.messages ++ [msg],
      total_steps: state.total_steps + 1,
      last_helper_response: helper_response,
      step_timings: state.step_timings ++ [%{node: "executor", turn: state.total_steps + 1, duration_ms: duration}]
    }
  end
end
```

---

## 6. Planner Agent

### 6.1 Design

The Planner uses LangChain.ex with **structured JSON output** (`response_format: json_schema`) to guarantee parseable responses. Falls back to regex extraction if the model doesn't support structured output.

```elixir
defmodule Exhub.Hercules.Planner do
  @moduledoc "PlannerAgent: produces next_step + routing decisions."

  alias LangChain.Chains.LLMChain
  alias LangChain.ChatModels.ChatOpenAI
  alias LangChain.Message

  @system_prompt """
  # Test Execution Task Planner
  ...(full prompt from Hercules, adapted for Elixir context)...
  """

  @json_schema %{
    "type" => "object",
    "properties" => %{
      "plan" => %{"type" => "string"},
      "next_step" => %{"type" => "string"},
      "terminate" => %{"type" => "string", "enum" => ["yes", "no"]},
      "final_response" => %{"type" => "string"},
      "is_assert" => %{"type" => "boolean"},
      "assert_summary" => %{"type" => "string"},
      "is_passed" => %{"type" => "boolean"},
      "target_helper" => %{"type" => "string",
        "enum" => ["browser", "api", "sql", "mcp", "time_keeper", "not_applicable"]}
    },
    "required" => ["plan", "next_step", "terminate", "target_helper"]
  }

  def step(state, config) do
    llm = build_llm(config)
    messages = [Message.new_system!(@system_prompt) | state.messages]

    {:ok, updated} =
      LLMChain.new!(%{llm: llm, verbose: false})
      |> LLMChain.add_messages(messages)
      |> LLMChain.run()

    content = extract_content(updated)
    parsed = parse_planner_response(content)

    %{state |
      planner_turn: state.planner_turn + 1,
      plan: parsed["plan"] || state.plan,
      next_step: parsed["next_step"] || "",
      target_helper: parsed["target_helper"] || "browser",
      terminate: parsed["terminate"] || "no",
      final_response: parsed["final_response"] || "",
      is_assert: parsed["is_assert"] || false,
      assert_summary: parsed["assert_summary"] || "",
      is_passed: parsed["is_passed"] || false,
      messages: state.messages ++ [Message.new_assistant!(content)]
    }
  end

  defp parse_planner_response(content) do
    # Try direct JSON parse, then strip markdown fences
    content
    |> String.replace("```json", "")
    |> String.replace("```", "")
    |> String.trim()
    |> Jason.decode()
    |> case do
      {:ok, map} -> map
      {:error, _} -> %{"next_step" => "", "terminate" => "yes",
                       "final_response" => "Failed to parse planner response"}
    end
  end
end
```

### 6.2 Context Compression

When the conversation exceeds the model's context window, compress older messages:

```elixir
defp compress_messages(messages) when length(messages) > 40 do
  # Keep system + first user message + last 10 messages
  # Summarize the middle into a single "COMPRESSED HISTORY" message
  [system | rest] = messages
  {old, recent} = Enum.split(rest, length(rest) - 10)
  summary = Enum.map_join(old, "\n", fn m -> "[#{m.role}] #{String.slice(to_string(m.content), 0, 200)}" end)
  [system, Message.new_user!("COMPRESSED HISTORY:\n#{summary}") | recent]
end
```

---

## 7. Nav Agent Architecture

### 7.1 Behaviour

```elixir
defmodule Exhub.Hercules.NavAgents.Behaviour do
  @moduledoc "Contract for all navigation/helper agents."

  @callback execute(task :: String.t(), opts :: keyword()) :: String.t()
  @callback tools() :: [LangChain.Function.t()]
  @callback system_prompt() :: String.t()
end
```

### 7.2 Multi-Turn Tool Loop (shared logic)

All nav agents share the same multi-turn execution pattern. Extracted into a shared module:

```elixir
defmodule Exhub.Hercules.NavAgents.ToolLoop do
  @moduledoc """
  Generic multi-turn tool-calling loop.
  Calls LLM → executes tool_calls → feeds results back → repeats
  until ##TERMINATE TASK## or max_rounds.
  """

  alias LangChain.Chains.LLMChain
  alias LangChain.Message
  alias LangChain.Function

  def run(system_prompt, task, tools, llm, max_rounds) do
    messages = [
      Message.new_system!(system_prompt),
      Message.new_user!(task)
    ]
    do_loop(messages, tools, llm, max_rounds, 0)
  end

  defp do_loop(_messages, _tools, _llm, max, turn) when turn >= max do
    "[ERROR] Max nav rounds (#{max}) reached before ##TERMINATE TASK##."
  end

  defp do_loop(messages, tools, llm, max, turn) do
    chain =
      LLMChain.new!(%{llm: llm, verbose: false})
      |> LLMChain.add_tools(tools)
      |> LLMChain.add_messages(messages)

    case LLMChain.run(chain, mode: :single) do
      {:ok, updated} ->
        last_msg = ChainResult.last_message(updated)

        cond do
          # Agent explicitly terminated
          is_binary(last_msg.content) and String.contains?(last_msg.content, "##TERMINATE TASK##") ->
            last_msg.content

          # Agent produced tool calls
          last_msg.tool_calls != [] ->
            tool_results = execute_tool_calls(last_msg.tool_calls, tools)
            tool_messages = Enum.map(tool_results, fn {call_id, result} ->
              Message.new_tool_result!(call_id, result)
            end)
            do_loop(messages ++ [last_msg | tool_messages], tools, llm, max, turn + 1)

          # Agent produced final text (no tools, no terminate marker)
          is_binary(last_msg.content) and last_msg.content != "" ->
            last_msg.content

          true ->
            do_loop(messages ++ [last_msg], tools, llm, max, turn + 1)
        end

      {:error, reason} ->
        "[ERROR] LLM call failed: #{inspect(reason)}"
    end
  end

  defp execute_tool_calls(tool_calls, tools) do
    tool_map = Map.new(tools, fn t -> {t.name, t} end)

    Enum.map(tool_calls, fn call ->
      case Map.get(tool_map, call.name) do
        nil -> {call.call_id, "[ERROR] Tool '#{call.name}' not found."}
        tool ->
          result = apply(tool.module, tool.function, [call.arguments])
          {call.call_id, to_string(result)}
      end
    end)
  end
end
```

### 7.3 Browser Nav Agent

```elixir
defmodule Exhub.Hercules.NavAgents.Browser do
  @moduledoc "Browser navigation agent using kuri-agent CDP tools."

  @behaviour Exhub.Hercules.NavAgents.Behaviour

  alias Exhub.Hercules.NavAgents.ToolLoop
  alias Exhub.Hercules.Tools.Browser, as: BTools

  @impl true
  def system_prompt do
    """
    # Web Navigation Agent
    You are a smart web navigation agent. Execute precise webpage interactions.
    ...(adapted from Hercules BrowserNavAgent prompt)...
    """
  end

  @impl true
  def tools do
    [
      BTools.navigate(),
      BTools.click(),
      BTools.fill(),
      BTools.select_option(),
      BTools.press_key(),
      BTools.snapshot(),
      BTools.screenshot(),
      BTools.get_page_text(),
      BTools.hover(),
      BTools.upload_file()
    ]
  end

  @impl true
  def execute(task, opts) do
    llm = build_nav_llm(opts)
    ToolLoop.run(system_prompt(), task, tools(), llm, opts[:max_rounds] || 50)
  end
end
```

### 7.4 Browser Tools (wrapping kuri-agent)

Each tool wraps an existing `Exhub.MCP.Tools.BrowserUse.*` function:

```elixir
defmodule Exhub.Hercules.Tools.Browser do
  @moduledoc "LangChain.Function definitions wrapping kuri-agent CDP tools."

  alias LangChain.Function
  alias Exhub.MCP.Tools.BrowserUse.{Navigate, Interact, Inspect}

  def navigate do
    Function.new!(%{
      name: "open_url",
      description: "Navigate browser to a URL. Returns page title and final URL.",
      parameters_schema: %{
        type: "object",
        properties: %{
          url: %{type: "string", description: "URL to navigate to (include protocol)"}
        },
        required: ["url"]
      },
      module: __MODULE__,
      function: :do_navigate
    })
  end

  def do_navigate(%{"url" => url}) do
    # Delegates to existing kuri-agent navigate tool
    case Navigate.handle(%{"url" => url}, %{}) do
      {:ok, result} -> result
      {:error, reason} -> "[ERROR] Navigation failed: #{inspect(reason)}"
    end
  end

  def click do
    Function.new!(%{
      name: "click",
      description: "Click an element by CSS selector, text content, or accessibility ref.",
      parameters_schema: %{
        type: "object",
        properties: %{
          selector: %{type: "string", description: "CSS selector or text to click"},
          ref: %{type: "string", description: "Accessibility tree ref (from snapshot)"}
        }
      },
      module: __MODULE__,
      function: :do_click
    })
  end

  # ... similar for fill, select, press_key, snapshot, screenshot, get_text, hover, upload
end
```

---

## 8. Runner (Lifecycle Management)

```elixir
defmodule Exhub.Hercules.Runner do
  @moduledoc """
  GenServer managing test run lifecycle.
  Supports sequential and parallel execution.
  """

  use GenServer

  # --- Client API ---

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc "Run a single feature/scenario. Blocks until complete."
  def run(feature_input, opts \\ []) do
    GenServer.call(__MODULE__, {:run, feature_input, opts}, :infinity)
  end

  @doc "Run asynchronously. Returns {:ok, task_ref}."
  def run_async(feature_input, opts \\ []) do
    GenServer.call(__MODULE__, {:run_async, feature_input, opts})
  end

  @doc "Get results from the last completed run."
  def last_results do
    GenServer.call(__MODULE__, :last_results)
  end

  @doc "List all historical runs."
  def list_runs do
    GenServer.call(__MODULE__, :list_runs)
  end

  # --- Server Callbacks ---

  @impl true
  def init(_opts) do
    {:ok, %{runs: [], active_tasks: %{}}}
  end

  @impl true
  def handle_call({:run, input, opts}, _from, state) do
    result = do_run(input, opts)
    {:reply, result, %{state | runs: [result | state.runs]}}
  end

  @impl true
  def handle_call({:run_async, input, opts}, from, state) do
    task = Task.async(fn -> do_run(input, opts) end)
    ref = task.ref
    {:reply, {:ok, ref}, %{state | active_tasks: Map.put(state.active_tasks, ref, {task, from})}}
  end

  @impl true
  def handle_info({ref, result}, state) when is_reference(ref) do
    case Map.pop(state.active_tasks, ref) do
      {{_task, from}, rest} ->
        GenServer.reply(from, result)
        {:noreply, %{state | active_tasks: rest, runs: [result | state.runs]}}
      {nil, _} ->
        {:noreply, %{state | runs: [result | state.runs]}}
    end
  end

  defp do_run(input, opts) do
    run_id = generate_run_id()
    proof_path = setup_proof_dir(run_id)

    # Parse input (Gherkin or plain English)
    scenarios = Exhub.Hercules.Gherkin.Parser.parse(input)

    results = Enum.map(scenarios, fn scenario ->
      task = scenario_to_task(scenario, opts)
      Exhub.Hercules.Orchestrator.run(task,
        Keyword.merge(opts, run_id: run_id, proof_path: proof_path)
      )
    end)

    # Generate reports
    junit_path = Exhub.Hercules.Reporting.JunitXml.generate(results, proof_path)
    Exhub.Hercules.Reporting.ThoughtLogger.flush(run_id, proof_path)

    %Exhub.Hercules.RunResult{
      run_id: run_id,
      status: if(Enum.all?(results, & &1.is_passed), do: :passed, else: :failed),
      junit_xml_path: junit_path,
      proof_path: proof_path
    }
  end
end
```

---

## 9. MCP Server

```elixir
defmodule Exhub.Hercules.MCPServer do
  @moduledoc "Anubis MCP server exposing Hercules test automation."

  use Anubis.Server,
    name: "hercules",
    description: "AI-powered E2E test execution agent"

  alias Exhub.Hercules.Runner
  alias Exhub.MCP.Desktop.Helpers, as: H

  tool "run_test" do
    description """
    Execute an end-to-end test from a Gherkin feature or plain English description.
    Returns test results including pass/fail status, assertions, and proof artifacts.
    """
    param :feature, :string, required: true,
      description: "Gherkin feature content or plain English test description"
    param :config, :map, default: %{},
      description: "Optional config: max_rounds, nav_max_rounds, model overrides"

    handler fn params ->
      result = Runner.run(params["feature"], Map.get(params, "config", %{}))
      H.toon_response(resp, Map.from_struct(result))
    end
  end

  tool "generate_gherkin" do
    description "Convert a plain-English test description into Gherkin format."
    param :description, :string, required: true

    handler fn params ->
      gherkin = Exhub.Hercules.Gherkin.Generator.generate(params["description"])
      H.toon_response(resp, %{gherkin: gherkin})
    end
  end

  tool "get_test_results" do
    description "Get results from the most recent test run."

    handler fn _params ->
      results = Runner.last_results()
      H.toon_response(resp, results)
    end
  end

  tool "list_test_runs" do
    description "List all historical test runs with status."

    handler fn _params ->
      runs = Runner.list_runs()
      H.toon_response(resp, %{runs: runs})
    end
  end
end
```

---

## 10. Gherkin Parser

```elixir
defmodule Exhub.Hercules.Gherkin.Parser do
  @moduledoc """
  Minimal Gherkin parser. Extracts Feature → Scenarios → Steps.
  Supports: Feature, Scenario, Scenario Outline, Given/When/Then/And/But.
  """

  defmodule Scenario do
    defstruct [:name, :steps, :tags, :examples]
  end

  defmodule Step do
    defstruct [:keyword, :text]  # keyword: :given | :when | :then | :and | :but
  end

  @doc """
  Parse Gherkin text into a list of Scenario structs.
  If input doesn't look like Gherkin, wraps it as a single scenario.
  """
  def parse(input) do
    if gherkin?(input) do
      parse_gherkin(input)
    else
      # Plain English → single scenario
      [%Scenario{name: "Plain English Test", steps: [%Step{keyword: :given, text: input}]}]
    end
  end

  defp gherkin?(input) do
    String.contains?(input, "Feature:") or String.contains?(input, "Scenario:")
  end

  defp parse_gherkin(input) do
    input
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == "" or String.starts_with?(&1, "#")))
    |> do_parse([], nil, [])
  end

  # ... recursive descent parser implementation
end
```

---

## 11. Reporting

### 11.1 JUnit XML

```elixir
defmodule Exhub.Hercules.Reporting.JunitXml do
  @moduledoc "Generate JUnit XML from test run results."

  def generate(results, output_dir) do
    path = Path.join(output_dir, "results.xml")

    xml = """
    <?xml version="1.0" encoding="UTF-8"?>
    <testsuites tests="#{length(results)}" failures="#{count_failures(results)}">
      <testsuite name="ExHub Hercules" tests="#{length(results)}">
        #{Enum.map_join(results, "\n", &testcase_xml/1)}
      </testsuite>
    </testsuites>
    """

    File.write!(path, xml)
    path
  end

  defp testcase_xml(result) do
    status = if result.is_passed, do: "", else: failure_xml(result)
    """
    <testcase name="#{escape(result.scenario_name)}" time="#{result.duration_ms / 1000}">
      #{status}
    </testcase>
    """
  end

  defp failure_xml(result) do
    """
    <failure message="#{escape(result.assert_summary)}">
      #{escape(result.final_response)}
    </failure>
    """
  end
end
```

### 11.2 Proof Logger

```elixir
defmodule Exhub.Hercules.Reporting.ProofLogger do
  @moduledoc "Captures screenshots and interaction logs per run."

  def screenshot(run_id, proof_path, label) do
    # Delegates to kuri-agent screenshot tool
    filename = "#{label}_#{System.system_time(:millisecond)}.png"
    path = Path.join(proof_path, filename)
    Exhub.MCP.Tools.BrowserUse.Inspect.screenshot(%{"path" => path})
    path
  end

  def log_interaction(run_id, proof_path, tool_name, args, result) do
    log_path = Path.join(proof_path, "interactions.jsonl")
    entry = Jason.encode!(%{
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601(),
      tool: tool_name,
      args: args,
      result: String.slice(to_string(result), 0, 500)
    })
    File.write!(log_path, entry <> "\n", [:append])
  end
end
```

---

## 12. Supervision Tree Integration

Add to `Exhub.Application`:

```elixir
# In children list, after existing MCP servers:
{Exhub.Hercules.Runner, name: Exhub.Hercules.Runner},
{Exhub.Hercules.MCPServer,
  transport: :streamable_http,
  request_timeout: 600_000,
  session_idle_timeout: 86_400_000 * 365},
```

---

## 13. Configuration

```elixir
# config/runtime.exs
config :exhub, Exhub.Hercules,
  # LLM model names (resolved via LlmConfigServer)
  planner_model: "openai/gpt-4o",
  nav_model: "openai/gpt-4o",
  # Limits
  max_planner_rounds: 500,
  max_nav_rounds: 50,
  # Paths
  proof_base_dir: "/tmp/hercules_proofs",
  test_data_dir: "priv/hercules/test_data",
  # Timeouts
  llm_timeout_ms: 60_000,
  tool_timeout_ms: 30_000
```

---

## 14. Error Handling & Resilience

| Failure Mode | Handling |
|---|---|
| LLM timeout | Retry once with exponential backoff; then terminate run with error report |
| LLM context overflow | Compress messages (summarize old turns), retry |
| Tool execution error | Return `[ERROR] ...` string to nav agent; agent decides retry/skip |
| Nav agent max rounds | Return explicit failure message to planner |
| Planner max rounds | Terminate run, mark as failed |
| Browser crash (kuri-agent) | KuriDaemon auto-restarts; nav agent retries navigation |
| Orchestrator process crash | Runner catches Task exit, marks run as `:error` |
| Invalid planner JSON | Fallback: attempt regex extraction; if fails, terminate with error |

---

## 15. Concurrency Model

```
Runner (GenServer)
  │
  ├── Task (run_1) ─── Orchestrator ─── Planner LLM calls
  │                                  └── Browser NavAgent ─── kuri-agent CDP
  │
  ├── Task (run_2) ─── Orchestrator ─── ...
  │
  └── Task (run_N) ─── ...
```

- Each `run` is an isolated `Task` with its own state
- Browser sessions are isolated via kuri-agent tab management (one tab per run)
- LLM calls are async HTTP (no shared state)
- Runner GenServer only tracks metadata (run list, active tasks)

---

## 16. Testing Strategy

| Layer | Approach |
|---|---|
| **Unit** | Gherkin parser, planner JSON parsing, JUnit XML generation |
| **Integration** | Nav agent tool loop with mocked LLM responses |
| **E2E** | Run a simple scenario against a local test page (e.g., `the-internet.herokuapp.com`) |
| **MCP** | Call `run_test` via MCP client, verify JUnit output |

---

## 17. Future Extensions (Phase 2+)

- **Visual validation**: Screenshot comparison via `Exhub.MCP.LookServer` (vision LLM)
- **SQL agent**: Database assertions via `Exhub.MCP.ArcheryServer`
- **MCP agent**: Call arbitrary upstream MCP tools during tests
- **Distributed runs**: `Task.Supervisor` across nodes via `:pg` process groups
- **CI integration**: GitHub Action that calls the MCP server
- **Test data management**: Dynamic data generation, CSV/Excel data-driven tests
- **Sagents integration**: Register as a Sagents agent for AiderDesk delegation

---

## 18. Decision Log

| # | Decision | Rationale |
|---|---|---|
| D1 | Task-based orchestrator (not GenServer) | Process isolation, simple state threading, natural crash boundary |
| D2 | Reuse kuri-agent (not Playwright) | Already in ExHub, Zig binary is fast, no Python dependency |
| D3 | LangChain.ex for LLM calls | Already integrated, supports tool-calling, multi-provider |
| D4 | Anubis for MCP server | Consistent with all other ExHub MCP servers |
| D5 | File-based proof output (not DB) | Simpler for v1; JUnit XML is the standard interface |
| D6 | Separate planner/nav model configs | Allows cheap model for nav, expensive for planning |
| D7 | `##TERMINATE TASK##` sentinel (not JSON) | Preserves Hercules convention; nav agents are free-form text |
| D8 | No LangGraph equivalent needed | The state machine is a simple loop; Elixir recursion is clearer |
