# Agent Hub Platform Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build an Agent Hub platform inside exhub powered by sagents, with HTTP chat API and MCP server endpoints.

**Architecture:** `Exhub.Sagents.Factory` creates agents using exhub's LLM config, `Exhub.Sagents.Hub` manages lifecycle, REST API at `/agent-hub/*` for chat, MCP server at `/agent-hub/mcp` for tool access, per-agent virtual routes at `/agent-hub/{name}/mcp`.

**Tech Stack:** Elixir, sagents (~> 0.7.0), LangChain, Anubis.Server, Plug.Router, SSE streaming

---

### Task 1: Add sagents dependency

**Files:**
- Modify: `mix.exs`

**Step 1: Add sagents to deps**

In `mix.exs`, add to the `deps` list:

```elixir
{:sagents, "~> 0.7.0"},
```

**Step 2: Fetch dependencies**

Run: `mix deps.get`
Expected: sagents and its dependencies (langchain, phoenix_pubsub, ecto, jason) downloaded

**Step 3: Verify compilation**

Run: `mix compile`
Expected: No errors. sagents compiles alongside exhub.

**Step 4: Commit**

```bash
git add mix.exs mix.lock
git commit -m "deps: add sagents ~> 0.7.0 for Agent Hub platform"
```

---

### Task 2: Add Sagents.Supervisor to application supervision tree

**Files:**
- Modify: `lib/exhub/application.ex`

**Step 1: Add Sagents.Supervisor to children**

In `lib/exhub/application.ex`, add `Sagents.Supervisor` to the children list **after** the Registry and **before** the MCP servers. Insert after the `{Registry, keys: :unique, name: Exhub.Registry}` line:

```elixir
# Sagents — agent orchestration framework (must start after Registry)
Sagents.Supervisor,
```

**Step 2: Verify compilation**

Run: `mix compile`
Expected: No errors.

**Step 3: Commit**

```bash
git add lib/exhub/application.ex
git commit -m "feat: add Sagents.Supervisor to application supervision tree"
```

---

### Task 3: Create Exhub.Sagents.McpAdapter

**Files:**
- Create: `lib/exhub/sagents/mcp_adapter.ex`

**Step 1: Write the test**

Create `test/exhub/sagents/mcp_adapter_test.exs`:

```elixir
defmodule Exhub.Sagents.McpAdapterTest do
  use ExUnit.Case, async: true

  alias Exhub.Sagents.McpAdapter

  describe "build_tools/1" do
    test "returns empty list for empty input" do
      assert McpAdapter.build_tools([]) == []
    end

    test "returns empty list for unknown tool group" do
      assert McpAdapter.build_tools([:nonexistent]) == []
    end

    test "builds LangChain.Function tools for known groups" do
      tools = McpAdapter.build_tools([:todo])
      assert is_list(tools)
      # Each tool should be a LangChain.Function
      Enum.each(tools, fn tool ->
        assert %LangChain.Function{} = tool
      end)
    end
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/sagents/mcp_adapter_test.exs`
Expected: FAIL — `Exhub.Sagents.McpAdapter` module not defined

**Step 3: Implement McpAdapter**

Create `lib/exhub/sagents/mcp_adapter.ex`:

```elixir
defmodule Exhub.Sagents.McpAdapter do
  @moduledoc """
  Bridges exhub MCP tools into LangChain.Function structs for sagents agents.

  Converts selected MCP tool groups (e.g., [:desktop, :web-tools]) into
  `LangChain.Function` structs that agents can call during execution.
  """

  alias Exhub.MCP.Hub.BuiltInRegistry
  require Logger

  @doc """
  Builds LangChain.Function tools from a list of MCP tool group atoms.

  ## Parameters
    - `tool_groups` — list of atoms like `[:desktop, :web-tools, :brain]`

  ## Returns
    - List of `LangChain.Function` structs
  """
  @spec build_tools([atom()]) :: [LangChain.Function.t()]
  def build_tools(tool_groups) when is_list(tool_groups) do
    tool_groups
    |> Enum.flat_map(&build_tools_for_group/1)
  end

  defp build_tools_for_group(group) do
    server_name = Atom.to_string(group)

    case BuiltInRegistry.list_tools(server_name) do
      [] ->
        Logger.warning("[McpAdapter] No tools found for group: #{group}")
        []

      tools ->
        Enum.map(tools, &build_langchain_function(&1, server_name))
    end
  end

  defp build_langchain_function(tool_map, server_name) do
    tool_name = Map.get(tool_map, "name")
    description = Map.get(tool_map, "description", "")
    input_schema = Map.get(tool_map, "inputSchema", %{})
    namespaced_name = "#{server_name}__#{tool_name}"

    # Extract parameters from JSON Schema
    parameters = extract_parameters(input_schema)

    LangChain.Function.new!(%{
      name: namespaced_name,
      description: "[#{server_name}] #{description}",
      parameters: parameters,
      function: fn args, _context ->
        # Strip namespace from tool name for the actual call
        case BuiltInRegistry.call_tool(server_name, tool_name, args) do
          {:ok, result} ->
            case Jason.encode(result) do
              {:ok, json} -> json
              _ -> inspect(result)
            end

          {:error, reason} ->
            Logger.error("[McpAdapter] Tool call failed: #{namespaced_name}: #{inspect(reason)}")
            "Error: #{inspect(reason)}"
        end
      end
    })
  end

  defp extract_parameters(%{"properties" => properties} = schema) do
    required = Map.get(schema, "required", [])

    properties
    |> Enum.map(fn {name, prop} ->
      %{
        name: name,
        type: map_json_type(prop),
        description: Map.get(prop, "description", ""),
        required: name in required
      }
    end)
  end

  defp extract_parameters(_), do: []

  defp map_json_type(%{"type" => "string"}), do: :string
  defp map_json_type(%{"type" => "integer"}), do: :integer
  defp map_json_type(%{"type" => "number"}), do: :number
  defp map_json_type(%{"type" => "boolean"}), do: :boolean
  defp map_json_type(%{"type" => "array"}), do: :array
  defp map_json_type(%{"type" => "object"}), do: :object
  defp map_json_type(_), do: :string
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/sagents/mcp_adapter_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/sagents/mcp_adapter.ex test/exhub/sagents/mcp_adapter_test.exs
git commit -m "feat: add McpAdapter to bridge exhub MCP tools to LangChain.Function"
```

---

### Task 4: Create Exhub.Sagents.Factory

**Files:**
- Create: `lib/exhub/sagents/factory.ex`

**Step 1: Write the test**

Create `test/exhub/sagents/factory_test.exs`:

```elixir
defmodule Exhub.Sagents.FactoryTest do
  use ExUnit.Case, async: true

  alias Exhub.Sagents.Factory

  describe "agents/0" do
    test "returns a map of agent definitions" do
      agents = Factory.agents()
      assert is_map(agents)
      assert map_size(agents) > 0
    end

    test "each agent has required fields" do
      agents = Factory.agents()

      Enum.each(agents, fn {name, config} ->
        assert is_binary(name), "Agent name should be a string"
        assert is_binary(config.system_prompt), "Agent #{name} missing system_prompt"
        assert is_list(config.mcp_tools), "Agent #{name} missing mcp_tools list"
      end)
    end
  end

  describe "create_agent/2" do
    test "creates a Sagents.Agent from agent_id and config" do
      config = %{
        system_prompt: "You are a test agent.",
        mcp_tools: [:todo],
        middleware: []
      }

      {:ok, agent, session_opts} = Factory.create_agent("test-agent", config)
      assert %Sagents.Agent{} = agent
      assert agent.agent_id == "test-agent"
      assert is_list(session_opts)
    end
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/sagents/factory_test.exs`
Expected: FAIL — `Exhub.Sagents.Factory` module not defined

**Step 3: Implement Factory**

Create `lib/exhub/sagents/factory.ex`:

```elixir
defmodule Exhub.Sagents.Factory do
  @moduledoc """
  Factory for creating sagents agents using exhub's LLM configuration.

  Implements the `Sagents.Factory` behaviour. Agents are defined as maps
  with `system_prompt`, `mcp_tools`, and optional `middleware`.

  Uses `Exhub.Llm.LlmConfigServer` for the default LLM model.
  """

  @behaviour Sagents.Factory

  alias Exhub.Llm.LlmConfigServer
  alias Exhub.Sagents.McpAdapter
  require Logger

  @impl true
  def create_agent(agent_id, config) do
    with {:ok, model} <- build_langchain_model(config) do
      mcp_tools = McpAdapter.build_tools(config[:mcp_tools] || [])
      middleware = config[:middleware] || default_middleware()

      agent =
        Sagents.Agent.new!(%{
          agent_id: agent_id,
          model: model,
          base_system_prompt: config[:system_prompt] || "You are a helpful assistant.",
          middleware: middleware,
          tools: mcp_tools
        })

      session_opts = config[:session_opts] || []
      {:ok, agent, session_opts}
    else
      {:error, reason} ->
        Logger.error("[Sagents.Factory] Failed to create agent #{agent_id}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Returns the map of all registered agent definitions.

  Each key is the agent name (string), each value is a config map with:
  - `:system_prompt` — the agent's base system prompt
  - `:mcp_tools` — list of MCP tool group atoms to inject
  - `:middleware` — optional list of sagents middleware (defaults to standard stack)
  - `:persistence` — optional `:file` to enable file-based state persistence
  """
  def agents do
    %{
      "coder" => %{
        system_prompt: """
        You are an expert coding assistant. You can read, write, and edit files,
        execute commands, and search code. Always explain what you're doing before
        taking action. Use the available tools to help the user with their coding tasks.
        """,
        mcp_tools: [:desktop, :web-tools],
        middleware:
          default_middleware() ++
            [
              {Sagents.Middleware.HumanInTheLoop,
               [interrupt_on: %{"execute_command" => true, "write_file" => true}]}
            ]
      },
      "researcher" => %{
        system_prompt: """
        You are a research assistant. You can search the web, read documents,
        and access the knowledge base. Provide thorough, well-sourced answers.
        """,
        mcp_tools: [:web-tools, :brain, :look]
      },
      "assistant" => %{
        system_prompt: """
        You are a general-purpose assistant. You can help with a wide range of
        tasks including file management, web search, and information retrieval.
        """,
        mcp_tools: [:desktop, :web-tools, :todo]
      }
    }
  end

  defp build_langchain_model(config) do
    # If config specifies a model, use it; otherwise use exhub's default
    case Map.get(config, :model) do
      nil ->
        case LlmConfigServer.get_default_llm_config() do
          {:ok, llm_config} ->
            {:ok, create_langchain_model(llm_config)}

          {:error, reason} ->
            {:error, reason}
        end

      model_name when is_binary(model_name) ->
        case LlmConfigServer.get_llm_config(model_name) do
          {:ok, llm_config} ->
            {:ok, create_langchain_model(llm_config)}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp create_langchain_model(config) do
    [provider, model_name] = String.split(config[:model], "/", parts: 2)

    base_config = %{
      model: model_name,
      api_key: config[:api_key]
    }

    llm_config =
      case provider do
        "google" ->
          Map.put(base_config, :endpoint, config[:api_base])

        "anthropic" ->
          Map.put(base_config, :endpoint, "#{config[:api_base]}/messages")

        _ ->
          Map.put(base_config, :endpoint, "#{config[:api_base]}/chat/completions")
      end

    case provider do
      "google" -> LangChain.ChatModels.ChatGoogleAI.new!(llm_config)
      "anthropic" -> LangChain.ChatModels.ChatAnthropic.new!(llm_config)
      _ -> LangChain.ChatModels.ChatOpenAI.new!(llm_config)
    end
  end

  defp default_middleware do
    [
      {Sagents.Middleware.TodoList, []},
      {Sagents.Middleware.FileSystem, []},
      {Sagents.Middleware.Summarization, []},
      {Sagents.Middleware.PatchToolCalls, []}
    ]
  end
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/sagents/factory_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/sagents/factory.ex test/exhub/sagents/factory_test.exs
git commit -m "feat: add Sagents.Factory for creating agents with exhub LLM config"
```

---

### Task 5: Create Exhub.Sagents.Hub GenServer

**Files:**
- Create: `lib/exhub/sagents/hub.ex`

**Step 1: Write the test**

Create `test/exhub/sagents/hub_test.exs`:

```elixir
defmodule Exhub.Sagents.HubTest do
  use ExUnit.Case, async: false

  alias Exhub.Sagents.Hub

  setup do
    # Hub is started by the application, just ensure it's running
    :ok
  end

  describe "list_agents/0" do
    test "returns all registered agent definitions" do
      agents = Hub.list_agents()
      assert is_list(agents)
      assert length(agents) > 0

      Enum.each(agents, fn agent ->
        assert Map.has_key?(agent, :name)
        assert Map.has_key?(agent, :running)
      end)
    end
  end

  describe "start_agent/1 and stop_agent/1" do
    test "starts and stops an agent" do
      # Start the assistant agent (simplest, no HITL)
      assert {:ok, _pid} = Hub.start_agent("assistant")
      assert Hub.agent_running?("assistant") == true

      assert :ok = Hub.stop_agent("assistant")
      assert Hub.agent_running?("assistant") == false
    end
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/sagents/hub_test.exs`
Expected: FAIL — `Exhub.Sagents.Hub` module not defined

**Step 3: Implement Hub**

Create `lib/exhub/sagents/hub.ex`:

```elixir
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
  Returns {:ok, response_text} or {:error, reason}.
  """
  def chat(name, message) do
    GenServer.call(__MODULE__, {:chat, name, message}, :infinity)
  end

  @doc """
  Send a chat message and return an enumerable stream of events.

  Each event is a map with `:type` key (:delta, :tool_call, :tool_result, :complete, :error).
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
        %{
          name: name,
          running: Map.has_key?(state.running, name)
        }
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
    with {:ok, pid, state} <- ensure_running(name, state) do
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

            opts =
              [
                agent: agent,
                initial_state: initial_state,
                inactivity_timeout: Keyword.get(session_opts, :inactivity_timeout, 3_600_000)
              ]
              |> maybe_add_persistence(config)

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
      pid when is_pid(pid) and pid == node() ->
        {:ok, pid, state}

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
        # Wait for completion by receiving events
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
        # Other events (deltas, tool calls, etc.) — keep waiting
        remaining = deadline - System.monotonic_time(:millisecond)

        if remaining > 0 do
          wait_for_completion(name, remaining)
        else
          {:error, :timeout}
        end
    after
      timeout ->
        {:error, :timeout}
    end
  end

  defp extract_last_response(name) do
    state = Sagents.AgentServer.get_state(name)

    case List.last(state.messages) do
      %LangChain.Message{role: :assistant, content: content} when is_binary(content) ->
        {:ok, content}

      %LangChain.Message{role: :assistant, content: content} when is_list(content) ->
        text =
          content
          |> Enum.filter(&is_binary/1)
          |> Enum.join("")

        {:ok, text}

      _ ->
        {:ok, ""}
    end
  end

  defp build_event_stream(name, message) do
    Stream.resource(
      fn ->
        # Start the chat in a task
        task =
          Task.async(fn ->
            user_message = LangChain.Message.new_user!(message)
            Sagents.AgentServer.add_message(name, user_message)
            collect_events(name, 120_000)
          end)

        {task, false}
      end
      ,
      fn
        {_task, true} ->
          {:halt, nil}

        {task, false} ->
          case Task.yield(task, :infinity) do
            {:ok, events} ->
              {events, {task, true}}

            {:exit, reason} ->
              {[%{type: :error, error: inspect(reason)}], {task, true}}
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
            Enum.map(deltas, fn delta ->
              %{type: :delta, text: extract_delta_text(delta)}
            end)

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

  defp maybe_add_persistence(opts, %{persistence: :file} = config) do
    name = Keyword.get(opts, :agent) |> Map.get(:agent_id)
    persistence_module = Exhub.Sagents.Persistence
    Keyword.put(opts, :agent_persistence, persistence_module)
  end

  defp maybe_add_persistence(opts, _config), do: opts
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/sagents/hub_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/sagents/hub.ex test/exhub/sagents/hub_test.exs
git commit -m "feat: add Sagents.Hub GenServer for agent lifecycle management"
```

---

### Task 6: Add Hub to supervision tree

**Files:**
- Modify: `lib/exhub/application.ex`

**Step 1: Add Hub to children**

In `lib/exhub/application.ex`, add after `Sagents.Supervisor`:

```elixir
# Agent Hub — manages sagents agent lifecycle
{Exhub.Sagents.Hub, name: Exhub.Sagents.Hub},
```

**Step 2: Verify compilation**

Run: `mix compile`
Expected: No errors.

**Step 3: Commit**

```bash
git add lib/exhub/application.ex
git commit -m "feat: add Sagents.Hub to application supervision tree"
```

---

### Task 7: Create MCP tool components for Agent Hub

**Files:**
- Create: `lib/exhub/sagents/tools/list_agents.ex`
- Create: `lib/exhub/sagents/tools/start_agent.ex`
- Create: `lib/exhub/sagents/tools/chat.ex`
- Create: `lib/exhub/sagents/tools/status.ex`
- Create: `lib/exhub/sagents/tools/reset.ex`
- Create: `lib/exhub/sagents/tools/stop.ex`

**Step 1: Create ListAgents tool**

Create `lib/exhub/sagents/tools/list_agents.ex`:

```elixir
defmodule Exhub.Sagents.Tools.ListAgents do
  @moduledoc "MCP Tool: agent_hub_list — List all registered agents."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_list"

  @impl true
  def description do
    "List all registered agents in the Agent Hub with their running status."
  end

  schema do
    # No parameters needed
  end

  @impl true
  def execute(_params, frame) do
    agents = Hub.list_agents()

    resp =
      Response.tool()
      |> Response.text(Jason.encode!(agents))

    {:reply, resp, frame}
  end
end
```

**Step 2: Create StartAgent tool**

Create `lib/exhub/sagents/tools/start_agent.ex`:

```elixir
defmodule Exhub.Sagents.Tools.StartAgent do
  @moduledoc "MCP Tool: agent_hub_start — Start an agent."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_start"

  @impl true
  def description do
    available = Hub.list_agents() |> Enum.map(& &1.name) |> Enum.join(", ")
    "Start an agent by name.\nAvailable agents: #{available}"
  end

  schema do
    field(:name, {:required, :string}, description: "Agent name to start")
  end

  @impl true
  def execute(%{name: name}, frame) do
    case Hub.start_agent(name) do
      {:ok, _pid} ->
        resp = Response.tool() |> Response.text(Jason.encode!(%{status: "started", name: name}))
        {:reply, resp, frame}

      {:error, :not_found} ->
        resp = Response.tool() |> Response.error("Agent '#{name}' not found.")
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to start agent: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
```

**Step 3: Create Chat tool**

Create `lib/exhub/sagents/tools/chat.ex`:

```elixir
defmodule Exhub.Sagents.Tools.Chat do
  @moduledoc "MCP Tool: agent_hub_chat — Send a message to an agent and get a response."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_chat"

  @impl true
  def description do
    "Send a chat message to a specific agent. The agent is auto-started if not running. Returns the agent's response."
  end

  schema do
    field(:agent, {:required, :string}, description: "Agent name")
    field(:message, {:required, :string}, description: "Message to send")
  end

  @impl true
  def execute(%{agent: agent, message: message}, frame) do
    case Hub.chat(agent, message) do
      {:ok, response} ->
        resp = Response.tool() |> Response.text(response)
        {:reply, resp, frame}

      {:error, :not_found} ->
        resp = Response.tool() |> Response.error("Agent '#{agent}' not found.")
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Chat failed: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
```

**Step 4: Create Status tool**

Create `lib/exhub/sagents/tools/status.ex`:

```elixir
defmodule Exhub.Sagents.Tools.Status do
  @moduledoc "MCP Tool: agent_hub_status — Get agent status."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_status"

  @impl true
  def description do
    "Get the current status of a specific agent."
  end

  schema do
    field(:name, {:required, :string}, description: "Agent name")
  end

  @impl true
  def execute(%{name: name}, frame) do
    status = Hub.get_status(name)
    resp = Response.tool() |> Response.text(Jason.encode!(status))
    {:reply, resp, frame}
  end
end
```

**Step 5: Create Reset tool**

Create `lib/exhub/sagents/tools/reset.ex`:

```elixir
defmodule Exhub.Sagents.Tools.Reset do
  @moduledoc "MCP Tool: agent_hub_reset — Reset agent conversation state."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_reset"

  @impl true
  def description do
    "Reset an agent's conversation state (messages, todos, metadata)."
  end

  schema do
    field(:name, {:required, :string}, description: "Agent name")
  end

  @impl true
  def execute(%{name: name}, frame) do
    Hub.reset(name)
    resp = Response.tool() |> Response.text(Jason.encode!(%{status: "reset", name: name}))
    {:reply, resp, frame}
  end
end
```

**Step 6: Create Stop tool**

Create `lib/exhub/sagents/tools/stop.ex`:

```elixir
defmodule Exhub.Sagents.Tools.Stop do
  @moduledoc "MCP Tool: agent_hub_stop — Stop a running agent."

  use Anubis.Server.Component, type: :tool
  alias Anubis.Server.Response
  alias Exhub.Sagents.Hub

  def name, do: "agent_hub_stop"

  @impl true
  def description do
    "Stop a running agent and free its resources."
  end

  schema do
    field(:name, {:required, :string}, description: "Agent name")
  end

  @impl true
  def execute(%{name: name}, frame) do
    Hub.stop_agent(name)
    resp = Response.tool() |> Response.text(Jason.encode!(%{status: "stopped", name: name}))
    {:reply, resp, frame}
  end
end
```

**Step 7: Verify compilation**

Run: `mix compile`
Expected: No errors.

**Step 8: Commit**

```bash
git add lib/exhub/sagents/tools/
git commit -m "feat: add MCP tool components for Agent Hub"
```

---

### Task 8: Create AgentHubServer (MCP server)

**Files:**
- Create: `lib/exhub/sagents/agent_hub_server.ex`

**Step 1: Create the MCP server**

Create `lib/exhub/sagents/agent_hub_server.ex`:

```elixir
defmodule Exhub.Sagents.AgentHubServer do
  @moduledoc """
  MCP Server for the Agent Hub platform.

  Provides tools to list, start, chat with, and manage sagents agents.
  Accessible at `/agent-hub/mcp`.
  """

  use Anubis.Server,
    name: "exhub-agent-hub-server",
    version: "1.0.0",
    capabilities: [:tools]

  # Agent Hub tools
  component Exhub.Sagents.Tools.ListAgents
  component Exhub.Sagents.Tools.StartAgent
  component Exhub.Sagents.Tools.Chat
  component Exhub.Sagents.Tools.Status
  component Exhub.Sagents.Tools.Reset
  component Exhub.Sagents.Tools.Stop

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end

  @impl true
  def handle_request(request, frame) do
    Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
  end
end
```

**Step 2: Verify compilation**

Run: `mix compile`
Expected: No errors.

**Step 3: Commit**

```bash
git add lib/exhub/sagents/agent_hub_server.ex
git commit -m "feat: add AgentHubServer MCP server at /agent-hub/mcp"
```

---

### Task 9: Add REST routes and MCP endpoint to router

**Files:**
- Modify: `lib/exhub/router.ex`

**Step 1: Add MCP forward**

In `lib/exhub/router.ex`, add in the MCP Endpoint section (after the existing `forward` calls):

```elixir
# Agent Hub MCP server
forward("/agent-hub/mcp",
  to: Exhub.MCP.LazyPlug,
  init_opts: [server: Exhub.Sagents.AgentHubServer, request_timeout: 300_000]
)
```

**Step 2: Add REST API routes**

In `lib/exhub/router.ex`, add in the MCP Hub Management API section:

```elixir
# ============================================================================
# Agent Hub API
# ============================================================================

get "/agent-hub/agents" do
  agents = Exhub.Sagents.Hub.list_agents()

  conn
  |> put_resp_content_type("application/json")
  |> send_resp(200, Jason.encode!(%{agents: agents}))
end

post "/agent-hub/agents/:name/chat" do
  message = conn.body_params["message"]

  if is_nil(message) or message == "" do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(400, Jason.encode!(%{error: "message is required"}))
  else
    # SSE streaming response
    conn =
      conn
      |> put_resp_header("content-type", "text/event-stream")
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_header("connection", "keep-alive")
      |> send_chunked(200)

    case Exhub.Sagents.Hub.chat_stream(name, message) do
      {:ok, stream} ->
        Enum.reduce_while(stream, conn, fn event, conn ->
          case Jason.encode(event) do
            {:ok, json} ->
              case Plug.Conn.chunk(conn, "data: #{json}\n\n") do
                {:ok, conn} -> {:cont, conn}
                {:error, :closed} -> {:halt, conn}
              end

            _ ->
              {:cont, conn}
          end
        end)
        |> then(fn conn ->
          {:ok, conn} = Plug.Conn.chunk(conn, "data: [DONE]\n\n")
          conn
        end)

      {:error, reason} ->
        {:ok, conn} =
          Plug.Conn.chunk(conn, "data: #{Jason.encode!(%{type: :error, error: inspect(reason)})}\n\n")

        conn
    end
  end
end

get "/agent-hub/agents/:name/status" do
  status = Exhub.Sagents.Hub.get_status(name)

  conn
  |> put_resp_content_type("application/json")
  |> send_resp(200, Jason.encode!(status))
end

post "/agent-hub/agents/:name/reset" do
  Exhub.Sagents.Hub.reset(name)

  conn
  |> put_resp_content_type("application/json")
  |> send_resp(200, Jason.encode!(%{status: "reset", name: name}))
end

post "/agent-hub/agents/:name/stop" do
  Exhub.Sagents.Hub.stop_agent(name)

  conn
  |> put_resp_content_type("application/json")
  |> send_resp(200, Jason.encode!(%{status: "stopped", name: name}))
end
```

**Step 3: Add agent-hub to BuiltInRegistry**

In `lib/exhub/mcp/hub/built_in_registry.ex`, add to the `@built_in_servers` map:

```elixir
"agent-hub" => Exhub.Sagents.AgentHubServer,
```

**Step 4: Add agent-hub to Hub.Store builtin configs**

In `lib/exhub/mcp/hub/client_manager.ex`, add to the `builtin_server_configs/0` function:

```elixir
%{name: "agent-hub", route: "/agent-hub/mcp"},
```

**Step 5: Verify compilation**

Run: `mix compile`
Expected: No errors.

**Step 6: Commit**

```bash
git add lib/exhub/router.ex lib/exhub/mcp/hub/built_in_registry.ex lib/exhub/mcp/hub/client_manager.ex
git commit -m "feat: add Agent Hub REST API routes and MCP endpoint"
```

---

### Task 10: Add Agent Hub to MCP Hub's builtin_server_configs

**Files:**
- Modify: `lib/exhub/mcp/hub/client_manager.ex`

**Step 1: Add agent-hub entry**

In `lib/exhub/mcp/hub/client_manager.ex`, in the `builtin_server_configs/0` function, add to the list:

```elixir
%{name: "agent-hub", route: "/agent-hub/mcp"},
```

**Step 2: Verify compilation**

Run: `mix compile`
Expected: No errors.

**Step 3: Commit**

```bash
git add lib/exhub/mcp/hub/client_manager.ex
git commit -m "feat: register agent-hub as built-in MCP Hub server"
```

---

### Task 11: Create optional file persistence module

**Files:**
- Create: `lib/exhub/sagents/persistence.ex`

**Step 1: Implement persistence**

Create `lib/exhub/sagents/persistence.ex`:

```elixir
defmodule Exhub.Sagents.Persistence do
  @moduledoc """
  Optional file-based persistence for sagents agent state.

  Implements `Sagents.AgentPersistence` behaviour. Saves/loads agent state
  to `~/.config/exhub/agent_hub/{agent_id}/state.json`.
  """

  @behaviour Sagents.AgentPersistence

  require Logger

  @base_dir "~/.config/exhub/agent_hub"

  @impl true
  def save_state(_scope, state, context) do
    agent_id = Map.get(context, :agent_id)
    dir = agent_dir(agent_id)
    File.mkdir_p!(dir)

    path = Path.join(dir, "state.json")
    exported = Sagents.AgentServer.export_state(agent_id)

    case Jason.encode(exported, pretty: true) do
      {:ok, json} ->
        File.write!(path, json)
        Logger.debug("[Sagents.Persistence] Saved state for #{agent_id}")
        :ok

      {:error, reason} ->
        Logger.error("[Sagents.Persistence] Failed to encode state: #{inspect(reason)}")
        {:error, reason}
    end
  rescue
    error ->
      Logger.error("[Sagents.Persistence] Save failed: #{inspect(error)}")
      {:error, inspect(error)}
  end

  @impl true
  def load_state(_scope, context) do
    agent_id = Map.get(context, :agent_id)
    path = Path.join(agent_dir(agent_id), "state.json")

    case File.read(path) do
      {:ok, content} ->
        case Jason.decode(content) do
          {:ok, data} ->
            Logger.debug("[Sagents.Persistence] Loaded state for #{agent_id}")
            {:ok, data}

          {:error, reason} ->
            Logger.warning("[Sagents.Persistence] Failed to decode state: #{inspect(reason)}")
            {:error, :not_found}
        end

      {:error, :enoent} ->
        {:error, :not_found}

      {:error, reason} ->
        Logger.error("[Sagents.Persistence] Failed to read state: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp agent_dir(agent_id) do
    @base_dir
    |> Path.expand()
    |> Path.join(agent_id)
  end
end
```

**Step 2: Verify compilation**

Run: `mix compile`
Expected: No errors.

**Step 3: Commit**

```bash
git add lib/exhub/sagents/persistence.ex
git commit -m "feat: add optional file-based persistence for agent state"
```

---

### Task 12: Integration test — end-to-end agent chat

**Files:**
- Create: `test/exhub/sagents/integration_test.exs`

**Step 1: Write integration test**

Create `test/exhub/sagents/integration_test.exs`:

```elixir
defmodule Exhub.Sagents.IntegrationTest do
  use ExUnit.Case, async: false

  alias Exhub.Sagents.Hub

  @moduletag :integration

  describe "Agent Hub end-to-end" do
    test "list agents returns all registered agents" do
      agents = Hub.list_agents()
      assert is_list(agents)
      assert Enum.any?(agents, &(&1.name == "assistant"))
      assert Enum.any?(agents, &(&1.name == "coder"))
      assert Enum.any?(agents, &(&1.name == "researcher"))
    end

    test "start and stop agent lifecycle" do
      {:ok, _pid} = Hub.start_agent("assistant")
      assert Hub.agent_running?("assistant")

      :ok = Hub.stop_agent("assistant")
      refute Hub.agent_running?("assistant")
    end

    test "get_status returns status for non-running agent" do
      status = Hub.get_status("nonexistent")
      assert status.status == :not_running
    end
  end
end
```

**Step 2: Run test**

Run: `mix test test/exhub/sagents/integration_test.exs --include integration`
Expected: PASS

**Step 3: Commit**

```bash
git add test/exhub/sagents/integration_test.exs
git commit -m "test: add integration tests for Agent Hub"
```

---

### Task 13: Documentation

**Files:**
- Create: `docs/modules/agent-hub.md`

**Step 1: Write documentation**

Create `docs/modules/agent-hub.md`:

```markdown
# Agent Hub (exhub-agent-hub)

MCP-based agent orchestration platform powered by sagents. Manage multiple
LLM-backed agents with middleware, tools, and conversation state.

## Features

- **Agent Factory** — Define agents in code with custom system prompts, middleware, and MCP tool groups
- **Chat API** — HTTP REST with SSE streaming at `POST /agent-hub/agents/{name}/chat`
- **MCP Server** — Single hub endpoint at `/agent-hub/mcp` with tools for agent management
- **Selective MCP Tool Injection** — Each agent can use specific exhub MCP tool groups (desktop, web-tools, brain, etc.)
- **Optional Persistence** — File-based state persistence per agent

## API Endpoints

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/agent-hub/agents` | List all agents |
| `POST` | `/agent-hub/agents/{name}/chat` | Chat with agent (SSE stream) |
| `GET` | `/agent-hub/agents/{name}/status` | Agent status |
| `POST` | `/agent-hub/agents/{name}/reset` | Reset agent state |
| `POST` | `/agent-hub/agents/{name}/stop` | Stop agent |

## MCP Tools

| Tool | Description |
|------|-------------|
| `agent_hub_list` | List all registered agents |
| `agent_hub_start` | Start an agent |
| `agent_hub_chat` | Send message to agent |
| `agent_hub_status` | Get agent status |
| `agent_hub_reset` | Reset agent state |
| `agent_hub_stop` | Stop a running agent |

## Default Agents

- **coder** — Coding assistant with desktop + web-tools, HITL for dangerous operations
- **researcher** — Research assistant with web-tools + brain + look
- **assistant** — General-purpose with desktop + web-tools + todo

## Chat Example

```bash
curl -N http://localhost:9069/agent-hub/agents/assistant/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "What files are in the current directory?"}'
```

## MCP Example

```bash
# List agents
echo '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"agent_hub_list","arguments":{}}}' | \
  curl -s -X POST http://localhost:9069/agent-hub/mcp -H "Content-Type: application/json" -d @-
```

## Adding Custom Agents

Edit `lib/exhub/sagents/factory.ex` and add to the `agents/0` function:

```elixir
"my-agent" => %{
  system_prompt: "You are a specialized assistant...",
  mcp_tools: [:desktop, :brain],
  middleware: default_middleware(),
  persistence: :file  # optional
}
```

Then hot-reload: `curl -X POST http://localhost:9069/system/reload`
```

**Step 2: Commit**

```bash
git add docs/modules/agent-hub.md
git commit -m "docs: add Agent Hub documentation"
```

---

### Task 14: Final verification

**Step 1: Full compilation check**

Run: `mix compile --warnings-as-errors`
Expected: No warnings or errors.

**Step 2: Run all tests**

Run: `mix test`
Expected: All tests pass.

**Step 3: Manual smoke test**

Start the server:
```bash
mix run --no-halt
```

Test the API:
```bash
# List agents
curl http://localhost:9069/agent-hub/agents

# Start an agent
curl -X POST http://localhost:9069/agent-hub/agents/assistant/status

# Chat (SSE)
curl -N http://localhost:9069/agent-hub/agents/assistant/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "Hello, what can you do?"}'
```

**Step 4: Final commit**

```bash
git add -A
git commit -m "feat: Agent Hub platform complete — sagents integration with REST API and MCP"
```
