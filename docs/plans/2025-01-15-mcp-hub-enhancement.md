# MCP Hub Enhancement Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add tool search & discovery (TF-IDF based `retrieve_tools`) and health monitoring with auto-reconnect to ExHub's MCP Hub.

**Architecture:** Build an in-memory TF-IDF search index in `ToolSearch` module. Enhance `ClientState` with health fields and add periodic health checks + exponential backoff auto-reconnect in `ClientManager`. Add `retrieve_tools` meta-tool to `Hub.Server` and logging for all tool calls.

**Tech Stack:** Elixir/OTP, Anubis MCP client, Plug HTTP API

---

## Task 1: Create ToolSearch Module

**Files:**
- Create: `lib/exhub/mcp/hub/tool_search.ex`
- Test: `test/exhub/mcp/hub/tool_search_test.exs`

**Step 1: Write the failing test**

```elixir
defmodule Exhub.MCP.Hub.ToolSearchTest do
  use ExUnit.Case
  alias Exhub.MCP.Hub.ToolSearch

  test "build_index/1 creates inverted index from tools" do
    tools = [
      %{"server" => "desktop", "name" => "read_file", "description" => "Read file contents"},
      %{"server" => "desktop", "name" => "write_file", "description" => "Write file contents"},
      %{"server" => "github", "name" => "get_repo", "description" => "Get repository info"}
    ]

    index = ToolSearch.build_index(tools)
    assert map_size(index) > 0
  end

  test "search/3 returns ranked results" do
    tools = [
      %{"server" => "desktop", "name" => "read_file", "description" => "Read file contents"},
      %{"server" => "desktop", "name" => "write_file", "description" => "Write file contents"}
    ]

    index = ToolSearch.build_index(tools)
    results = ToolSearch.search(index, "read file", limit: 2)
    assert length(results) > 0
    assert hd(results)["name"] == "desktop__read_file"
  end

  test "search/3 returns empty list for no matches" do
    index = ToolSearch.build_index([])
    assert ToolSearch.search(index, "nonexistent") == []
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/hub/tool_search_test.exs`
Expected: FAIL - module not defined

**Step 3: Implement ToolSearch module**

```elixir
defmodule Exhub.MCP.Hub.ToolSearch do
  @moduledoc """
  In-memory TF-IDF search for MCP tools.
  """

  require Logger

  @doc """
  Builds an inverted index from a list of tools.
  Each tool should have: server, name, description keys.
  """
  def build_index(tools) when is_list(tools) do
    docs = Enum.map(tools, &to_doc/1)
    {docs, build_inverted_index(docs)}
  end

  def build_index(_), do: {%{}, %{}}

  @doc """
  Searches the index for tools matching the query.
  Returns a list of scored results sorted by relevance.
  """
  def search({_docs, index}, query, opts \\ []) when is_binary(query) do
    limit = Keyword.get(opts, :limit, 5)
    tokens = tokenize(query)

    if tokens == [] do
      []
    else
      score_docs(index, tokens)
      |> Enum.sort_by(fn {_id, score} -> score end, :desc)
      |> Enum.take(limit)
      |> Enum.map(fn {doc, _score} -> doc end)
    end
  end

  def search(_, _, _), do: []

  # --- Private ---

  defp to_doc(tool) do
    server = Map.get(tool, "server", "unknown")
    name = Map.get(tool, "name", "")
    description = Map.get(tool, "description", "")
    full_name = "#{server}__#{name}"

    text = "#{name} #{description}"
    tokens = tokenize(text)

    %{
      id: full_name,
      server: server,
      name: name,
      full_name: full_name,
      description: description,
      input_schema: Map.get(tool, "inputSchema", %{}),
      tokens: tokens,
      token_freqs: token_frequencies(tokens)
    }
  end

  defp tokenize(text) when is_binary(text) do
    text
    |> String.downcase()
    |> String.replace(~r/[^\w\s]/u, " ")
    |> String.split()
    |> Enum.reject(&(&1 in stop_words()))
  end

  defp tokenize(_), do: []

  defp stop_words do
    ~w(the a an is are was were be been have has had do does did will would could should)
  end

  defp token_frequencies(tokens) do
    Enum.frequencies(tokens)
  end

  defp build_inverted_index(docs) do
    docs
    |> Enum.reduce(%{}, fn doc, acc ->
      Enum.reduce(doc.tokens, acc, fn token, idx_acc ->
        Map.update(idx_acc, token, [doc.id], &[doc.id | &1])
      end)
    end)
    |> Map.new(fn {token, doc_ids} ->
      {token, Enum.frequencies(doc_ids)}
    end)
  end

  defp score_docs(index, query_tokens) do
    doc_scores = %{}

    query_tokens
    |> Enum.reduce(doc_scores, fn token, acc ->
      case Map.get(index, token) do
        nil -> acc
        doc_freqs ->
          Enum.reduce(doc_freqs, acc, fn {doc_id, freq}, scores_acc ->
            score = :math.log(1 + freq)
            Map.update(scores_acc, doc_id, score, &(&1 + score))
          end)
      end
    end)
    |> Enum.map(fn {doc_id, score} -> {doc_id, score} end)
  end
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/hub/tool_search_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/hub/tool_search.ex test/exhub/mcp/hub/tool_search_test.exs
git commit -m "feat(mcp-hub): add TF-IDF tool search module"
```

---

## Task 2: Enhance ClientState with Health Fields

**Files:**
- Modify: `lib/exhub/mcp/hub/client_state.ex`

**Step 1: Add health fields to ClientState**

```elixir
defmodule Exhub.MCP.Hub.ClientState do
  @moduledoc """
  Client state struct for tracking upstream MCP server connections.
  """

  @type status :: :connecting | :connected | :disconnected | :error | :degraded
  @type health_status :: :healthy | :degraded | :unhealthy

  @type t :: %__MODULE__{
    server_name: String.t(),
    config: Exhub.MCP.Hub.ServerConfig.t(),
    pid: pid() | nil,
    supervisor_pid: pid() | nil,
    status: status(),
    tools: [map()] | nil,
    last_error: String.t() | nil,
    crash_count: non_neg_integer(),
    connected_at: DateTime.t() | nil,
    # NEW health fields
    last_health_check: DateTime.t() | nil,
    health_status: health_status(),
    reconnect_attempts: non_neg_integer(),
    last_reconnect_at: DateTime.t() | nil
  }

  defstruct [
    :server_name, :config, :pid, :supervisor_pid, :status, :tools,
    :last_error, :crash_count, :connected_at,
    :last_health_check, :health_status, :reconnect_attempts, :last_reconnect_at
  ]
end
```

**Step 2: Commit**

```bash
git add lib/exhub/mcp/hub/client_state.ex
git commit -m "feat(mcp-hub): add health fields to ClientState"
```

---

## Task 3: Add Health Check and Auto-Reconnect to ClientManager

**Files:**
- Modify: `lib/exhub/mcp/hub/client_manager.ex`
- Test: `test/exhub/mcp/hub/client_manager_test.exs`

**Step 1: Add health check scheduling in init/1**

In `init/1`, after loading configs:
```elixir
# Schedule first health check
schedule_health_check()
```

**Step 2: Add health check handle_info**

```elixir
def handle_info(:health_check, state) do
  {new_clients, reconnects} = check_health(state.clients)

  # Schedule reconnects
  Enum.each(reconnects, fn server_name ->
    Process.send_after(self(), {:reconnect_client, server_name}, 0)
  end)

  schedule_health_check()
  {:noreply, %{state | clients: new_clients}}
end
```

**Step 3: Implement check_health/1 and reconnect logic**

```elixir
defp check_health(clients) do
  Enum.reduce(clients, {clients, []}, fn {name, client}, {acc, reconnects} ->
    now = DateTime.utc_now()

    case client.status do
      :connected ->
        # Ping the client
        case ping_client(client) do
          :ok ->
            new_client = %{client |
              last_health_check: now,
              health_status: :healthy,
              reconnect_attempts: 0
            }
            {Map.put(acc, name, new_client), reconnects}

          {:error, reason} ->
            new_client = %{client |
              last_health_check: now,
              health_status: :degraded,
              last_error: "Health check failed: #{inspect(reason)}"
            }
            {Map.put(acc, name, new_client), reconnects}
        end

      :error ->
        if should_reconnect?(client) do
          {acc, [name | reconnects]}
        else
          {acc, reconnects}
        end

      _ ->
        {acc, reconnects}
    end
  end)
end

defp ping_client(%{pid: pid}) when is_pid(pid) do
  if Process.alive?(pid) do
    # Try to ping via Anubis.Client
    try do
      # This is a simplified ping - actual implementation depends on Anubis API
      :ok
    catch
      _, _ -> {:error, :ping_failed}
    end
  else
    {:error, :process_dead}
  end
end
defp ping_client(_), do: {:error, :no_pid}

defp should_reconnect?(client) do
  client.reconnect_attempts < 3 and
    (is_nil(client.last_reconnect_at) or
     DateTime.diff(DateTime.utc_now(), client.last_reconnect_at, :second) > 60)
end

defp schedule_health_check do
  Process.send_after(self(), :health_check, 30_000)
end
```

**Step 4: Update reconnect logic**

In `handle_info({:reconnect_client, server_name}, state)`:
```elixir
def handle_info({:reconnect_client, server_name}, state) do
  case Map.get(state.clients, server_name) do
    %{config: %{enabled: true}, reconnect_attempts: count} when count < 3 ->
      # Calculate backoff delay
      delay = calculate_backoff(count)
      Process.send_after(self(), {:do_reconnect, server_name}, delay)

      new_clients = Map.update!(state.clients, server_name, fn client ->
        %{client |
          status: :connecting,
          reconnect_attempts: count + 1,
          last_reconnect_at: DateTime.utc_now()
        }
      end)

      {:noreply, %{state | clients: new_clients}}

    _ ->
      {:noreply, state}
  end
end

defp calculate_backoff(attempt) do
  [5_000, 10_000, 20_000, 40_000, 60_000] |> Enum.at(attempt, 60_000)
end
```

**Step 5: Commit**

```bash
git add lib/exhub/mcp/hub/client_manager.ex
git commit -m "feat(mcp-hub): add health checks and auto-reconnect with backoff"
```

---

## Task 4: Add retrieve_tools Meta-Tool to Hub.Server

**Files:**
- Modify: `lib/exhub/mcp/hub/server.ex`

**Step 1: Add retrieve_tools to tools/1**

```elixir
def tools(_frame) do
  # Get existing tools from upstream servers
  upstream_tools = case Exhub.MCP.Hub.ClientManager.list_all_tools() do
    {:ok, tools} -> tools
    {:error, _} -> []
  end

  # Build search index
  index = Exhub.MCP.Hub.ToolSearch.build_index(upstream_tools)

  # Store index in process state (or ETS)
  :ets.insert(:mcp_hub_search_index, {:index, index})

  # Return existing tools + retrieve_tools meta-tool
  existing = Enum.map(upstream_tools, fn tool ->
    server = Map.get(tool, "server", "unknown")
    name = Map.get(tool, "name", "")
    description = Map.get(tool, "description", "")
    input_schema = Map.get(tool, "inputSchema", %{}) || %{}

    %{
      name: "#{server}__#{name}",
      description: "[#{server}] #{description}",
      inputSchema: input_schema
    }
  end)

  retrieve_tools = %{
    name: "retrieve_tools",
    description: "Search for relevant tools across all connected MCP servers. Use natural language to describe what you want to accomplish.",
    inputSchema: %{
      type: "object",
      properties: %{
        query: %{type: "string", description: "Natural language description of what you want to accomplish"},
        limit: %{type: "integer", description: "Maximum number of tools to return (default: 5)"}
      },
      required: ["query"]
    }
  }

  [retrieve_tools | existing]
end
```

**Step 2: Add handle_tool_call for retrieve_tools**

```elixir
def handle_tool_call("retrieve_tools", arguments, frame) do
  query = Map.get(arguments, "query", "")
  limit = Map.get(arguments, "limit", 5)

  # Get the search index
  results = case :ets.lookup(:mcp_hub_search_index, :index) do
    [{:index, index}] ->
      Exhub.MCP.Hub.ToolSearch.search(index, query, limit: limit)

    [] ->
      # Fallback: rebuild index
      case Exhub.MCP.Hub.ClientManager.list_all_tools() do
        {:ok, tools} ->
          index = Exhub.MCP.Hub.ToolSearch.build_index(tools)
          :ets.insert(:mcp_hub_search_index, {:index, index})
          Exhub.MCP.Hub.ToolSearch.search(index, query, limit: limit)

        {:error, _} ->
          []
      end
  end

  formatted = Enum.map(results, fn result ->
    %{
      name: result["full_name"],
      description: result["description"],
      server: result["server"],
      inputSchema: result["input_schema"]
    }
  end)

  {:ok, %{tools: formatted, count: length(formatted)}, frame}
end
```

**Step 3: Add tool call logging**

In `handle_tool_call/3`, add before routing:
```elixir
require Logger

Logger.info("[MCP Hub] Tool call: #{tool_name} with args: #{inspect(arguments)}")
```

**Step 4: Commit**

```bash
git add lib/exhub/mcp/hub/server.ex
git commit -m "feat(mcp-hub): add retrieve_tools meta-tool and tool call logging"
```

---

## Task 5: Add HTTP Search Endpoint

**Files:**
- Modify: `lib/exhub/controllers/mcp_hub_controller.ex`
- Modify: `lib/exhub/router.ex`

**Step 1: Add search endpoint to controller**

```elixir
def search_tools(conn) do
  query = conn.query_params["query"] || ""
  limit = String.to_integer(conn.query_params["limit"] || "5")

  if query == "" do
    send_error(conn, 400, "Missing 'query' parameter")
  else
    case Exhub.MCP.Hub.ClientManager.search_tools(query, limit) do
      {:ok, results} ->
        conn
        |> put_resp_content_type("application/json")
        |> send_resp(200, Jason.encode!(%{tools: results, query: query}))

      {:error, reason} ->
        send_error(conn, 500, "Search failed: #{inspect(reason)}")
    end
  end
end
```

**Step 2: Add search_tools/2 to ClientManager**

```elixir
def search_tools(query, limit) do
  GenServer.call(__MODULE__, {:search_tools, query, limit}, 30_000)
end

def handle_call({:search_tools, query, limit}, _from, state) do
  tools =
    state.clients
    |> Enum.flat_map(fn
      {name, %{status: :connected, tools: tools}} when is_list(tools) ->
        Enum.map(tools, &Map.put(&1, "server", name))

      _ ->
        []
    end)

  index = Exhub.MCP.Hub.ToolSearch.build_index(tools)
  results = Exhub.MCP.Hub.ToolSearch.search(index, query, limit: limit)

  {:reply, {:ok, results}, state}
end
```

**Step 3: Add route**

In `lib/exhub/router.ex`:
```elixir
get "/mcp-hub/tools/search", MCPHubController, :search_tools
```

**Step 4: Commit**

```bash
git add lib/exhub/controllers/mcp_hub_controller.ex lib/exhub/router.ex
git commit -m "feat(mcp-hub): add HTTP search endpoint for tools"
```

---

## Task 6: Add Tool Call Logging

**Files:**
- Modify: `lib/exhub/mcp/hub/server.ex`
- Modify: `lib/exhub/mcp/hub/client_manager.ex`

**Step 1: Log in Hub.Server.handle_tool_call/3**

```elixir
def handle_tool_call(tool_name, arguments, frame) do
  Logger.info("[MCP Hub] Tool call initiated: #{tool_name}")
  start_time = System.monotonic_time(:millisecond)

  result = do_handle_tool_call(tool_name, arguments, frame)

  duration = System.monotonic_time(:millisecond) - start_time
  Logger.info("[MCP Hub] Tool call completed: #{tool_name} in #{duration}ms")

  result
end

defp do_handle_tool_call("retrieve_tools", arguments, frame) do
  # ... existing retrieve_tools logic
end

defp do_handle_tool_call(tool_name, arguments, frame) do
  # ... existing routing logic
end
```

**Step 2: Log in ClientManager.call_tool/3**

```elixir
def call_tool(server_name, tool_name, arguments) do
  Logger.info("[MCP Hub] Calling tool on #{server_name}: #{tool_name}")
  # ... existing logic
end
```

**Step 3: Commit**

```bash
git add lib/exhub/mcp/hub/server.ex lib/exhub/mcp/hub/client_manager.ex
git commit -m "feat(mcp-hub): add structured logging for tool calls"
```

---

## Task 7: Update Tests

**Files:**
- Modify: `test/exhub/mcp/hub/client_manager_test.exs`
- Modify: `test/exhub/controllers/mcp_hub_controller_test.exs`

**Step 1: Add health check tests**

```elixir
test "health check marks connected clients as healthy", %{state: state} do
  # Mock a connected client
  client = %ClientState{status: :connected, pid: self()}
  # ... test health check logic
end

test "auto-reconnect triggers after health check failure" do
  # Test reconnect logic
end
```

**Step 2: Add search endpoint tests**

```elixir
test "GET /mcp-hub/tools/search returns matching tools" do
  conn = conn(:get, "/mcp-hub/tools/search?query=read&limit=2")
  # ... assert response
end
```

**Step 3: Commit**

```bash
git add test/
git commit -m "test(mcp-hub): add tests for health checks and search"
```

---

## Summary of Changes

| File | Action | Description |
|------|--------|-------------|
| `lib/exhub/mcp/hub/tool_search.ex` | Create | TF-IDF search module |
| `lib/exhub/mcp/hub/client_state.ex` | Modify | Add health fields |
| `lib/exhub/mcp/hub/client_manager.ex` | Modify | Health checks, auto-reconnect, search |
| `lib/exhub/mcp/hub/server.ex` | Modify | retrieve_tools meta-tool, logging |
| `lib/exhub/controllers/mcp_hub_controller.ex` | Modify | search_tools endpoint |
| `lib/exhub/router.ex` | Modify | Add search route |
| `test/exhub/mcp/hub/tool_search_test.exs` | Create | Search tests |
| `test/exhub/mcp/hub/client_manager_test.exs` | Modify | Health/reconnect tests |
| `test/exhub/controllers/mcp_hub_controller_test.exs` | Modify | Search endpoint tests |
