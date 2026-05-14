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
    # Health monitoring fields
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

defmodule Exhub.MCP.Hub.ClientManager do
  @moduledoc """
  GenServer that manages upstream MCP server connections via Anubis.Client.

  Responsibilities:
  - Load and persist server configurations from priv/mcp_servers.json
  - Start/stop Anubis.Client instances via DynamicSupervisor
  - Aggregate tools from all connected upstream servers
  - Route tool calls to the appropriate upstream server
  - Expose management API for CRUD operations on servers

  ## Startup Behavior (Non-blocking)

  The ClientManager starts **without** blocking on upstream client initialization.
  On `init/1`, only the DynamicSupervisor and config are loaded. Actual client
  startup (including MCP handshake and tool discovery) is deferred to
  `handle_continue/2`, where each client is started in its own Task for
  parallelism. This ensures the ExHub application supervisor tree completes
  quickly, allowing the Cowboy WebSocket server to start on time.

  ## Registry Keys

  Clients are registered in Exhub.Registry with the format:
  `{:mcp_hub_client, server_name}`

  ## Configuration Persistence

  Configurations are stored in `priv/mcp_servers.json` and automatically
  persisted on any modification.
  """

  use GenServer
  require Logger

  alias Exhub.MCP.Hub.{ServerConfig, ClientState}

  defstruct [
    :config_path,
    configs: %{},
    clients: %{},
    pending_tasks: %{},
    pending_tool_calls: %{},
    supervisors: %{}
  ]

  @max_crashes 3

  # Client API

  @doc """
  Starts the ClientManager GenServer.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Lists all configured servers with their current status.
  """
  @spec list_servers() :: [map()]
  def list_servers, do: GenServer.call(__MODULE__, :list_servers)

  @doc """
  Lists all available tools from all connected servers.
  """
  @spec list_all_tools() :: {:ok, [map()]} | {:error, term()}
  def list_all_tools, do: GenServer.call(__MODULE__, :list_all_tools, 60_000)

  @doc """
  Searches for tools matching the given query.
  """
  @spec search_tools(String.t(), integer()) :: {:ok, [map()]} | {:error, term()}
  def search_tools(query, limit) do
    GenServer.call(__MODULE__, {:search_tools, query, limit}, 30_000)
  end

  @doc """
  Calls a tool on a specific upstream server.
  """
  @spec call_tool(String.t(), String.t(), map()) :: {:ok, map()} | {:error, term()}
  def call_tool(server_name, tool_name, arguments) do
    GenServer.call(__MODULE__, {:call_tool, server_name, tool_name, arguments}, 120_000)
  end

  @doc """
  Adds a new upstream server configuration.
  """
  @spec add_server(map()) :: {:ok, ServerConfig.t()} | {:error, term()}
  def add_server(config_data), do: GenServer.call(__MODULE__, {:add_server, config_data})

  @doc """
  Removes an upstream server configuration.
  """
  @spec remove_server(String.t()) :: :ok | {:error, term()}
  def remove_server(name), do: GenServer.call(__MODULE__, {:remove_server, name})

  @doc """
  Toggles the enabled state of a server.
  """
  @spec toggle_server(String.t(), boolean()) :: {:ok, ServerConfig.t()} | {:error, term()}
  def toggle_server(name, enabled), do: GenServer.call(__MODULE__, {:toggle_server, name, enabled})

  @doc """
  Gets the status of a specific server.
  """
  @spec get_server_status(String.t()) :: map() | nil
  def get_server_status(name), do: GenServer.call(__MODULE__, {:get_server_status, name})

  @doc """
  Gets the client PID for a server (used by ProxyPlug).
  """
  @spec get_client_pid(String.t()) :: {:ok, pid()} | {:error, term()}
  def get_client_pid(name), do: GenServer.call(__MODULE__, {:get_client_pid, name})

  @doc """
  Gets a server configuration by its exposed route name.
  """
  @spec get_exposed_route(String.t()) :: {:ok, ServerConfig.t()} | :not_found
  def get_exposed_route(route), do: GenServer.call(__MODULE__, {:get_exposed_route, route})

  @doc """
  Updates a server configuration.
  """
  @spec update_server(String.t(), map()) :: {:ok, ServerConfig.t()} | {:error, term()}
  def update_server(name, config_data), do: GenServer.call(__MODULE__, {:update_server, name, config_data})

  # Server Callbacks

  @impl true
  def init(_opts) do
    Process.flag(:trap_exit, true)

    config_path = Path.join(:code.priv_dir(:exhub), "mcp_servers.json")
    configs = load_configs(config_path)

    # Merge built-in server configs with external configs from mcp_servers.json
    builtin_configs = builtin_server_configs()
    all_configs = merge_configs(configs, builtin_configs)

    # Initialize all enabled clients as :connecting — actual startup is deferred
    # to handle_continue so that init/1 returns immediately and does not block
    # the ExHub supervisor tree (and the Cowboy WebSocket server startup).
    # Built-in servers are marked as :connected immediately since they run
    # in the same BEAM VM and don't need Anubis.Client connections.
    clients =
      all_configs
      |> Enum.filter(& &1.enabled)
      |> Enum.reduce(%{}, fn config, acc ->
        # Built-in servers are always available (no HTTP connection needed)
        initial_status = if config.builtin, do: :connected, else: :connecting

        Map.put(acc, config.name, %ClientState{
          server_name: config.name,
          config: config,
          status: initial_status,
          crash_count: 0,
          health_status: :healthy,
          reconnect_attempts: 0
        })
      end)

    state = %__MODULE__{
      config_path: config_path,
      configs: Map.new(all_configs, &{&1.name, &1}),
      clients: clients,
      pending_tasks: %{},
      pending_tool_calls: %{},
      supervisors: %{}
    }

    # Schedule first health check
    schedule_health_check()

    {:ok, state, {:continue, :start_clients}}
  end

  @impl true
  def handle_continue(:start_clients, state) do
    enabled_configs =
      state.configs
      |> Map.values()
      |> Enum.filter(& &1.enabled)

    if Enum.empty?(enabled_configs) do
      {:noreply, state}
    else
      # Each client gets its own DynamicSupervisor for fault isolation.
      # Launch each client in its own Task for parallel startup.
      # Built-in servers are skipped since they run in the same BEAM VM
      # and are accessed directly via BuiltInRegistry.
      {pending_tasks, supervisors} =
        enabled_configs
        |> Enum.reject(& &1.builtin)
        |> Enum.reduce({%{}, state.supervisors}, fn config, {tasks_acc, sups_acc} ->
          {:ok, sup_pid} = new_dynamic_supervisor()
          task = spawn_client_task(sup_pid, config)
          {
            Map.put(tasks_acc, task.ref, config.name),
            Map.put(sups_acc, config.name, sup_pid)
          }
        end)

      {:noreply, %{state | pending_tasks: pending_tasks, supervisors: supervisors}}
    end
  end

  @impl true
  def handle_info({ref, result}, state) when is_reference(ref) do
    Process.demonitor(ref, [:flush])

    # Check if this is a client startup task
    case Map.pop(state.pending_tasks, ref) do
      {nil, _} ->
        # Check if this is a tool call task
        case Map.pop(state.pending_tool_calls, ref) do
          {nil, _} ->
            {:noreply, state}

          {{from, server_name, tool_name, start_time}, new_pending_calls} ->
            duration = System.monotonic_time(:millisecond) - start_time
            Logger.info("[MCP Hub] Tool call completed: #{server_name}:#{tool_name} in #{duration}ms")
            GenServer.reply(from, result)
            {:noreply, %{state | pending_tool_calls: new_pending_calls}}
        end

      {server_name, new_pending} ->
        new_clients = apply_client_result(state.clients, server_name, result)

        # Rebuild search index after a client connects successfully
        case result do
          {:ok, _pid, _tools} -> rebuild_search_index(new_clients)
          _ -> :ok
        end

        {:noreply, %{state | clients: new_clients, pending_tasks: new_pending}}
    end
  end

  @impl true
  def handle_info({:DOWN, ref, :process, _pid, reason}, state) do
    # Check if this is a client startup task
    case Map.pop(state.pending_tasks, ref) do
      {nil, _} ->
        # Check if this is a tool call task
        case Map.pop(state.pending_tool_calls, ref) do
          {nil, _} ->
            {:noreply, state}

          {{from, server_name, tool_name, start_time}, new_pending_calls} ->
            duration = System.monotonic_time(:millisecond) - start_time
            Logger.error("[MCP Hub] Tool call task crashed: #{server_name}:#{tool_name} in #{duration}ms: #{inspect(reason)}")
            GenServer.reply(from, {:error, {:task_crashed, reason}})
            {:noreply, %{state | pending_tool_calls: new_pending_calls}}
        end

      {server_name, new_pending} ->
        Logger.error("Client startup task for #{server_name} crashed: #{inspect(reason)}")

        new_clients =
          Map.update!(state.clients, server_name, fn client ->
            %{client | status: :error, last_error: "task_crashed: #{inspect(reason)}"}
          end)

        {:noreply, %{state | clients: new_clients, pending_tasks: new_pending}}
    end
  end

  @impl true
  def handle_info({:EXIT, pid, reason}, state) do
    # Check if this is a per-client DynamicSupervisor crashing
    case Enum.find(state.supervisors, fn {_name, sup_pid} -> sup_pid == pid end) do
      {server_name, _sup_pid} ->
        crash_count = get_in(state.clients, [server_name, :crash_count]) || 0
        new_crash_count = crash_count + 1

        Logger.error(
          "DynamicSupervisor for #{server_name} crashed (#{new_crash_count}/#{@max_crashes}): #{inspect(reason)}"
        )

        if new_crash_count >= @max_crashes do
          # Permanently disable this client
          Logger.error("Client #{server_name} exceeded max crashes (#{@max_crashes}), permanently disabling")

          new_clients =
            Map.update!(state.clients, server_name, fn client ->
              %{client | status: :error, pid: nil, supervisor_pid: nil,
                last_error: "max_crashes_reached: #{inspect(reason)}",
                crash_count: new_crash_count}
            end)

          {:noreply, %{state | clients: new_clients, supervisors: Map.delete(state.supervisors, server_name)}}
        else
          # Schedule reconnect for this specific client after delay
          new_clients =
            Map.update!(state.clients, server_name, fn client ->
              %{client | status: :error, pid: nil, supervisor_pid: nil,
                last_error: "supervisor_crashed: #{inspect(reason)}",
                crash_count: new_crash_count}
            end)

          Process.send_after(self(), {:reconnect_client, server_name}, 30_000)

          {:noreply, %{state | clients: new_clients, supervisors: Map.delete(state.supervisors, server_name)}}
        end

      nil ->
        # EXIT from an unrelated process — ignore
        {:noreply, state}
    end
  end

  @impl true
  def handle_info(:health_check, state) do
    {new_clients, reconnects} = check_health(state.clients)

    # Schedule reconnects with backoff
    Enum.each(reconnects, fn {server_name, attempt} ->
      delay = calculate_backoff(attempt)
      Process.send_after(self(), {:reconnect_client, server_name}, delay)
    end)

    schedule_health_check()
    {:noreply, %{state | clients: new_clients}}
  end

  @impl true
  def handle_info({:reconnect_client, server_name}, state) do
    case Map.get(state.clients, server_name) do
      %{config: %{enabled: true}, crash_count: count} when count < @max_crashes ->
        {:ok, sup_pid} = new_dynamic_supervisor()

        task = spawn_client_task(sup_pid, state.clients[server_name].config)

        new_clients =
          Map.update!(state.clients, server_name, fn client ->
            %{client | status: :connecting, supervisor_pid: sup_pid}
          end)

        new_supervisors = Map.put(state.supervisors, server_name, sup_pid)
        new_pending = Map.put(state.pending_tasks, task.ref, server_name)

        {:noreply, %{state | clients: new_clients, supervisors: new_supervisors, pending_tasks: new_pending}}

      _ ->
        {:noreply, state}
    end
  end

  @impl true
  def handle_info(_msg, state) do
    {:noreply, state}
  end

  @impl true
  def handle_call(:list_servers, _from, state) do
    servers =
      state.configs
      |> Map.values()
      |> Enum.map(fn config ->
        client = Map.get(state.clients, config.name)

        # For built-in servers, get tool count from BuiltInRegistry
        tool_count =
          if config.builtin do
            length(Exhub.MCP.Hub.BuiltInRegistry.list_tools(config.name))
          else
            if(client && client.tools, do: length(client.tools), else: 0)
          end

        %{
          name: config.name,
          transport: config.transport,
          enabled: config.enabled,
          status: if(client, do: client.status, else: :disconnected),
          tool_count: tool_count,
          last_error: if(client, do: client.last_error),
          expose_route: config.expose_route
        }
      end)

    {:reply, servers, state}
  end

  @impl true
  def handle_call(:list_all_tools, _from, state) do
    upstream_tools =
      state.clients
      |> Enum.flat_map(fn
        {name, %{status: :connected, tools: tools, pid: pid}} when is_list(tools) ->
          if Process.alive?(pid) do
            Enum.map(tools, &Map.put(&1, "server", name))
          else
            []
          end

        _ ->
          []
      end)

    # Merge with built-in server tools (always available, no HTTP overhead)
    builtin_tools = Exhub.MCP.Hub.BuiltInRegistry.list_all_tools()

    {:reply, {:ok, upstream_tools ++ builtin_tools}, state}
  end

  @impl true
  def handle_call({:search_tools, query, limit}, _from, state) do
    upstream_tools =
      state.clients
      |> Enum.flat_map(fn
        {name, %{status: :connected, tools: tools}} when is_list(tools) ->
          Enum.map(tools, &Map.put(&1, "server", name))

        _ ->
          []
      end)

    # Include built-in server tools in search
    builtin_tools = Exhub.MCP.Hub.BuiltInRegistry.list_all_tools()
    all_tools = upstream_tools ++ builtin_tools

    index = Exhub.MCP.Hub.ToolSearch.build_index(all_tools)
    results = Exhub.MCP.Hub.ToolSearch.search(index, query, limit: limit)

    {:reply, {:ok, results}, state}
  end

  @impl true
  def handle_call({:call_tool, server_name, tool_name, arguments}, from, state) do
    Logger.info("[MCP Hub] Calling tool on #{server_name}: #{tool_name}")

    # Check if this is a built-in server first (direct execution, no HTTP)
    if Exhub.MCP.Hub.BuiltInRegistry.built_in?(server_name) do
      result =
        case Exhub.MCP.Hub.BuiltInRegistry.call_tool(server_name, tool_name, arguments) do
          {:ok, result} -> {:reply, {:ok, result}, state}
          {:error, reason} -> {:reply, {:error, reason}, state}
        end

      duration = 0
      Logger.info("[MCP Hub] Tool call completed: #{server_name}:#{tool_name} in #{duration}ms")
      result
    else
      case Map.get(state.clients, server_name) do
        %{status: :connecting} ->
          {:reply, {:error, {:connecting, "Server '#{server_name}' is still initializing, please retry later"}}, state}

        %{status: :connected, pid: pid} ->
          if Process.alive?(pid) do
            # Spawn async task to avoid blocking the GenServer during upstream call
            start_time = System.monotonic_time(:millisecond)
            client_reg_name = ServerConfig.client_name(server_name)

            task =
              Task.Supervisor.async_nolink(
                Exhub.MCP.Hub.TaskSupervisor,
                fn ->
                  result =
                    try do
                      case Anubis.Client.call_tool(client_reg_name, tool_name, arguments, timeout: 120_000) do
                        {:ok, %{is_error: true} = response} ->
                          error_text = extract_error_text(response)
                          {:error, error_text}

                        {:ok, response} ->
                          {:ok, response.result}

                        {:error, reason} ->
                          {:error, reason}
                      end
                    rescue
                      e -> {:error, {:exception, inspect(e)}}
                    catch
                      kind, reason -> {:error, {kind, reason}}
                    end

                  {server_name, tool_name, start_time, result}
                end
              )

            new_pending = Map.put(state.pending_tool_calls, task.ref, {from, server_name, tool_name, start_time})
            {:noreply, %{state | pending_tool_calls: new_pending}}
          else
            {:reply, {:error, :server_not_running}, state}
          end

        %{status: :error, last_error: error} ->
          {:reply, {:error, {:upstream_error, error}}, state}

        nil ->
          {:reply, {:error, :server_not_found}, state}
      end
    end
  end

  @impl true
  def handle_call({:add_server, config_data}, _from, state) do
    config = ServerConfig.from_json(config_data)

    with :ok <- ServerConfig.validate(config),
         :ok <- check_name_unique(config.name, state.configs) do
      new_state = start_and_register_client(state, config)
      {:reply, {:ok, config}, new_state}
    else
      {:error, reason} -> {:reply, {:error, reason}, state}
    end
  end

  @impl true
  def handle_call({:remove_server, name}, _from, state) do
    # Prevent removal of built-in servers
    case Map.get(state.configs, name) do
      %{builtin: true} ->
        {:reply, {:error, :cannot_remove_builtin}, state}

      _ ->
        do_remove_server(name, state)
    end
  end

  @impl true
  def handle_call({:toggle_server, name, enabled}, _from, state) do
    case Map.get(state.configs, name) do
      nil ->
        {:reply, {:error, :server_not_found}, state}

      %{builtin: true} ->
        {:reply, {:error, :cannot_toggle_builtin}, state}

      config ->
        updated_config = %{config | enabled: enabled, updated_at: DateTime.utc_now()}
        new_configs = Map.put(state.configs, name, updated_config)

        new_state =
          if enabled do
            # Create a new DynamicSupervisor and start client asynchronously
            {:ok, sup_pid} = new_dynamic_supervisor()
            task = spawn_client_task(sup_pid, updated_config)

            new_clients = Map.put(state.clients, name, %ClientState{
              server_name: name,
              config: updated_config,
              status: :connecting,
              supervisor_pid: sup_pid,
              crash_count: 0,
              health_status: :healthy,
              reconnect_attempts: 0
            })

            new_supervisors = Map.put(state.supervisors, name, sup_pid)
            new_pending = Map.put(state.pending_tasks, task.ref, name)

            %{state | configs: new_configs, clients: new_clients, supervisors: new_supervisors, pending_tasks: new_pending}
          else
            # Terminate the client and its supervisor
            client = state.clients[name]
            if client && client.pid && client.supervisor_pid do
              DynamicSupervisor.terminate_child(client.supervisor_pid, client.pid)
            end
            if sup_pid = state.supervisors[name] do
              Process.exit(sup_pid, :normal)
            end

            new_clients = Map.put(state.clients, name, %ClientState{
              server_name: name,
              config: updated_config,
              status: :disconnected,
              crash_count: 0,
              health_status: :healthy,
              reconnect_attempts: 0
            })

            %{state | configs: new_configs, clients: new_clients, supervisors: Map.delete(state.supervisors, name)}
          end

        save_configs(new_state.configs, new_state.config_path)
        {:reply, {:ok, updated_config}, new_state}
    end
  end

  @impl true
  def handle_call({:get_server_status, name}, _from, state) do
    config = Map.get(state.configs, name)
    client = Map.get(state.clients, name)

    result =
      if config do
        # For built-in servers, get tool count from BuiltInRegistry
        tool_count =
          if config.builtin do
            length(Exhub.MCP.Hub.BuiltInRegistry.list_tools(config.name))
          else
            if(client && client.tools, do: length(client.tools), else: 0)
          end

        %{
          name: config.name,
          transport: config.transport,
          enabled: config.enabled,
          status: if(client, do: client.status, else: :disconnected),
          tool_count: tool_count,
          last_error: if(client, do: client.last_error),
          expose_route: config.expose_route
        }
      else
        nil
      end

    {:reply, result, state}
  end

  @impl true
  def handle_call({:get_client_pid, name}, _from, state) do
    case Map.get(state.clients, name) do
      %{status: :connected, pid: pid} when is_pid(pid) ->
        if Process.alive?(pid) do
          {:reply, {:ok, pid}, state}
        else
          {:reply, {:error, :server_not_running}, state}
        end

      _ ->
        {:reply, {:error, :server_not_found}, state}
    end
  end

  @impl true
  def handle_call({:get_exposed_route, route}, _from, state) do
    result =
      state.configs
      |> Enum.find(fn {_name, config} ->
        ServerConfig.exposed?(config) && config.expose_route == route
      end)

    case result do
      {_, config} -> {:reply, {:ok, config}, state}
      nil -> {:reply, :not_found, state}
    end
  end

  @impl true
  def handle_call({:update_server, name, config_data}, _from, state) do
    case Map.get(state.configs, name) do
      nil ->
        {:reply, {:error, :server_not_found}, state}

      existing_config ->
        updated_config = ServerConfig.from_json(
          Map.merge(
            ServerConfig.to_json(existing_config),
            config_data
          )
        )
        |> Map.put(:updated_at, DateTime.utc_now())

        with :ok <- ServerConfig.validate(updated_config) do
          new_configs = Map.put(state.configs, name, updated_config)

          # Stop existing client and supervisor
          client = state.clients[name]
          if client && client.pid && client.supervisor_pid do
            DynamicSupervisor.terminate_child(client.supervisor_pid, client.pid)
          end
          if sup_pid = state.supervisors[name] do
            Process.exit(sup_pid, :normal)
          end

          {new_clients, new_supervisors, new_pending} =
            if updated_config.enabled do
              {:ok, sup_pid} = new_dynamic_supervisor()
              task = spawn_client_task(sup_pid, updated_config)

              new_clients_map = Map.put(state.clients, name, %ClientState{
                server_name: name,
                config: updated_config,
                status: :connecting,
                supervisor_pid: sup_pid,
                crash_count: 0,
                health_status: :healthy,
                reconnect_attempts: 0
              })

              {new_clients_map, Map.put(state.supervisors, name, sup_pid),
               Map.put(state.pending_tasks, task.ref, name)}
            else
              new_clients_map = Map.put(state.clients, name, %ClientState{
                server_name: name,
                config: updated_config,
                status: :disconnected,
                crash_count: 0,
                health_status: :healthy,
                reconnect_attempts: 0
              })

              {new_clients_map, Map.delete(state.supervisors, name), state.pending_tasks}
            end

          new_state = %{state | configs: new_configs, clients: new_clients, supervisors: new_supervisors, pending_tasks: new_pending}
          save_configs(new_state.configs, new_state.config_path)
          {:reply, {:ok, updated_config}, new_state}
        else
          {:error, reason} -> {:reply, {:error, reason}, state}
        end
    end
  end

  defp do_remove_server(name, state) do
    # Terminate the Anubis.Client child under its DynamicSupervisor
    case Map.get(state.clients, name) do
      %{pid: pid, supervisor_pid: sup_pid} when is_pid(pid) and is_pid(sup_pid) ->
        DynamicSupervisor.terminate_child(sup_pid, pid)
        Logger.info("Stopped client for server: #{name}")

      %{pid: pid} when is_pid(pid) ->
        Logger.warning("No supervisor found for #{name}, sending exit to client pid")
        Process.exit(pid, :normal)

      _ ->
        :ok
    end

    # Shut down the per-client DynamicSupervisor
    if sup_pid = state.supervisors[name] do
      Process.exit(sup_pid, :normal)
    end

    new_state = %{state |
      configs: Map.delete(state.configs, name),
      clients: Map.delete(state.clients, name),
      supervisors: Map.delete(state.supervisors, name)
    }

    save_configs(new_state.configs, new_state.config_path)
    {:reply, :ok, new_state}
  end

  # Built-in server configurations
  # These are auto-registered as upstream servers via localhost HTTP
  defp builtin_server_configs do
    port = Application.get_env(:exhub, :port, 9069)
    base_url = "http://localhost:#{port}"

    [
      %{name: "habit", route: "/mcp"},
      %{name: "time", route: "/time/mcp"},
      %{name: "think", route: "/think/mcp"},
      %{name: "web-tools", route: "/web-tools/mcp"},
      %{name: "archery", route: "/archery/mcp"},
      %{name: "browser-use", route: "/browser-use/mcp"},
      %{name: "image-gen", route: "/image-gen/mcp"},
      %{name: "doc-extract", route: "/doc-extract/mcp"},
      %{name: "look", route: "/look/mcp"},
      %{name: "todo", route: "/todo/mcp"},
      %{name: "desktop", route: "/desktop/mcp"},
      %{name: "agent", route: "/agent/mcp"},
      %{name: "brain", route: "/brain/mcp"},
      %{name: "exhub", route: "/exhub/mcp"},
      %{name: "mac-use", route: "/mac-use/mcp"}
    ]
    |> Enum.map(fn %{name: name, route: route} ->
      %ServerConfig{
        name: name,
        transport: :streamable_http,
        enabled: true,
        url: "#{base_url}#{route}",
        headers: %{},
        args: [],
        env: %{},
        builtin: true,
        created_at: DateTime.utc_now(),
        updated_at: DateTime.utc_now()
      }
    end)
  end

  # Merge external configs with built-in configs.
  # External configs take precedence (can override built-in).
  defp merge_configs(external_configs, builtin_configs) do
    external_map = Map.new(external_configs, &{&1.name, &1})

    # Only include built-in configs that aren't overridden by external configs
    builtin_filtered = Enum.reject(builtin_configs, &Map.has_key?(external_map, &1.name))

    external_configs ++ builtin_filtered
  end

  # Rebuild the TF-IDF search index from all connected clients.
  # Called after a client successfully connects to keep the index fresh.
  defp rebuild_search_index(clients) do
    tools =
      clients
      |> Enum.flat_map(fn
        {name, %{status: :connected, tools: tools, pid: pid}} when is_list(tools) ->
          if Process.alive?(pid) do
            Enum.map(tools, &Map.put(&1, "server", name))
          else
            []
          end

        _ ->
          []
      end)

    index = Exhub.MCP.Hub.ToolSearch.build_index(tools)
    Exhub.MCP.Hub.Store.put_search_index(index)
  end

  # Private functions

  defp new_dynamic_supervisor do
    DynamicSupervisor.start_link(
      strategy: :one_for_one,
      max_restarts: 3,
      max_seconds: 10
    )
  end

  defp spawn_client_task(supervisor, config) do
    # Each client is started in a dedicated Task that:
    # 1. Starts the Anubis.Client under its own DynamicSupervisor
    # 2. Waits for MCP handshake (with retries)
    # 3. Discovers available tools
    # The result is sent back as a message to this GenServer.
    Task.Supervisor.async_nolink(
      Exhub.MCP.Hub.TaskSupervisor,
      fn ->
        start_client_sync(supervisor, config)
      end
    )
  end

  defp start_client_sync(supervisor, config) do
    client_opts = ServerConfig.to_anubis_client_opts(config)
    client_reg_name = ServerConfig.client_name(config.name)
    client_label = config.name

    case DynamicSupervisor.start_child(supervisor, {Anubis.Client, client_opts}) do
      {:ok, pid} ->
        # Retry list_tools with backoff to wait for MCP initialize handshake
        tools = fetch_tools_with_retry(client_label, client_reg_name, 5, 1000)

        Logger.info("Client #{client_label} started (PID: #{inspect(pid)}), #{length(tools)} tools")
        {:ok, pid, tools}

      {:error, reason} ->
        Logger.error("Failed to start client #{client_label}: #{inspect(reason)}")
        {:error, reason}
    end
  end

  defp apply_client_result(clients, server_name, {:ok, pid, tools}) do
    Map.update!(clients, server_name, fn client ->
      %{client |
        pid: pid,
        status: :connected,
        tools: tools,
        connected_at: DateTime.utc_now(),
        last_error: nil,
        crash_count: 0
      }
    end)
  end

  defp apply_client_result(clients, server_name, {:error, reason}) do
    Map.update!(clients, server_name, fn client ->
      %{client |
        status: :error,
        last_error: inspect(reason)
      }
    end)
  end

  defp fetch_tools_with_retry(client_label, client_reg_name, attempts, base_delay) do
    do_fetch_tools(client_label, client_reg_name, attempts, base_delay, [])
  end

  defp do_fetch_tools(_client_label, _client_reg_name, 0, _delay, []) do
    []
  end

  defp do_fetch_tools(_client_label, _client_reg_name, 0, _delay, tools) do
    tools
  end

  defp do_fetch_tools(client_label, client_reg_name, attempts, delay, _prev_tools) do
    result =
      try do
        case Anubis.Client.list_tools(client_reg_name, timeout: 10_000) do
          {:ok, %{result: %{"tools" => tool_list}}} when is_list(tool_list) -> {:ok, tool_list}
          {:ok, %{result: result}} when is_map(result) ->
            {:ok, Map.get(result, "tools", [])}
          {:error, %Anubis.MCP.Error{} = e} ->
            {:error, e}
          other ->
            Logger.warning("Client #{client_label} list_tools unexpected: #{inspect(other)}")
            {:error, :unexpected}
        end
      rescue
        e ->
          Logger.warning("Client #{client_label} list_tools exception: #{inspect(e)}")
          {:error, :exception}
      catch
        kind, reason ->
          Logger.warning("Client #{client_label} list_tools caught #{kind}: #{inspect(reason)}")
          {:error, kind}
      end

    case result do
      {:ok, tools} -> tools
      {:error, _reason} ->
        Process.sleep(delay)
        do_fetch_tools(client_label, client_reg_name, attempts - 1, delay, [])
    end
  end

  defp start_and_register_client(state, config) do
    # Create a dedicated DynamicSupervisor for this client
    {:ok, sup_pid} = new_dynamic_supervisor()

    task = spawn_client_task(sup_pid, config)

    new_clients = Map.put(state.clients, config.name, %ClientState{
      server_name: config.name,
      config: config,
      status: :connecting,
      supervisor_pid: sup_pid,
      crash_count: 0,
      health_status: :healthy,
      reconnect_attempts: 0
    })

    new_configs = Map.put(state.configs, config.name, config)
    new_supervisors = Map.put(state.supervisors, config.name, sup_pid)
    new_pending = Map.put(state.pending_tasks, task.ref, config.name)

    save_configs(new_configs, state.config_path)

    %{state |
      configs: new_configs,
      clients: new_clients,
      supervisors: new_supervisors,
      pending_tasks: new_pending
    }
  end

  defp load_configs(path) do
    if File.exists?(path) do
      case File.read!(path) |> Jason.decode() do
        {:ok, %{"servers" => servers}} when is_list(servers) ->
          Enum.map(servers, &ServerConfig.from_json/1)

        _ ->
          []
      end
    else
      []
    end
  end

  defp save_configs(configs, path) do
    data = %{
      "servers" =>
        configs
        |> Map.values()
        |> Enum.reject(& &1.builtin)
        |> Enum.map(&ServerConfig.to_json/1)
    }

    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Jason.encode!(data, pretty: true))
  end

  defp check_name_unique(name, configs) do
    if Map.has_key?(configs, name), do: {:error, :server_already_exists}, else: :ok
  end

  # --- Health Check Functions ---

  defp schedule_health_check do
    Process.send_after(self(), :health_check, 30_000)
  end

  defp check_health(clients) do
    Enum.reduce(clients, {clients, []}, fn {name, client}, {acc, reconnects} ->
      now = DateTime.utc_now()

      case client.status do
        :connected ->
          # For connected clients, just update health check timestamp
          new_client = %{client |
            last_health_check: now,
            health_status: :healthy
          }
          {Map.put(acc, name, new_client), reconnects}

        :error ->
          if should_reconnect?(client) do
            new_client = %{client |
              last_health_check: now,
              reconnect_attempts: client.reconnect_attempts + 1,
              last_reconnect_at: now
            }
            {Map.put(acc, name, new_client), [{name, client.reconnect_attempts} | reconnects]}
          else
            new_client = %{client | last_health_check: now}
            {Map.put(acc, name, new_client), reconnects}
          end

        _ ->
          {acc, reconnects}
      end
    end)
  end

  defp should_reconnect?(client) do
    client.reconnect_attempts < 3 and
      (is_nil(client.last_reconnect_at) or
       DateTime.diff(DateTime.utc_now(), client.last_reconnect_at, :second) > 60)
  end

  defp calculate_backoff(attempt) do
    [5_000, 10_000, 20_000, 40_000, 60_000] |> Enum.at(attempt, 60_000)
  end

  defp extract_error_text(%{result: %{"content" => content}}) when is_list(content) do
    content
    |> Enum.filter(&(&1["type"] == "text"))
    |> Enum.map_join("\n", &(&1["text"] || ""))
  end

  defp extract_error_text(%{result: result}), do: inspect(result)
  defp extract_error_text(other), do: inspect(other)
end
