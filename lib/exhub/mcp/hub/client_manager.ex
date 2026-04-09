defmodule Exhub.MCP.Hub.ClientState do
  @moduledoc """
  Client state struct for tracking upstream MCP server connections.
  """

  @type status :: :connecting | :connected | :disconnected | :error

  @type t :: %__MODULE__{
    server_name: String.t(),
    config: Exhub.MCP.Hub.ServerConfig.t(),
    pid: pid() | nil,
    status: status(),
    tools: [map()] | nil,
    last_error: String.t() | nil,
    connected_at: DateTime.t() | nil
  }

  defstruct [:server_name, :config, :pid, :status, :tools, :last_error, :connected_at]
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
    :dynamic_supervisor,
    :config_path,
    configs: %{},
    clients: %{}
  ]

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
    {:ok, dynamic_supervisor} = DynamicSupervisor.start_link(strategy: :one_for_one)

    config_path = Path.join(:code.priv_dir(:exhub), "mcp_servers.json")
    configs = load_configs(config_path)

    clients =
      configs
      |> Enum.filter(& &1.enabled)
      |> Enum.reduce(%{}, fn config, acc ->
        case start_client(dynamic_supervisor, config) do
          {:ok, pid, tools} ->
            Map.put(acc, config.name, %ClientState{
              server_name: config.name,
              config: config,
              pid: pid,
              status: :connected,
              tools: tools,
              connected_at: DateTime.utc_now()
            })

          {:error, reason} ->
            Logger.error("Failed to start client #{config.name}: #{inspect(reason)}")

            Map.put(acc, config.name, %ClientState{
              server_name: config.name,
              config: config,
              status: :error,
              last_error: inspect(reason)
            })
        end
      end)

    {:ok, %__MODULE__{
      dynamic_supervisor: dynamic_supervisor,
      config_path: config_path,
      configs: Map.new(configs, &{&1.name, &1}),
      clients: clients
    }}
  end

  @impl true
  def handle_call(:list_servers, _from, state) do
    servers =
      state.configs
      |> Map.values()
      |> Enum.map(fn config ->
        client = Map.get(state.clients, config.name)

        %{
          name: config.name,
          transport: config.transport,
          enabled: config.enabled,
          status: if(client, do: client.status, else: :disconnected),
          tool_count: if(client && client.tools, do: length(client.tools), else: 0),
          last_error: if(client, do: client.last_error),
          expose_route: config.expose_route
        }
      end)

    {:reply, servers, state}
  end

  @impl true
  def handle_call(:list_all_tools, _from, state) do
    tools =
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

    {:reply, {:ok, tools}, state}
  end

  @impl true
  def handle_call({:call_tool, server_name, tool_name, arguments}, _from, state) do
    case Map.get(state.clients, server_name) do
      %{status: :connected, pid: pid} ->
        if Process.alive?(pid) do
          client_reg_name = ServerConfig.client_name(server_name)

          case Anubis.Client.call_tool(client_reg_name, tool_name, arguments, timeout: 120_000) do
            {:ok, response} ->
              {:reply, {:ok, response}, state}

            {:error, reason} ->
              {:reply, {:error, reason}, state}
          end
        else
          {:reply, {:error, :server_not_running}, state}
        end

      %{status: :error, last_error: error} ->
        {:reply, {:error, {:upstream_error, error}}, state}

      nil ->
        {:reply, {:error, :server_not_found}, state}
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
    case Map.get(state.clients, name) do
      %{pid: pid} when is_pid(pid) ->
        DynamicSupervisor.terminate_child(state.dynamic_supervisor, pid)
        Logger.info("Stopped client for server: #{name}")

      _ ->
        :ok
    end

    new_state = %{state |
      configs: Map.delete(state.configs, name),
      clients: Map.delete(state.clients, name)
    }

    save_configs(new_state.configs, new_state.config_path)
    {:reply, :ok, new_state}
  end

  @impl true
  def handle_call({:toggle_server, name, enabled}, _from, state) do
    case Map.get(state.configs, name) do
      nil ->
        {:reply, {:error, :server_not_found}, state}

      config ->
        updated_config = %{config | enabled: enabled, updated_at: DateTime.utc_now()}
        new_configs = Map.put(state.configs, name, updated_config)

        new_clients =
          if enabled do
            case start_client(state.dynamic_supervisor, updated_config) do
              {:ok, pid, tools} ->
                Map.put(state.clients, name, %ClientState{
                  server_name: name,
                  config: updated_config,
                  pid: pid,
                  status: :connected,
                  tools: tools,
                  connected_at: DateTime.utc_now()
                })

              {:error, reason} ->
                Logger.error("Failed to start client #{name}: #{inspect(reason)}")

                Map.put(state.clients, name, %ClientState{
                  server_name: name,
                  config: updated_config,
                  status: :error,
                  last_error: inspect(reason)
                })
            end
          else
            case Map.get(state.clients, name) do
              %{pid: pid} when is_pid(pid) ->
                DynamicSupervisor.terminate_child(state.dynamic_supervisor, pid)

              _ ->
                :ok
            end

            Map.put(state.clients, name, %ClientState{
              server_name: name,
              config: updated_config,
              status: :disconnected
            })
          end

        new_state = %{state | configs: new_configs, clients: new_clients}
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
        %{
          name: config.name,
          transport: config.transport,
          enabled: config.enabled,
          status: if(client, do: client.status, else: :disconnected),
          tool_count: if(client && client.tools, do: length(client.tools), else: 0),
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

          new_clients =
            if updated_config.enabled do
              case Map.get(state.clients, name) do
                %{pid: pid} when is_pid(pid) ->
                  DynamicSupervisor.terminate_child(state.dynamic_supervisor, pid)
                _ -> :ok
              end

              case start_client(state.dynamic_supervisor, updated_config) do
                {:ok, pid, tools} ->
                  Map.put(state.clients, name, %ClientState{
                    server_name: name,
                    config: updated_config,
                    pid: pid,
                    status: :connected,
                    tools: tools,
                    connected_at: DateTime.utc_now()
                  })

                {:error, reason} ->
                  Logger.error("Failed to restart client #{name}: #{inspect(reason)}")

                  Map.put(state.clients, name, %ClientState{
                    server_name: name,
                    config: updated_config,
                    status: :error,
                    last_error: inspect(reason)
                  })
              end
            else
              case Map.get(state.clients, name) do
                %{pid: pid} when is_pid(pid) ->
                  DynamicSupervisor.terminate_child(state.dynamic_supervisor, pid)
                _ -> :ok
              end

              Map.put(state.clients, name, %ClientState{
                server_name: name,
                config: updated_config,
                status: :disconnected
              })
            end

          new_state = %{state | configs: new_configs, clients: new_clients}
          save_configs(new_state.configs, new_state.config_path)
          {:reply, {:ok, updated_config}, new_state}
        else
          {:error, reason} -> {:reply, {:error, reason}, state}
        end
    end
  end

  # Private functions

  defp start_client(supervisor, config) do
    client_opts = ServerConfig.to_anubis_client_opts(config)
    client_reg_name = ServerConfig.client_name(config.name)

    case DynamicSupervisor.start_child(supervisor, {Anubis.Client, client_opts}) do
      {:ok, pid} ->
        client_label = config.name

        tools =
          try do
            case Anubis.Client.list_tools(client_reg_name, timeout: 30_000) do
              {:ok, %{result: %{"tools" => tool_list}}} -> tool_list
              {:ok, %{result: result}} when is_map(result) -> Map.get(result, "tools", [])
              _ -> []
            end
          rescue
            _ -> []
          catch
            _, _ -> []
          end

        Logger.info("Client #{client_label} started (PID: #{inspect(pid)}), #{length(tools)} tools")
        {:ok, pid, tools}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp start_and_register_client(state, config) do
    case start_client(state.dynamic_supervisor, config) do
      {:ok, pid, tools} ->
        client_state = %ClientState{
          server_name: config.name,
          config: config,
          pid: pid,
          status: :connected,
          tools: tools,
          connected_at: DateTime.utc_now()
        }

        new_state = %{state |
          configs: Map.put(state.configs, config.name, config),
          clients: Map.put(state.clients, config.name, client_state)
        }

        save_configs(new_state.configs, new_state.config_path)
        new_state

      {:error, reason} ->
        Logger.error("Failed to start client #{config.name}: #{inspect(reason)}")
        state
    end
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
        |> Enum.map(&ServerConfig.to_json/1)
    }

    File.mkdir_p!(Path.dirname(path))
    File.write!(path, Jason.encode!(data, pretty: true))
  end

  defp check_name_unique(name, configs) do
    if Map.has_key?(configs, name), do: {:error, :server_already_exists}, else: :ok
  end
end
