defmodule Exhub.Llm.Mcp.ClientManager do
  use GenServer
  alias Exhub.Llm.Mcp.Client
  require Logger

  @impl true
  def init(_) do
    {:ok, %{registry: %{}}}
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def new(client_name, server_name) do
    GenServer.call(__MODULE__, {:build, client_name, server_name})
  end

  def kill(client_name) do
    GenServer.call(__MODULE__, {:kill, client_name})
  end

  def get_all_info do
    GenServer.call(__MODULE__, :get_all_info)
  end

  def get_info(client_name) do
    GenServer.call(__MODULE__, {:get_info, client_name})
  end

  @impl true
  def handle_call({:build, client_name, server_name}, _from, state) do
    case Map.get(state.registry, client_name) do
      nil ->
        {:ok, client} =
          Client.start_link(
            name: client_name,
            transport: server_name,
            client_info: %{
              "name" => "#{client_name}",
              "version" => "1.0.0"
            },
            capabilities: %{
              "roots" => %{
                "listChanged" => true
              },
              "sampling" => %{}
            }
          )

        Logger.info("Client started on PID #{inspect(client)}")
        new_registry = Map.put(state.registry, client_name, %{client: client, server_name: server_name, client_name: client_name})
        {:reply, {:ok, client}, %{state | registry: new_registry}}

      %{client: client} ->
        {:reply, {:ok, client}, state}
    end
  end

  @impl true
  def handle_call({:kill, client_name}, _from, state) do
    case Map.get(state.registry, client_name) do
      nil -> {:reply, :not_found, state}
      %{client: client} ->
        Process.exit(client, :kill)
        new_registry = Map.delete(state.registry, client_name)
        {:reply, :ok, %{state | registry: new_registry}}
    end
  end

  @impl true
  def handle_call(:get_all_info, _from, state) do
    {:reply, state.registry, state}
  end

  @impl true
  def handle_call({:get_info, client_name}, _from, state) do
    client = Map.get(state.registry, client_name)
    {:reply, client, state}
  end
end
