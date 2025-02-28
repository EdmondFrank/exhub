defmodule Exhub.Llm.Mcp.ServerManager do
  use GenServer

  alias Hermes.Transport.STDIO

  # Genserver callbacks
  @impl true
  def init(_) do
    {:ok, %{registry: %{}}}
  end

  def start_link(_) do
    GenServer.start_link(__MODULE__, [], name: __MODULE__)
  end

  def new(name, command, args) do
    GenServer.call(__MODULE__, {:new, name, command, args})
  end

  def get_info(name) do
    GenServer.call(__MODULE__, {:get_info, name})
  end

  def kill(name) do
    GenServer.call(__MODULE__, {:kill, name})
  end

  def get_all_info() do
    GenServer.call(__MODULE__, :get_all_info)
  end

  # Genserver API

  @impl true
  def handle_call({:new, name, command, args}, _from, state) do
    case Map.get(state.registry, name) do
      nil ->
        {:ok, pid} = STDIO.start_link(command: command, args: args, client: name)
        new_registry = Map.put(state.registry, name, pid)
        {:reply, {:ok, pid}, %{state | registry: new_registry}}

      pid ->
        {:reply, {:ok, pid}, state}
    end
  end

  @impl true
  def handle_call({:get_info, name}, _from, state) do
    pid = Map.get(state.registry, name)
    {:reply, {name, pid}, state}
  end

  @impl true
  def handle_call({:kill, name}, _from, state) do
    pid = Map.get(state.registry, name)
    case pid do
      nil -> {:reply, :not_found, state}
      _ ->
        Process.exit(pid, :kill)
        new_registry = Map.delete(state.registry, name)
        {:reply, :ok, %{state | registry: new_registry}}
    end
  end

  @impl true
  def handle_call(:get_all_info, _from, state) do
    {:reply, state.registry, state}
  end
end
