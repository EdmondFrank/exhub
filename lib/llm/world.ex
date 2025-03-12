defmodule Exhub.Llm.World do
  use GenServer

  # Client API

  @doc """
  Starts the World GenServer.
  """
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  @doc """
  Creates an agent in the World.
  """
  def create_agent(agent_name, agent_module, opts) do
    GenServer.call(__MODULE__, {:create_agent, agent_name, agent_module, opts}, :infinity)
  end

  @doc """
  Stops an agent in the World.
  """
  def stop_agent(agent_name) do
    GenServer.call(__MODULE__, {:stop_agent, agent_name}, :infinity)
  end

  @doc """
  Sends a message to an agent in the World.
  """
  def send_message(agent_name, message) do
    GenServer.call(__MODULE__, {:send_message, agent_name, message}, :infinity)
  end

  @doc """
  Lists all agents in the World.
  """
  def list_agents() do
    GenServer.call(__MODULE__, :list_agents, :infinity)
  end

  @impl true
  def init(_) do
    # Create network and agents
    {:ok, network} = SwarmEx.create_network()

    {:ok, %{network: network, agents: %{}}}
  end

  @impl true
  def handle_call({:create_agent, agent_name, agent_module, opts}, _from, state) do
    {:ok, agent} = SwarmEx.create_agent(state.network, agent_module, opts)
    new_agents = Map.put(state.agents, agent_name, agent)
    {:reply, agent, %{state | agents: new_agents}}
  end

  @impl true
  def handle_call({:stop_agent, agent_name}, _from, state) do
    agent = Map.get(state.agents, agent_name)
    :ok = SwarmEx.stop_agent(agent)
    new_agents = Map.delete(state.agents, agent_name)
    {:reply, :ok, %{state | agents: new_agents}}
  end

  @impl true
  def handle_call({:send_message, agent_name, message}, _from, state) do
    response = SwarmEx.send_message(state.network, agent_name, message)
    {:reply, response, state}
  end

  @impl true
  def handle_call(:list_agents, _from, state) do
    agents = SwarmEx.Client.list_agents(state.network)
    {:reply, agents, state}
  end
end
