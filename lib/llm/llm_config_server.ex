defmodule Exhub.Llm.LlmConfigServer do
  use GenServer

  # Define the default LLM name
  @default_llm_name "codestral/codestral-latest"

  # Fetch the LLM configurations from the application environment
  @config Application.compile_env(:exhub, :llms)

  # Start the GenServer with the configuration
  def start_link(_) do
    GenServer.start_link(__MODULE__, @config, name: __MODULE__)
  end

  # Retrieve the default LLM configuration
  def get_default_llm_config do
    GenServer.call(__MODULE__, :get_default_llm_config)
  end

  # Set the default LLM name
  def set_default_llm_name(llm_name) do
    GenServer.cast(__MODULE__, {:set_default_llm_name, llm_name})
  end

  # Retrieve the configuration for a specific LLM
  def get_llm_config(llm_name) do
    GenServer.call(__MODULE__, {:get_llm_config, llm_name})
  end

  # Set the configuration for a specific LLM
  def set_llm_config(llm_name, config) do
    GenServer.cast(__MODULE__, {:set_llm_config, llm_name, config})
  end

  def list_llm_names do
    GenServer.call(__MODULE__, :list_llm_names)
  end

  def get_default_llm_name do
    GenServer.call(__MODULE__, :get_default_llm_name)
  end

  # Initialize the GenServer with the configuration and set the default LLM name
  def init(config) do
    # Set the default LLM name in the configuration map
    config = Map.put(config, :current_llm, @default_llm_name)
    {:ok, config}
  end

  def handle_call(:list_llm_names, _from, config) do
    llm_names = Map.keys(config) |> Enum.reject(&(&1 == config[:current_llm] || &1 == :current_llm))
    {:reply, llm_names, config}
  end

  def handle_call(:get_default_llm_name, _from, config) do
    {:reply, config[:current_llm], config}
  end

  # Handle the call to retrieve the configuration for a specific LLM
  def handle_call({:get_llm_config, llm_name}, _from, config) do
    # Fetch the configuration for the specified LLM from the state map
    {:reply, Map.fetch(config, llm_name), config}
  end

  # Handle the call to retrieve the default LLM configuration
  def handle_call(:get_default_llm_config, _from, config) do
    {:reply, Map.fetch(config, config[:current_llm]), config}
  end

  # Handle the cast to set the configuration for a specific LLM
  def handle_cast({:set_llm_config, llm_name, config}, state) do
    # Update the state with the new LLM configuration
    new_state = Map.put(state, llm_name, config)
    {:noreply, new_state}
  end

  # Handle the cast to set the default LLM name
  def handle_cast({:set_default_llm_name, llm_name}, state) do
    # Update the state with the new default LLM name
    new_state = Map.put(state, :current_llm, llm_name)
    {:noreply, new_state}
  end
end
