defmodule Exhub.Llm.LlmConfigServer do
  use GenServer

  # Define the default LLM name
  @default_llm_name "codestral/codestral-latest"

  # Configuration state keys
  @current_llm_key :current_llm
  @config Application.compile_env(:exhub, :llms)

  @type llm_name :: String.t()
  @type llm_config :: %{api_base: String.t(), api_key: String.t(), model: String.t()}
  @type config_state :: %{optional(atom()) => llm_name() | llm_config()}

  @doc "Start the GenServer with the configuration"
  def start_link(_) do
    GenServer.start_link(__MODULE__, @config, name: __MODULE__)
  end

  @doc "Retrieve the default LLM configuration"
  @spec get_default_llm_config() :: {:ok, llm_config()} | {:error, String.t()}
  def get_default_llm_config do
    GenServer.call(__MODULE__, :get_default_llm_config)
  end

  @doc "Set the default LLM name"
  @spec set_default_llm_name(llm_name()) :: :ok
  def set_default_llm_name(llm_name) do
    GenServer.cast(__MODULE__, {:set_default_llm_name, llm_name})
  end

  @doc "Retrieve the configuration for a specific LLM"
  @spec get_llm_config(llm_name()) :: {:ok, llm_config()} | {:error, String.t()}
  def get_llm_config(llm_name) do
    GenServer.call(__MODULE__, {:get_llm_config, llm_name})
  end

  @doc "Set the configuration for a specific LLM"
  @spec set_llm_config(llm_name(), llm_config()) :: :ok
  def set_llm_config(llm_name, config) do
    GenServer.cast(__MODULE__, {:set_llm_config, llm_name, config})
  end

  @doc "List all available LLM names (excluding current/default LLM)"
  @spec list_llm_names() :: list(llm_name())
  def list_llm_names do
    GenServer.call(__MODULE__, :list_llm_names)
  end

  @doc "Get the current default LLM name"
  @spec get_default_llm_name() :: llm_name()
  def get_default_llm_name do
    GenServer.call(__MODULE__, :get_default_llm_name)
  end

  # Initialize the GenServer with configuration validation
  @impl true
  def init(config) when is_map(config) do
    initial_state = config
      |> validate_config()
      |> Map.put(@current_llm_key, @default_llm_name)
      |> ensure_default_llm_exists()
    
    {:ok, initial_state}
  end

  # Handle listing LLM names
  @impl true
  def handle_call(:list_llm_names, _from, config) do
    llm_names = config
      |> Map.keys()
      |> Enum.reject(&is_config_key/1)
      |> Enum.sort()
    
    {:reply, llm_names, config}
  end

  # Handle getting default LLM name
  @impl true
  def handle_call(:get_default_llm_name, _from, config) do
    {:reply, config[@current_llm_key], config}
  end

  # Handle getting LLM configuration with validation
  @impl true
  def handle_call({:get_llm_config, llm_name}, _from, config) do
    response = case Map.get(config, llm_name) do
      nil -> {:error, "LLM '#{llm_name}' not found"}
      config -> {:ok, config}
    end
    
    {:reply, response, config}
  end

  # Handle getting default LLM configuration with validation
  @impl true
  def handle_call(:get_default_llm_config, _from, config) do
    default_name = config[@current_llm_key]
    response = case Map.get(config, default_name) do
      nil -> {:error, "Default LLM '#{default_name}' not found in configuration"}
      config -> {:ok, config}
    end
    
    {:reply, response, config}
  end

  # Handle setting LLM configuration with validation
  @impl true
  def handle_cast({:set_llm_config, llm_name, new_config}, config) do
    validated_config = validate_llm_config(new_config)
    new_state = Map.put(config, llm_name, validated_config)
    {:noreply, new_state}
  end

  # Handle setting default LLM name with validation
  @impl true
  def handle_cast({:set_default_llm_name, llm_name}, config) do
    if Map.has_key?(config, llm_name) do
      new_state = Map.put(config, @current_llm_key, llm_name)
      {:noreply, new_state}
    else
      # Keep current state if LLM doesn't exist
      {:noreply, config}
    end
  end

  # Private helper functions
  defp validate_config(config) when is_map(config), do: config
  defp validate_config(_), do: %{}

  defp validate_llm_config(config) do
    Map.take(config, [:api_base, :api_key, :model])
  end

  defp is_config_key(key) when is_atom(key), do: true
  defp is_config_key(_), do: false

  defp ensure_default_llm_exists(config) do
    default_name = config[@current_llm_key]
    if Map.has_key?(config, default_name) do
      config
    else
      # If default LLM doesn't exist, find first available LLM
      first_llm = config
        |> Map.keys()
        |> Enum.reject(&is_config_key/1)
        |> List.first()
      
      case first_llm do
        nil -> config  # No LLMs available
        name -> Map.put(config, @current_llm_key, name)
      end
    end
  end
end
