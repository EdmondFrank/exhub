defmodule Exhub.Client do
  alias Exhub.Protocol
  alias Exhub.Transport
  alias Exhub.Errors

  @moduledoc """
  Client represents an MCP client that can connect to and interact with MCP servers.
  """

  defstruct transport: nil, protocol: nil, capabilities: nil, initialized: false, pid: nil

  @doc """
  Creates a new MCP client with the specified transport.
  """
  def start_link(transport) do
    GenServer.start_link(__MODULE__, transport, name: __MODULE__)
  end

  def init(transport) do
    protocol = Protocol.new(nil)
    {:ok, %__MODULE__{
      transport: transport,
      protocol: protocol,
      capabilities: nil,
      initialized: false,
      pid: self()
    }}
  end

  @doc """
  Connects to the server and retrieves its capabilities.
  """
  def initialize(client, _ctx) do
    GenServer.cast(client.pid, :initialize)
  end

  @doc """
  Retrieves the list of available tools from the server.
  """
  def list_tools(client, _ctx, cursor) do
    GenServer.call(client.pid, {:list_tools, cursor})
  end

  @doc """
  Calls a specific tool on the server with the provided arguments.
  """
  def call_tool(client, _ctx, name, arguments) do
    GenServer.call(client.pid, {:call_tool, name, arguments})
  end

  @doc """
  Retrieves the list of available prompts from the server.
  """
  def list_prompts(client, _ctx, cursor) do
    GenServer.call(client.pid, {:list_prompts, cursor})
  end

  @doc """
  Retrieves a specific prompt from the server.
  """
  def get_prompt(client, _ctx, name, arguments) do
    GenServer.call(client.pid, {:get_prompt, name, arguments})
  end

  @doc """
  Retrieves the list of available resources from the server.
  """
  def list_resources(client, _ctx, cursor) do
    GenServer.call(client.pid, {:list_resources, cursor})
  end

  @doc """
  Reads a specific resource from the server.
  """
  def read_resource(client, _ctx, uri) do
    GenServer.call(client.pid, {:read_resource, uri})
  end

  @doc """
  Sends a ping request to the server to check connectivity.
  """
  def ping(client, _ctx) do
    GenServer.call(client.pid, :ping)
  end

  @doc """
  Returns the server capabilities obtained during initialization.
  """
  def get_capabilities(client) do
    GenServer.call(client.pid, :get_capabilities)
  end

  def handle_cast(:initialize, %__MODULE__{initialized: true} = client) do
    {:noreply, client}
  end

  def handle_cast(:initialize, %__MODULE__{initialized: false} = client) do
    case Protocol.connect(client.protocol, client.transport) do
      :ok ->
        response = Protocol.request(client.protocol, "initialize", %{}, nil)
        case response do
          {:ok, response_bytes} ->
            case Jason.decode(response_bytes) do
              {:ok, init_result} ->
                client = %__MODULE__{
                  capabilities: init_result["capabilities"],
                  initialized: true
                }
                {:noreply, client}
              {:error, _reason} ->
                {:noreply, client}
            end
          {:error, _reason} ->
            {:noreply, client}
        end
      {:error, _reason} ->
        {:noreply, client}
    end
  end

  def handle_call({:list_tools, _cursor}, %__MODULE__{initialized: false} = client) do
    {:reply, {:error, "client not initialized"}, client}
  end

  def handle_call({:list_tools, cursor}, %__MODULE__{initialized: true} = client) do
    params = %{"cursor" => cursor}
    response = Protocol.request(client.protocol, "tools/list", params, nil)
    case response do
      {:ok, response_bytes} ->
        case Jason.decode(response_bytes) do
          {:ok, tools_response} ->
            {:reply, {:ok, tools_response}, client}
          {:error, reason} ->
            {:reply, {:error, "failed to unmarshal tools response: #{reason}"}, client}
        end
      {:error, reason} ->
        {:reply, {:error, "failed to list tools: #{reason}"}, client}
    end
  end

  def handle_call({:call_tool, _name, _arguments}, %__MODULE__{initialized: false} = client) do
    {:reply, {:error, "client not initialized"}, client}
  end

  def handle_call({:call_tool, name, arguments}, %__MODULE__{initialized: true} = client) do
    case Jason.encode(arguments) do
      {:ok, arguments_json} ->
        params = %{"name" => name, "arguments" => arguments_json}
        response = Protocol.request(client.protocol, "tools/call", params, nil)
        case response do
          {:ok, response_bytes} ->
            case Jason.decode(response_bytes) do
              {:ok, tool_response} ->
                {:reply, {:ok, tool_response}, client}
              {:error, reason} ->
                {:reply, {:error, "failed to unmarshal tool response: #{reason}"}, client}
            end
          {:error, reason} ->
            {:reply, {:error, "failed to call tool: #{reason}"}, client}
        end
      {:error, reason} ->
        {:reply, {:error, "failed to marshal arguments: #{reason}"}, client}
    end
  end

  def handle_call({:list_prompts, _cursor}, %__MODULE__{initialized: false} = client) do
    {:reply, {:error, "client not initialized"}, client}
  end

  def handle_call({:list_prompts, cursor}, %__MODULE__{initialized: true} = client) do
    params = %{"cursor" => cursor}
    response = Protocol.request(client.protocol, "prompts/list", params, nil)
    case response do
      {:ok, response_bytes} ->
        case Jason.decode(response_bytes) do
          {:ok, prompts_response} ->
            {:reply, {:ok, prompts_response}, client}
          {:error, reason} ->
            {:reply, {:error, "failed to unmarshal prompts response: #{reason}"}, client}
        end
      {:error, reason} ->
        {:reply, {:error, "failed to list prompts: #{reason}"}, client}
    end
  end

  def handle_call({:get_prompt, _name, _arguments}, %__MODULE__{initialized: false} = client) do
    {:reply, {:error, "client not initialized"}, client}
  end

  def handle_call({:get_prompt, name, arguments}, %__MODULE__{initialized: true} = client) do
    case Jason.encode(arguments) do
      {:ok, arguments_json} ->
        params = %{"name" => name, "arguments" => arguments_json}
        response = Protocol.request(client.protocol, "prompts/get", params, nil)
        case response do
          {:ok, response_bytes} ->
            case Jason.decode(response_bytes) do
              {:ok, prompt_response} ->
                {:reply, {:ok, prompt_response}, client}
              {:error, reason} ->
                {:reply, {:error, "failed to unmarshal prompt response: #{reason}"}, client}
            end
          {:error, reason} ->
            {:reply, {:error, "failed to get prompt: #{reason}"}, client}
        end
      {:error, reason} ->
        {:reply, {:error, "failed to marshal arguments: #{reason}"}, client}
    end
  end

  def handle_call({:list_resources, _cursor}, %__MODULE__{initialized: false} = client) do
    {:reply, {:error, "client not initialized"}, client}
  end

  def handle_call({:list_resources, cursor}, %__MODULE__{initialized: true} = client) do
    params = %{"cursor" => cursor}
    response = Protocol.request(client.protocol, "resources/list", params, nil)
    case response do
      {:ok, response_bytes} ->
        case Jason.decode(response_bytes) do
          {:ok, resources_response} ->
            {:reply, {:ok, resources_response}, client}
          {:error, reason} ->
            {:reply, {:error, "failed to unmarshal resources response: #{reason}"}, client}
        end
      {:error, reason} ->
        {:reply, {:error, "failed to list resources: #{reason}"}, client}
    end
  end

  def handle_call(:get_capabilities, %__MODULE__{capabilities: capabilities} = client) do
    {:reply, capabilities, client}
  end

  def handle_call(:ping, %__MODULE__{initialized: false} = client) do
    {:reply, {:error, "client not initialized"}, client}
  end

  def handle_call(:ping, %__MODULE__{initialized: true} = client) do
    case Protocol.request(client.protocol, "ping", nil, nil) do
      {:ok, _response} ->
        {:reply, :ok, client}
      {:error, reason} ->
        {:reply, {:error, "failed to ping server: #{reason}"}, client}
    end
  end

  def handle_call({:read_resource, _uri}, %__MODULE__{initialized: false} = client) do
    {:reply, {:error, "client not initialized"}, client}
  end

  def handle_call({:read_resource, uri}, %__MODULE__{initialized: true} = client) do
    params = %{"uri" => uri}
    response = Protocol.request(client.protocol, "resources/read", params, nil)
    case response do
      {:ok, response_bytes} ->
        case Jason.decode(response_bytes) do
          {:ok, resource_response} ->
            if resource_response["error"] do
              {:reply, {:error, resource_response["error"]}, client}
            else
              {:reply, {:ok, resource_response["response"]}, client}
            end
          {:error, reason} ->
            {:reply, {:error, "failed to unmarshal resource response: #{reason}"}, client}
        end
      {:error, reason} ->
        {:reply, {:error, "failed to read resource: #{reason}"}, client}
    end
  end

end
