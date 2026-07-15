defmodule Exhub.MCP.Hub.BuiltInRegistry do
  @moduledoc """
  Registry for built-in MCP servers that run in the same BEAM VM.

  Maps built-in server names to their server modules, enabling direct tool
  discovery and execution without HTTP overhead.

  Built-in servers are Anubis.Server modules mounted via Exhub.MCP.LazyPlug
  in the router. This registry bypasses the Anubis.Client → HTTP → Anubis.Server
  indirection for local servers.

  ## Usage

      # List all tools from built-in servers
      tools = Exhub.MCP.Hub.BuiltInRegistry.list_all_tools()

      # Call a tool directly
      {:ok, result} = Exhub.MCP.Hub.BuiltInRegistry.call_tool("habit", "read_habits", %{"key" => "user_id"})
  """

  require Logger

  # Built-in server modules mapped by their route name
  @built_in_servers %{
    "habit" => Exhub.MCP.HabitServer,
    "time" => Exhub.MCP.TimeServer,
    "think" => Exhub.MCP.ThinkServer,
    "web-tools" => Exhub.MCP.WebToolsServer,
    "archery" => Exhub.MCP.ArcheryServer,
    "browser-use" => Exhub.MCP.BrowserUseServer,
    "image-gen" => Exhub.MCP.ImageGenServer,
    "doc-extract" => Exhub.MCP.DocExtractServer,
    "look" => Exhub.MCP.LookServer,
    "todo" => Exhub.MCP.TodoServer,
    "desktop" => Exhub.MCP.DesktopServer,
    "agent" => Exhub.MCP.AgentServer,
    "brain" => Exhub.MCP.BrainServer,
    "exhub" => Exhub.MCP.ExhubServer,
    "mac-use" => Exhub.MCP.MacUseServer,
    "emacs" => Exhub.MCP.EmacsServer,
    "listen" => Exhub.MCP.ListenServer,
    "agent-hub" => Exhub.Sagents.AgentHubServer
  }

  @doc """
  Returns a list of all built-in server names.
  """
  @spec list_servers() :: [String.t()]
  def list_servers, do: Map.keys(@built_in_servers)

  @doc """
  Returns the server module for a given built-in server name.
  """
  @spec server_module(String.t()) :: module() | nil
  def server_module(name) do
    Map.get(@built_in_servers, name)
  end

  @doc """
  Returns the server info map for a built-in server.

  Delegates to the server module's `server_info/0` callback (auto-generated
  by `use Anubis.Server`). Falls back to a sensible default if the module
  does not implement the callback.

  ## Example

      iex> Exhub.MCP.Hub.BuiltInRegistry.server_info("desktop")
      %{"name" => "exhub-desktop-server", "version" => "1.1.0"}
  """
  @spec server_info(String.t()) :: map()
  def server_info(name) do
    case Map.get(@built_in_servers, name) do
      nil ->
        %{"name" => name, "version" => "1.0.0"}

      server_module ->
        try do
          server_module.server_info()
        rescue
          _ -> %{"name" => name, "version" => "1.0.0"}
        catch
          _, _ -> %{"name" => name, "version" => "1.0.0"}
        end
    end
  end

  @doc """
  Returns the server capabilities map for a built-in server.

  Delegates to the server module's `server_capabilities/0` callback (auto-generated
  by `use Anubis.Server`). Falls back to `%{}` if the module does not implement
  the callback.

  ## Example

      iex> Exhub.MCP.Hub.BuiltInRegistry.server_capabilities("desktop")
      %{"tools" => %{}}
  """
  @spec server_capabilities(String.t()) :: map()
  def server_capabilities(name) do
    case Map.get(@built_in_servers, name) do
      nil ->
        %{}

      server_module ->
        try do
          server_module.server_capabilities()
        rescue
          _ -> %{}
        catch
          _, _ -> %{}
        end
    end
  end

  @doc """
  Checks if a server name is a built-in server.
  """
  @spec built_in?(String.t()) :: boolean()
  def built_in?(name) do
    Map.has_key?(@built_in_servers, name)
  end

  @doc """
  Lists all tools from all built-in servers.

  Returns a list of tool maps with the MCP-compatible format:
  - `"name"` — the tool name
  - `"description""` — the tool description
  - `"inputSchema"` — the JSON schema for tool inputs
  - `"server"` — the server name this tool belongs to
  """
  @spec list_all_tools() :: [map()]
  def list_all_tools do
    @built_in_servers
    |> Enum.flat_map(fn {server_name, server_module} ->
      list_tools_for_server(server_name, server_module)
    end)
  end

  @doc """
  Lists all tools from a specific built-in server.
  """
  @spec list_tools(String.t()) :: [map()]
  def list_tools(server_name) do
    case Map.get(@built_in_servers, server_name) do
      nil -> []
      server_module -> list_tools_for_server(server_name, server_module)
    end
  end

  @doc """
  Calls a tool on a built-in server directly.

  ## Parameters
    * `server_name` — the built-in server name (e.g., "habit")
    * `tool_name` — the tool name (e.g., "read_habits")
    * `arguments` — a map of arguments to pass to the tool

  ## Returns
    * `{:ok, result}` — the tool executed successfully
    * `{:error, reason}` — the tool failed or was not found
  """
  @spec call_tool(String.t(), String.t(), map()) ::
          {:ok, term()} | {:error, term()}
  def call_tool(server_name, tool_name, arguments) do
    with {:ok, server_module} <- fetch_server_module(server_name),
         {:ok, tool} <- fetch_tool(server_module, tool_name) do
      execute_tool(tool, arguments)
    end
  end

  # --- Private Functions ---

  defp list_tools_for_server(server_name, server_module) do
    server_module.__components__(:tool)
    |> Enum.map(fn tool ->
      %{
        "name" => tool.name,
        "description" => tool.description,
        "inputSchema" => tool.input_schema,
        "server" => server_name
      }
    end)
  rescue
    error ->
      Logger.warning(
        "[BuiltInRegistry] Failed to list tools for #{server_name}: #{inspect(error)}"
      )

      []
  end

  defp fetch_server_module(server_name) do
    case Map.get(@built_in_servers, server_name) do
      nil -> {:error, :server_not_found}
      module -> {:ok, module}
    end
  end

  defp fetch_tool(server_module, tool_name) do
    tool =
      server_module.__components__(:tool)
      |> Enum.find(&(&1.name == tool_name))

    case tool do
      nil -> {:error, :tool_not_found}
      tool -> {:ok, tool}
    end
  end

  defp execute_tool(%{handler: handler, validate_input: validate}, arguments) do
    # Validate and transform input using the tool's Peri schema
    with {:ok, validated} <- validate_tool_input(validate, arguments) do
      # Create a minimal frame for the tool execution
      frame = %Anubis.Server.Frame{}

      case handler.execute(validated, frame) do
        {:reply, %Anubis.Server.Response{} = response, _frame} ->
          {:ok, Anubis.Server.Response.to_protocol(response)}

        {:reply, result, _frame} when is_map(result) ->
          {:ok, result}

        {:error, %Anubis.MCP.Error{} = error, _frame} ->
          {:error, error}

        {:error, reason, _frame} ->
          {:error, reason}

        other ->
          Logger.warning("[BuiltInRegistry] Unexpected tool response: #{inspect(other)}")
          {:ok, other}
      end
    end
  rescue
    error ->
      Logger.error("[BuiltInRegistry] Tool execution failed: #{inspect(error)}")
      {:error, inspect(error)}
  end

  defp validate_tool_input(nil, arguments), do: {:ok, arguments}

  defp validate_tool_input(validate_fn, arguments) when is_function(validate_fn, 1) do
    case validate_fn.(arguments) do
      {:ok, validated} -> {:ok, validated}
      {:error, errors} -> {:error, %{message: "Validation failed", errors: errors}}
    end
  end
end
