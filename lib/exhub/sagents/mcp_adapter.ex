defmodule Exhub.Sagents.McpAdapter do
  @moduledoc """
  Bridges exhub MCP tools into LangChain.Function structs for sagents agents.

  Converts selected MCP tool groups (e.g., [:desktop, :web-tools]) into
  `LangChain.Function` structs that agents can call during execution.
  """

  alias Exhub.MCP.Hub.BuiltInRegistry
  require Logger

  @doc """
  Builds LangChain.Function tools from a list of MCP tool group atoms.

  ## Parameters
    - `tool_groups` — list of atoms like `[:desktop, :web-tools, :brain]`

  ## Returns
    - List of `LangChain.Function` structs
  """
  @spec build_tools([atom()]) :: [LangChain.Function.t()]
  def build_tools(tool_groups) when is_list(tool_groups) do
    tool_groups
    |> Enum.flat_map(&build_tools_for_group/1)
  end

  defp build_tools_for_group(group) do
    server_name = Atom.to_string(group)

    case BuiltInRegistry.list_tools(server_name) do
      [] ->
        Logger.warning("[McpAdapter] No tools found for group: #{group}")
        []

      tools ->
        Enum.map(tools, &build_langchain_function(&1, server_name))
    end
  end

  defp build_langchain_function(tool_map, server_name) do
    tool_name = Map.get(tool_map, "name")
    description = Map.get(tool_map, "description", "")
    input_schema = Map.get(tool_map, "inputSchema", %{})
    namespaced_name = "#{server_name}__#{tool_name}"

    # MCP gives us a JSON Schema; hand it to LangChain unchanged. Building
    # `parameters:` out of `%FunctionParam{}` structs would flatten nested
    # objects and arrays, and langchain 0.9 rejects plain maps there.
    LangChain.Function.new!(%{
      name: namespaced_name,
      description: "[#{server_name}] #{description}",
      parameters_schema: parameters_schema(input_schema),
      function: fn args, _context ->
        case BuiltInRegistry.call_tool(server_name, tool_name, args) do
          {:ok, result} ->
            case Jason.encode(result) do
              {:ok, json} -> json
              _ -> inspect(result)
            end

          {:error, reason} ->
            Logger.error("[McpAdapter] Tool call failed: #{namespaced_name}: #{inspect(reason)}")
            "Error: #{inspect(reason)}"
        end
      end
    })
  end

  # Accept the MCP `inputSchema` as-is when it is an object schema; otherwise
  # fall back to a no-argument schema so the tool stays callable.
  defp parameters_schema(%{"type" => "object", "properties" => properties} = schema)
       when is_map(properties),
       do: schema

  defp parameters_schema(%{"properties" => properties} = schema) when is_map(properties),
    do: Map.put(schema, "type", "object")

  defp parameters_schema(_schema), do: %{"type" => "object", "properties" => %{}}
end
