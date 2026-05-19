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

    parameters = extract_parameters(input_schema)

    LangChain.Function.new!(%{
      name: namespaced_name,
      description: "[#{server_name}] #{description}",
      parameters: parameters,
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

  defp extract_parameters(%{"properties" => properties} = schema) do
    required = Map.get(schema, "required", [])

    properties
    |> Enum.map(fn {name, prop} ->
      %{
        name: name,
        type: map_json_type(prop),
        description: Map.get(prop, "description", ""),
        required: name in required
      }
    end)
  end

  defp extract_parameters(_), do: []

  defp map_json_type(%{"type" => "string"}), do: :string
  defp map_json_type(%{"type" => "integer"}), do: :integer
  defp map_json_type(%{"type" => "number"}), do: :number
  defp map_json_type(%{"type" => "boolean"}), do: :boolean
  defp map_json_type(%{"type" => "array"}), do: :array
  defp map_json_type(%{"type" => "object"}), do: :object
  defp map_json_type(_), do: :string
end
