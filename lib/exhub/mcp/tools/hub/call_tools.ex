defmodule Exhub.MCP.Tools.Hub.CallTools do
  @moduledoc """
  MCP Tool: call_tools

  Execute a tool on a specific upstream MCP server.
  Use this to call any tool from any connected server through the hub.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "call_tools"

  @impl true
  def description do
    """
    Execute a tool on a specific upstream MCP server. Use this to call any tool from any connected server through the hub.
    """
  end

  schema do
    field :server_name, :string, description: "Name of the upstream server to call the tool on", required: true
    field :tool_name, :string, description: "Name of the tool to execute (without server prefix)", required: true
    field :arguments, {:either, [:map, :string]}, description: "Arguments to pass to the tool (map or JSON string)", default: %{}
  end

  @impl true
  def execute(params, frame) do
    server_name = Map.get(params, :server_name, "")
    tool_name = Map.get(params, :tool_name, "")
    arguments = parse_arguments(Map.get(params, :arguments, %{}))

    require Logger
    Logger.info("[MCP Hub] call_tools: #{server_name}:#{tool_name}")

    case Exhub.MCP.Hub.ClientManager.call_tool(server_name, tool_name, arguments) do
      {:ok, result} ->
        resp = Response.tool() |> Response.structured(%{result: result})
        {:reply, resp, frame}

      {:error, reason} ->
        case strip_server_prefix(tool_name) do
          {:ok, bare_name} ->
            Logger.info("[MCP Hub] Retrying call_tools with stripped prefix: #{bare_name}")
            case Exhub.MCP.Hub.ClientManager.call_tool(server_name, bare_name, arguments) do
              {:ok, result} ->
                resp = Response.tool() |> Response.structured(%{result: result})
                {:reply, resp, frame}

              {:error, retry_reason} ->
                error_msg = "Tool call failed on #{server_name}: #{inspect(retry_reason)}"
                Logger.error(error_msg)
                resp = Response.tool() |> Response.error(error_msg)
                {:reply, resp, frame}
            end

          :error ->
            error_msg = "Tool call failed on #{server_name}: #{inspect(reason)}"
            Logger.error(error_msg)
            resp = Response.tool() |> Response.error(error_msg)
            {:reply, resp, frame}
        end
    end
  end

  defp strip_server_prefix(tool_name) when is_binary(tool_name) do
    case String.split(tool_name, "__", parts: 2) do
      [_prefix, bare_name] when bare_name != "" -> {:ok, bare_name}
      _ -> :error
    end
  end

  defp strip_server_prefix(_), do: :error

  defp parse_arguments(args) when is_map(args), do: args
  defp parse_arguments(args) when is_binary(args) do
    case Jason.decode(args) do
      {:ok, decoded} when is_map(decoded) -> decoded
      _ -> %{}
    end
  end

  defp parse_arguments(_), do: %{}
end
