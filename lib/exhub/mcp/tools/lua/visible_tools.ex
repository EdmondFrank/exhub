defmodule Exhub.MCP.Tools.Lua.VisibleTools do
  @moduledoc """
  The tool set the `lua` server exposes — shared by `code_mode` and `help`.

  Starts from every tool the hub knows (`ClientManager.list_all_tools/0`), drops
  the meta hub and the `lua` server itself (`config :exhub, :lua, :exclude_servers`),
  then applies the request's `x-include-tools` / `x-exclude-tools` headers. Both
  tools resolve visibility here, so `help` can never document a tool the sandbox
  cannot call (or omit one it can).
  """

  alias Exhub.Lua.Engine
  alias Exhub.MCP.Hub.ClientManager

  @doc """
  Tools visible to a request, given its Anubis `frame`. Returns `[]` if the hub
  answers with anything unexpected.
  """
  @spec list(map() | nil) :: [map()]
  def list(frame) do
    headers = frame_headers(frame)
    excluded = Engine.config()[:exclude_servers] || []

    case ClientManager.list_all_tools() do
      {:ok, tools} when is_list(tools) ->
        tools
        |> Enum.reject(&(Map.get(&1, "server") in excluded))
        |> apply_header_filter(headers)

      _ ->
        []
    end
  end

  defp frame_headers(%{context: %{headers: headers}}) when is_map(headers), do: headers
  defp frame_headers(_frame), do: %{}

  # `x-include-tools` / `x-exclude-tools` may name a tool either by its bare
  # upstream name or by the `server__tool` form — match on both.
  defp apply_header_filter(tools, headers) do
    include = parse_tool_list(headers["x-include-tools"])
    exclude = parse_tool_list(headers["x-exclude-tools"])

    tools
    |> maybe_include(include)
    |> maybe_exclude(exclude)
  end

  defp parse_tool_list(value) when is_binary(value) do
    value
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp parse_tool_list(_value), do: []

  defp maybe_include(tools, []), do: tools

  defp maybe_include(tools, include) do
    allowed = MapSet.new(include)
    Enum.filter(tools, &matches?(&1, allowed))
  end

  defp maybe_exclude(tools, []), do: tools

  defp maybe_exclude(tools, exclude) do
    denied = MapSet.new(exclude)
    Enum.reject(tools, &matches?(&1, denied))
  end

  defp matches?(tool, set) do
    server = Map.get(tool, "server", "")
    name = Map.get(tool, "name", "")
    MapSet.member?(set, name) or MapSet.member?(set, "#{server}__#{name}")
  end
end
