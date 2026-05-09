defmodule Exhub.MCP.ServerHelpers do
  @moduledoc """
  Shared helpers for Exhub MCP servers.

  Provides `filter_tools_by_headers/2` which filters a list of tools based on
  the `x-include-tools` and `x-exclude-tools` headers.

  ## Headers

  - `x-include-tools` – Comma-separated list of tool names to include.
    Only these tools will be returned. If omitted, all tools are considered.
  - `x-exclude-tools` – Comma-separated list of tool names to exclude.
    Applied after the include filter.

  Both headers are case-insensitive and values are trimmed.
  """

  alias Anubis.Server.Frame

  @doc """
  Filters a list of tools based on the `x-include-tools` and `x-exclude-tools`
  headers found in the request context.

  ## Examples

      iex> tools = [%{name: "read_file"}, %{name: "write_file"}]
      iex> headers = %{"x-include-tools" => "read_file"}
      iex> Exhub.MCP.ServerHelpers.filter_tools_by_headers(tools, headers)
      [%{name: "read_file"}]

      iex> tools = [%{name: "read_file"}, %{name: "write_file"}]
      iex> headers = %{"x-exclude-tools" => "write_file"}
      iex> Exhub.MCP.ServerHelpers.filter_tools_by_headers(tools, headers)
      [%{name: "read_file"}]
  """
  @spec filter_tools_by_headers([map()], %{String.t() => String.t()}) :: [map()]
  def filter_tools_by_headers(tools, headers) when is_list(tools) do
    include = parse_tool_list(headers["x-include-tools"])
    exclude = parse_tool_list(headers["x-exclude-tools"])

    tools
    |> maybe_include(include)
    |> maybe_exclude(exclude)
  end

  @doc """
  Wraps the default Anubis request handler to filter `tools/list` responses
  based on the `x-include-tools` / `x-exclude-tools` headers.
  """
  @spec handle_request_with_filtered_tools(module(), map(), Frame.t()) ::
          {:reply, map(), Frame.t()}
          | {:noreply, Frame.t()}
          | {:error, term(), Frame.t()}
  def handle_request_with_filtered_tools(server_module, request, frame) do
    case request do
      %{"method" => "tools/list"} ->
        # Delegate to the default handler first
        case Anubis.Server.Handlers.handle(request, server_module, frame) do
          {:reply, %{"tools" => tools} = response, new_frame} ->
            headers = new_frame.context.headers
            filtered_tools = filter_tools_by_headers(tools, headers)
            {:reply, %{response | "tools" => filtered_tools}, new_frame}

          other ->
            other
        end

      _ ->
        Anubis.Server.Handlers.handle(request, server_module, frame)
    end
  end

  # ------------------------------------------------------------------
  # Private helpers
  # ------------------------------------------------------------------

  defp parse_tool_list(nil), do: []
  defp parse_tool_list(""), do: []

  defp parse_tool_list(value) when is_binary(value) do
    value
    |> String.split(",")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp maybe_include(tools, []), do: tools

  defp maybe_include(tools, include) do
    include_set = MapSet.new(include)
    Enum.filter(tools, &MapSet.member?(include_set, tool_name(&1)))
  end

  defp maybe_exclude(tools, []), do: tools

  defp maybe_exclude(tools, exclude) do
    exclude_set = MapSet.new(exclude)
    Enum.reject(tools, &MapSet.member?(exclude_set, tool_name(&1)))
  end

  defp tool_name(%{name: name}), do: name
  defp tool_name(%{"name" => name}), do: name
end
