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

  @project_dir_header "x-project-dir"

  @doc """
  Returns the caller's project directory, taken from the `x-project-dir` header.

  One long-lived MCP server is shared by many clients — and by a single client
  across many repositories — so the server's own cwd is *not* the caller's.
  Clients that know the repository root send `x-project-dir` on every request;
  `Exhub.MCP.Desktop.Helpers.resolve_path/2` then resolves repository-relative
  paths against it and the process tools use it as the default `working_dir`.

  Returns `:error` when the header is absent, blank, not absolute, or does not
  name an existing directory: the server must never guess a directory.
  """
  @spec request_cwd(Frame.t() | map() | nil) :: {:ok, String.t()} | :error
  def request_cwd(frame) do
    with headers when is_map(headers) <- request_headers(frame),
         value when is_binary(value) <- header_value(headers, @project_dir_header),
         true <- value != "",
         true <- Path.type(value) == :absolute,
         true <- File.dir?(value) do
      {:ok, Path.expand(value)}
    else
      _ -> :error
    end
  end

  @doc """
  Returns the request's headers as a map, or `%{}` when the frame carries none.

  Used by callers that re-dispatch a tool call (the Lua sandbox, the hub meta
  server) and must forward the caller's context to the next hop.
  """
  @spec request_headers(Frame.t() | map() | nil) :: map()
  def request_headers(frame) do
    case raw_headers(frame) do
      headers when is_map(headers) -> headers
      _ -> %{}
    end
  end

  # ------------------------------------------------------------------
  # Private helpers
  # ------------------------------------------------------------------

  defp raw_headers(%Frame{context: context}) when is_map(context),
    do: Map.get(context, :headers)

  defp raw_headers(%{headers: headers}), do: headers

  # A plain headers map (tests, callers that already extracted them).
  defp raw_headers(headers) when is_map(headers) and not is_struct(headers), do: headers
  defp raw_headers(_frame), do: nil

  defp header_value(headers, name) do
    Enum.find_value(headers, fn
      {key, value} when is_binary(key) and is_binary(value) ->
        if String.downcase(key) == name, do: String.trim(value)

      _ ->
        nil
    end)
  end

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
