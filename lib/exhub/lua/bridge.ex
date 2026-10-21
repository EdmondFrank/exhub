defmodule Exhub.Lua.Bridge do
  @moduledoc """
  Builds the Lua sandbox: registers the sandbox-owned globals (`print`,
  `parallel`, `parallel_all`, `retrieve_tools`, `describe_tool`, `help`) and
  bridges every visible tool in twice — nested by server
  (`desktop.read_file({…})`) and flat via `tools[\"server__tool\"]({…})`.

  Also owns the tool-call funnel (outcome classification + MCP `isError`
  handling), the `parallel` / `parallel_all` implementations, and the in-sandbox
  discovery helpers (`retrieve_tools`, `describe_tool`, `help`).
  """

  alias Exhub.Lua.Diagnostics
  alias Exhub.Lua.Help
  alias Exhub.Lua.Value
  alias Exhub.MCP.Encoding
  alias Exhub.MCP.Hub.ToolSearch
  alias Exhub.MCP.Tools.Hub.RetrieveTools

  # Globals the sandbox owns; a server whose sanitized name lands on one of
  # these is only reachable through the flat `tools` table.
  @reserved_globals ~w(print parallel parallel_all retrieve_tools describe_tool help)

  @doc "Globals the sandbox owns (reserved names)."
  @spec reserved_globals() :: [String.t()]
  def reserved_globals, do: @reserved_globals

  @doc """
  Builds a Lua state with `tools` bridged in as functions and the sandbox globals
  set. `call_fun` dispatches one tool call; `cfg` carries the sandbox limits.
  """
  def build(tools, call_fun, cfg) do
    lua =
      Lua.new(
        max_instructions: cfg[:max_instructions],
        max_call_depth: cfg[:max_call_depth],
        max_string_bytes: cfg[:max_string_bytes]
      )

    # Discovery helpers search the exact set of tools bridged below, so a snippet
    # can find a tool mid-run instead of spending a separate `retrieve_tools`
    # round trip. The index is rebuilt per evaluation from the visible tools —
    # the same list the bridge uses.
    index = ToolSearch.build_index(tools)

    lua =
      lua
      |> Lua.set!([:print], print_fun())
      |> Lua.set!([:parallel], parallel_fun(call_fun, cfg, :strict))
      |> Lua.set!([:parallel_all], parallel_fun(call_fun, cfg, :soft))
      |> Lua.set!([:retrieve_tools], retrieve_tools_fun(index))
      |> Lua.set!([:describe_tool], describe_tool_fun(elem(index, 0)))
      |> Lua.set!([:help], help_fun(tools))

    Enum.reduce(tools, lua, fn tool, lua ->
      server = tool |> Map.get("server") |> to_string_or_nil()
      name = tool |> Map.get("name") |> to_string_or_nil()

      if server == nil or name == nil do
        lua
      else
        bridge = tool_fun(server, name, call_fun, cfg)
        flat = Lua.set!(lua, ["tools", "#{server}__#{name}"], bridge)

        if sanitize(server) in @reserved_globals do
          flat
        else
          Lua.set!(flat, [sanitize(server), sanitize(name)], bridge)
        end
      end
    end)
  end

  # --- Tool bridging ---

  defp tool_fun(server, name, call_fun, cfg) do
    fn args, lua ->
      with {:ok, arg_map} <- Value.to_args(Lua.decode_list!(lua, args)) do
        case call_tool(call_fun, server, name, arg_map, cfg) do
          {:ok, result} ->
            {encoded, lua} = Lua.encode!(lua, result)
            {encoded, lua}

          {:error, message} ->
            {:error, "ToolFailure: #{server}.#{name} failed: #{message}", lua}
        end
      else
        {:error, message} ->
          {:error, "ToolFailure: #{server}.#{name} failed: #{message}", lua}
      end
    end
  end

  # Single funnel for dispatching one tool call and classifying its outcome.
  # Returns `{:ok, result}` or `{:error, message}`; `parallel_fun/3` reuses it.
  defp call_tool(call_fun, server, tool, args, cfg) do
    case call_fun.(server, tool, args) do
      {:ok, result} ->
        if tool_error?(result) and raise_on_tool_error?(cfg) do
          {:error, tool_error_text(result)}
        else
          {:ok, result}
        end

      {:error, reason} ->
        {:error, Diagnostics.format_reason(reason)}

      other ->
        {:error, "unexpected #{inspect(other)}"}
    end
  end

  # --- parallel / parallel_all ---

  defp parallel_fun(call_fun, cfg, mode) do
    fn args, lua ->
      descriptors = args |> then(&Lua.decode_list!(lua, &1)) |> parallel_args()
      results = run_parallel(descriptors, call_fun, cfg)

      case mode do
        :strict -> render_strict(descriptors, results, lua)
        :soft -> render_soft(results, lua)
      end
    end
  end

  defp run_parallel(descriptors, call_fun, cfg) do
    max = cfg[:max_concurrency] || 8

    descriptors
    |> Task.async_stream(
      fn
        %{server: server, tool: tool, args: args} when is_binary(server) and is_binary(tool) ->
          call_tool(call_fun, server, tool, args, cfg)

        bad ->
          {:error, "invalid descriptor #{inspect(bad)}"}
      end,
      ordered: true,
      max_concurrency: max,
      timeout: :infinity
    )
    |> Enum.map(fn
      {:ok, result} -> result
      {:exit, reason} -> {:error, Diagnostics.format_reason(reason)}
    end)
  end

  # Strict `parallel` is the `Promise.all` analog: it aborts on the first
  # failure. But `run_parallel/3` has already settled every call, so the abort
  # carries the whole batch — each call's descriptor and result — instead of
  # discarding it. When at least one call succeeded the report is stashed for
  # `Sandbox.Lua.execute/4`, which returns it as an ordinary result so the
  # successes survive the failure; a batch in which every call failed stays a
  # plain error.
  defp render_strict(descriptors, results, lua) do
    if Enum.any?(results, &match?({:error, _}, &1)) do
      report = parallel_report(descriptors, results)

      if Enum.any?(results, &match?({:ok, _}, &1)) do
        Process.put(:code_mode_partial, report)
      end

      {:error, report, lua}
    else
      values = Enum.map(results, fn {:ok, value} -> value end)
      Lua.encode!(lua, values)
    end
  end

  defp parallel_report(descriptors, results) do
    failures = Enum.count(results, &match?({:error, _}, &1))
    total = length(results)

    lines =
      descriptors
      |> Enum.zip(results)
      |> Enum.with_index(1)
      |> Enum.map(fn {{descriptor, result}, index} ->
        "[#{index}] #{call_label(descriptor)} #{settled_line(result)}"
      end)

    Enum.join(["parallel: #{failures} of #{total} calls failed" | lines], "\n")
  end

  defp call_label(%{server: server, tool: tool}) when is_binary(server) and is_binary(tool),
    do: "#{server}.#{tool}"

  defp call_label(_descriptor), do: "(invalid call)"

  # Results here are raw Elixir terms from `ClientManager`, not Lua-decoded
  # values, so encode them with the JSON fallback used for error payloads
  # (`Value.render_value/1` assumes Lua's `{key, value}`-pair shape and blows up
  # on a plain list of maps).
  defp settled_line({:ok, value}), do: "ok: #{Value.encode_fallback(value)}"
  defp settled_line({:error, message}), do: "error: #{message}"

  defp render_soft(results, lua) do
    payload =
      Enum.map(results, fn
        {:ok, value} -> %{"ok" => true, "result" => value}
        {:error, message} -> %{"ok" => false, "error" => message}
      end)

    Lua.encode!(lua, payload)
  end

  defp parallel_args([first | _rest]) do
    case Value.lua_to_elixir(first) do
      list when is_list(list) -> Enum.map(list, &descriptor/1)
      _ -> []
    end
  end

  defp parallel_args(_), do: []

  defp descriptor(%{} = call) do
    args = normalize_args(Map.get(call, "args"))

    case {Map.get(call, "server"), Map.get(call, "tool"), Map.get(call, "name")} do
      {server, tool, _name} when is_binary(server) and is_binary(tool) ->
        %{server: server, tool: tool, args: args}

      {_server, _tool, name} when is_binary(name) ->
        case String.split(name, "__", parts: 2) do
          [server, tool] -> %{server: server, tool: tool, args: args}
          _ -> %{server: nil, tool: nil, args: args}
        end

      _ ->
        %{server: nil, tool: nil, args: args}
    end
  end

  defp descriptor(_), do: %{server: nil, tool: nil, args: %{}}

  defp normalize_args(nil), do: %{}
  defp normalize_args(args) when is_map(args), do: args
  defp normalize_args(_), do: %{}

  # --- MCP-level failure detection ---

  defp raise_on_tool_error?(cfg), do: cfg[:raise_on_tool_error] != false

  defp tool_error?(%{"isError" => true}), do: true
  defp tool_error?(%{isError: true}), do: true
  defp tool_error?(_), do: false

  defp tool_error_text(result) do
    case result do
      %{"content" => content} when is_list(content) ->
        text =
          content
          |> Enum.map(fn
            %{"text" => text} when is_binary(text) -> text
            other -> inspect(other)
          end)
          |> Enum.join("\n")

        if text == "", do: Value.encode_fallback(result), else: Encoding.sanitize_utf8(text)

      _ ->
        Value.encode_fallback(result)
    end
  end

  defp print_fun do
    fn args, lua ->
      line = args |> then(&Lua.decode_list!(lua, &1)) |> Enum.map_join("\t", &Value.stringify/1)
      Process.put(:code_mode_log, Process.get(:code_mode_log, []) ++ [line])
      {[], lua}
    end
  end

  # --- Tool discovery helpers ---

  # `retrieve_tools({query, limit})` — the in-sandbox counterpart of the hub's
  # own `retrieve_tools` tool, backed by the same `ToolSearch` TF-IDF ranking
  # over the bridged tool set. An empty query browses every tool alphabetically.
  defp retrieve_tools_fun(index) do
    fn args, lua ->
      request = args |> then(&Lua.decode_list!(lua, &1)) |> discovery_request()
      {encoded, lua} = Lua.encode!(lua, search_index(index, request))
      {encoded, lua}
    end
  end

  defp search_index(index, request) do
    query = Map.get(request, "query", "")
    limit = to_int(Map.get(request, "limit"), 10)

    docs =
      case index do
        {list, _inverted} when is_list(list) -> list
        _ -> []
      end

    matches =
      if query == "" do
        docs |> Enum.sort_by(& &1["full_name"]) |> Enum.take(limit)
      else
        ToolSearch.search(index, query, limit: limit)
      end

    entries = Enum.map(matches, &tool_entry/1)
    %{"tools" => entries, "count" => length(entries)}
  end

  defp tool_entry(doc) do
    base = %{
      "name" => doc["full_name"],
      "server" => doc["server"],
      "description" => doc["description"]
    }

    case RetrieveTools.compact_params(doc["input_schema"]) do
      nil -> base
      params -> Map.put(base, "params", params)
    end
  end

  # `describe_tool(path)` — full description and input schema for one tool, so a
  # snippet can see the nested arguments/enums a compact param summary hides.
  # Accepts the flat `server__tool` name or the nested `server.tool` path;
  # returns `{error = "..."}` for an unknown path.
  defp describe_tool_fun(docs) do
    fn args, lua ->
      path = args |> then(&Lua.decode_list!(lua, &1)) |> discovery_path()
      {encoded, lua} = Lua.encode!(lua, describe_doc(docs, path))
      {encoded, lua}
    end
  end

  defp describe_doc(docs, path) do
    case find_doc(docs, path) do
      nil ->
        %{
          "error" =>
            "UnknownTool: #{inspect(path)} is not a known tool. " <>
              "Use retrieve_tools({query = '...'}) to find available tools."
        }

      doc ->
        %{
          "name" => doc["full_name"],
          "server" => doc["server"],
          "description" => doc["description"],
          "input_schema" => doc["input_schema"]
        }
    end
  end

  defp find_doc(docs, path) when is_binary(path) do
    trimmed = path |> String.trim() |> String.replace_prefix("tools.", "")
    variants = [trimmed, String.replace(trimmed, ".", "__")]

    Enum.find(docs, fn doc ->
      doc["full_name"] in variants or "#{doc["server"]}.#{doc["name"]}" in variants
    end)
  end

  defp find_doc(_docs, _path), do: nil

  # `help(path)` — the tool index (no argument) or one tool's rendered
  # documentation, sourced from the same tool list bridged above. The sister of
  # the MCP tool `Exhub.MCP.Tools.Lua.Help`, so a snippet can read docs without a
  # discovery round trip; rendering lives in `Exhub.Lua.Help`.
  defp help_fun(tools) do
    fn args, lua ->
      path = args |> then(&Lua.decode_list!(lua, &1)) |> help_path()
      {encoded, lua} = Lua.encode!(lua, Help.render(tools, path))
      {encoded, lua}
    end
  end

  # Accept `help()`, `help("server.tool")`, or `help({tool = "..."})`.
  defp help_path([]), do: nil

  defp help_path([first | _rest]) do
    case Value.lua_to_elixir(first) do
      path when is_binary(path) -> path
      map when is_map(map) -> Map.get(map, "tool") || Map.get(map, "name") || Map.get(map, "path")
      _ -> nil
    end
  end

  # Accept either a bare query string or `{query = ..., limit = ...}`.
  defp discovery_request([]), do: %{}

  defp discovery_request([first | _rest]) do
    case Value.lua_to_elixir(first) do
      map when is_map(map) -> map
      query when is_binary(query) -> %{"query" => query}
      _ -> %{}
    end
  end

  defp discovery_path([]), do: nil

  defp discovery_path([first | _rest]) do
    case Value.lua_to_elixir(first) do
      path when is_binary(path) -> path
      map when is_map(map) -> Map.get(map, "name") || Map.get(map, "path") || Map.get(map, "tool")
      _ -> nil
    end
  end

  defp to_int(value, _default) when is_integer(value) and value > 0, do: value
  defp to_int(value, _default) when is_float(value) and value > 0, do: trunc(value)
  defp to_int(_value, default), do: default

  # --- Names ---

  defp sanitize(name) do
    name
    |> to_string()
    |> String.replace(~r/[^A-Za-z0-9_]/, "_")
  end

  defp to_string_or_nil(nil), do: nil
  defp to_string_or_nil(""), do: nil
  defp to_string_or_nil(value) when is_binary(value), do: value
  defp to_string_or_nil(value), do: to_string(value)
end
