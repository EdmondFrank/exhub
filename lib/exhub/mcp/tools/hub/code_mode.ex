defmodule Exhub.MCP.Tools.Hub.CodeMode do
  @moduledoc """
  MCP tool: `code_mode`

  Evaluates a Lua 5.3 snippet with every visible MCP hub tool bridged in as a
  function, so the caller can fetch, filter and combine tool results in a single
  round trip instead of one `call_tools` per step. See
  `Exhub.MCP.Hub.CodeMode` for the sandbox itself.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Hub.ClientManager
  alias Exhub.MCP.Hub.CodeMode, as: Engine

  use Anubis.Server.Component, type: :tool

  # The Hub server's `request_timeout` (see `application.ex` / `router.ex`) is
  # the outer bound: `ConcurrentToolDispatcher` hard-kills the tools/call Task
  # at that point. The Lua VM must time out first so it can return a graceful
  # error, so the sandbox default matches it and callers cannot exceed it.
  @server_timeout_ms 600_000
  @default_timeout_s div(@server_timeout_ms, 1000)

  def name, do: "code_mode"

  @impl true
  def description do
    """
    Execute a Lua 5.3 snippet in a sandbox with every visible MCP hub tool
    bridged in as a function, and return the snippet's result in one round trip.

    Instead of calling `call_tools` once per tool — discovering, waiting, then
    discovering again — write one script that fetches, filters, loops and returns
    the distilled answer:

        local r = desktop.read_file({path = "/etc/hosts"})
        return r.content[1].text

    **Calling tools** (both forms work):
    - Nested by server: `desktop.read_file(args)`, `web_tools.web_fetch(args)`
    - Flat lookup: `tools["desktop__read_file"](args)` — the exact `server__tool`
      name returned by `retrieve_tools` (kept verbatim, e.g.
      `tools["browser-use__browser_navigate"]`).

    **Parallel calls** (one round trip, many tools at once):
        local r = parallel({
          {server = "time",      tool = "get_current_time", args = {timezone = "UTC"}},
          {server = "web-tools", tool = "fetch",            args = {url = "https://…"}}
        })
        return r[1].content[1].text .. r[2].content[1].text
    `parallel` raises on the first failing call and returns results index-aligned
    with the input; `parallel_all` never raises, returning one
    `{ok = true, result = …}` / `{ok = false, error = …}` per call.

    **Rules**
    - Arguments are a Lua table with named keys, e.g.
      `{path = "/x", pattern = "*.ex", search_type = "content"}`.
    - Results are Lua tables; most tools return their text payload under
      `r.content[1].text`.
    - A failing call raises a Lua error — both hub/transport errors and MCP
      results carrying `isError = true`. Catch it with
      `local ok, r = pcall(desktop.read_file, {path = "/nope"})`.
    - `print(...)` output is collected and returned above your `return` value.
    - Return a table/string/number to send it back; returning nothing sends
      "Execution completed with no return value."
    - The sandbox blocks io/os/require/filesystem — the only way out is the
      tools. Loops are bounded by an instruction, timeout and memory budget.

    Available namespaces: #{namespaces_hint()}.
    """
  end

  schema do
    field(:code, :string,
      description:
        "Lua 5.3 code to execute. Tools are callable as `server.tool(args)` or `tools[\"server__tool\"](args)`; `args` is a table of named keys. `print(...)` is captured. Use `return` to send a value back.",
      required: true
    )

    field(:timeout, :integer,
      description:
        "Maximum execution time in seconds (default #{@default_timeout_s}, capped at #{@default_timeout_s} to match the MCP server request timeout).",
      default: @default_timeout_s
    )
  end

  @impl true
  def execute(params, frame) do
    code = Map.get(params, :code, "")
    timeout_s = Map.get(params, :timeout, @default_timeout_s)

    response =
      cond do
        not Engine.enabled?() ->
          error("code_mode is disabled")

        not is_binary(code) or code == "" ->
          error("`code` is required")

        not (is_integer(timeout_s) and timeout_s > 0) ->
          error("`timeout` must be a positive integer number of seconds")

        true ->
          tools = visible_tools(frame)
          # Read the cap from the engine config (single source of truth) rather
          # than the compile-time constant, so a config change can't drift.
          server_timeout_ms = Engine.config()[:timeout_ms] || @server_timeout_ms
          timeout_ms = min(timeout_s * 1000, server_timeout_ms)

          Engine.run(code, tools, timeout_ms: timeout_ms)
          |> to_response()
      end

    {:reply, response, frame}
  end

  # --- Tool visibility ---

  defp visible_tools(frame) do
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
  # upstream name or by the hub's `server__tool` form — match on both.
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

  defp parse_tool_list(_), do: []

  defp maybe_include(tools, []), do: tools

  defp maybe_include(tools, include) do
    allowed = MapSet.new(include)
    Enum.filter(tools, fn tool -> matches?(tool, allowed) end)
  end

  defp maybe_exclude(tools, []), do: tools

  defp maybe_exclude(tools, exclude) do
    denied = MapSet.new(exclude)
    Enum.reject(tools, fn tool -> matches?(tool, denied) end)
  end

  defp matches?(tool, set) do
    server = Map.get(tool, "server", "")
    name = Map.get(tool, "name", "")
    MapSet.member?(set, name) or MapSet.member?(set, "#{server}__#{name}")
  end

  # --- Response helpers ---

  defp to_response({:ok, text}), do: Response.tool() |> Response.text(text)
  defp to_response({:error, text}), do: error(text)

  defp error(message), do: Response.tool() |> Response.error(message)

  defp namespaces_hint do
    case Engine.namespaces() do
      [] -> "(none connected)"
      names -> Enum.join(names, ", ")
    end
  end
end
