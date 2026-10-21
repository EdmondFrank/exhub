defmodule Exhub.MCP.Tools.Lua.CodeMode do
  @moduledoc """
  MCP tool: `code_mode` (server `lua`)

  Evaluates a Lua 5.3 snippet with every visible MCP tool bridged in as a
  function, so the caller can fetch, filter and combine tool results in a single
  round trip instead of one call per step. See `Exhub.Lua.Engine` for the sandbox
  itself and `docs/modules/lua.md` for the server.

  The description is selected by `config :exhub, :lua, :tool_docs`:

    * `:on_demand` (default) keeps only the language rules and points at the
      `help` / `retrieve_tools` / `describe_tool` discovery helpers, so the
      catalog is pulled when needed instead of inlined on every request;
    * `:inline` appends the full namespace catalog to the description.

  `Anubis.Server.Component.get_description/1` calls `description/0` at runtime,
  so the setting applies without a rebuild.
  """

  alias Anubis.Server.Response
  alias Exhub.Lua.Engine
  alias Exhub.MCP.Tools.Lua.VisibleTools

  use Anubis.Server.Component, type: :tool

  # The server's `request_timeout` (see `application.ex` / `router.ex`) is the
  # outer bound: `ConcurrentToolDispatcher` hard-kills the tools/call Task at
  # that point. The Lua VM must time out first so it can return a graceful
  # error, so the sandbox default matches it and callers cannot exceed it.
  @server_timeout_ms 600_000
  @default_timeout_s div(@server_timeout_ms, 1000)

  def name, do: "code_mode"

  @impl true
  def description do
    case Engine.config()[:tool_docs] do
      :inline -> inline_description()
      _ -> on_demand_description()
    end
  end

  # Default: rules + discovery, with the docs pulled on demand (`help`,
  # `retrieve_tools`, `describe_tool`) instead of an inlined catalog.
  defp on_demand_description do
    """
    Execute a Lua 5.3 snippet in a sandbox with every visible MCP tool bridged
    in as a function, and return the snippet's result in one round trip.

    Instead of calling one tool at a time — discovering, waiting, then
    discovering again — write one script that fetches, filters and returns the
    distilled answer:

        local r = parallel({
          {server = "desktop", tool = "read_file",       args = {path = "/etc/hosts"}},
          {server = "desktop", tool = "execute_command", args = {command = "uname -a"}}
        })
        return {hosts = r[1].content[1].text:sub(1, 40), uname = r[2].content[1].text}

    `parallel` fans the calls out concurrently and returns results index-aligned
    with the input (a call may also be `{name = "server__tool", args = {...}}`).
    `parallel_all` never raises, returning one `{ok = ..., result = ...}` /
    `{ok = ..., error = ...}` per call.

    **Calling tools** (both forms work):
    - Nested by server: `desktop.read_file({path = "..."})`,
      `web_tools.web_fetch({url = "..."})` — server and tool names have every
      character outside `[A-Za-z0-9_]` replaced with `_`.
    - Flat, verbatim: `tools["browser-use__browser_navigate"]({...})`.
    - Arguments are a Lua table with **named keys**; a positional table is
      rejected. A result is a Lua table — the text payload is usually
      `r.content[1].text`.
    - A failing call raises a Lua error (catch it with `pcall`, which yields the
      message string). An uncaught failure is categorised as `SyntaxError`,
      `UnknownTool`, `ToolFailure` or `TimeoutExceeded`.
    - `print(...)` output is returned above the `return` value. Returning
      nothing reports "Execution completed with no return value."; `return a, b`
      returns both as a JSON array.

    **Finding tools — never guess a name**
    - `retrieve_tools({query = "...", limit = 10})` — TF-IDF search over the
      bridged tools (an empty query browses all). Returns `{tools = {...}, count = n}`;
      each entry is `{name, server, description, params}`.
    - `describe_tool("server__tool")` (or `"server.tool"`) — that tool's full
      `description` and `input_schema`.
    - `help()` / `help("server.tool")` — the tool index, or one tool's
      description and parameters. The same docs are available as the MCP tool
      `help`, without running code.

    The sandbox blocks io/os/require/filesystem, so the only way out is the
    bridged tools. Loops are bounded by an instruction budget, a wall-clock
    timeout (default #{@default_timeout_s}s) and a memory budget.
    """
  end

  # Opt in with `tool_docs: :inline` — the catalog rides in the description.
  defp inline_description do
    """
    Execute a Lua 5.3 snippet in a sandbox with every visible MCP tool bridged
    in as a function, and return the snippet's result in one round trip.

    Instead of calling one tool at a time — discovering, waiting, then
    discovering again — write one script that fetches, filters, loops and returns
    the distilled answer:

        -- Read, bulk-edit and rebuild in a single round trip.
        local edits = {{"OldName", "NewName"}, {"old_flag", "new_flag"}}
        for _, file in ipairs({"/repo/lib/a.ex", "/repo/lib/b.ex"}) do
          local src = desktop.read_file({path = file}).content[1].text
          for _, e in ipairs(edits) do
            src = src:gsub(e[1], e[2])
          end
          desktop.write_file({path = file, content = src})
        end

        return desktop.execute_command({
          command = "mix compile --warnings-as-errors",
          working_dir = "/repo"
        }).content[1].text

    **This lets you:**
    - call many tools from a single turn, in sequence or in parallel;
    - filter, reshape and join results before they reach context;
    - loop, branch and compute over tool output in a real language.

    **Calling tools** (both forms work):
    - Nested by server: `desktop.read_file(args)`, `web_tools.web_fetch(args)`
    - Flat lookup: `tools["desktop__read_file"](args)`, `tools["browser-use__browser_navigate"](args)`.

    **Parallel calls** (one round trip, many tools at once):
        local r = parallel({
          {server = "desktop", tool = "read_file",       args = {path = "/etc/hosts"}},
          {server = "desktop", tool = "execute_command", args = {command = "uname -a"}}
        })
        return r[1].content[1].text .. r[2].content[1].text
    `parallel` fans the calls out concurrently and returns results index-aligned
    with the input (assign to `local r`, then index `r[1]`, `r[2]`). It still
    aborts on the first failure, but every call has already settled, so the abort
    reports each call and — when at least one succeeded — is returned as an
    ordinary result rather than an error, preserving the successes; a batch where
    every call failed stays an error. `parallel_all` never raises, returning one
    `{ok = true, result = …}` / `{ok = false, error = …}` per call.

    **Finding tools (inside the script)**
    - `retrieve_tools({query = "...", limit = 10})` searches every callable tool
      by name/description — the in-sandbox counterpart of the `retrieve_tools`
      tool, so a script can find a tool it wasn't told about without a second
      round trip. Returns `{tools = {...}, count = n}`; an empty query lists
      tools alphabetically. Accepts a bare string too: `retrieve_tools("read file")`.
    - `describe_tool("server__tool")` (or `"server.tool"`) returns that tool's
      full `description` and `input_schema`, so argument names/nesting are
      visible before the call. Unknown paths return `{error = "..."}`.
    - `help()` / `help("server.tool")` — the tool index, or one tool's
      description and parameters.

    **Rules**
    - Arguments are a Lua table with named keys, e.g.
      `{path = "/x", pattern = "*.ex", search_type = "content"}`. A positional
      table (`{"a", "b"}`) is rejected — omit the argument to pass `{}`.
    - Results are Lua tables; most tools return their text payload under
      `r.content[1].text`.
    - A failing call raises a Lua error — both transport errors and MCP results
      carrying `isError = true`. Catch it with
      `local ok, r = pcall(desktop.read_file, {path = "/nope"})`; note `pcall`
      yields only the message string, not the structured payload.
    - An uncaught failure comes back categorised: `SyntaxError`, `UnknownTool`
      (with a `retrieve_tools` hint), `ToolFailure` (`server.tool failed: …`),
      or `TimeoutExceeded`.
    - `print(...)` output is collected and returned above your `return` value.
    - Return a table/string/number to send it back; returning nothing sends
      "Execution completed with no return value."; `return a, b` returns both as
      a JSON array.
    - When the result exceeds the output cap, the returned text is truncated and
      the full output is written to a temp file whose path is included — read it
      back with `desktop.read_file`.
    - The sandbox blocks io/os/require/filesystem — the only way out is the
      tools. Loops are bounded by an instruction budget, a wall-clock timeout
      (default #{@default_timeout_s}s) and a memory budget.

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
          tools = VisibleTools.list(frame)
          # Read the cap from the engine config (single source of truth) rather
          # than the compile-time constant, so a config change can't drift.
          server_timeout_ms = Engine.config()[:timeout_ms] || @server_timeout_ms
          timeout_ms = min(timeout_s * 1000, server_timeout_ms)

          Engine.run(code, tools, timeout_ms: timeout_ms)
          |> to_response()
      end

    {:reply, response, frame}
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
