# Lua Code-Mode Server (`lua`)

Standalone MCP server that runs Lua 5.3 snippets with **every visible MCP tool**
bridged in as a function — the
[code execution beats function calling](https://www.anthropic.com/engineering/code-execution-with-mcp)
pattern, so a caller spends one round trip instead of one per tool call.

- **Endpoint**: `/lua/mcp` (streamable HTTP)
- **Built-in name**: `lua` (reachable via `Exhub.MCP.Hub.BuiltInRegistry` without an HTTP hop)
- **Server**: `Exhub.MCP.LuaServer`
- **Tools**: `Exhub.MCP.Tools.Lua.CodeMode` (`code_mode`), `Exhub.MCP.Tools.Lua.Help` (`help`)

It replaced the hub's former `code_mode` tool (which lived on `mcp-hub`); the hub
no longer exposes it.

## Layers

| Module | Role |
|--------|------|
| `Exhub.Lua.Engine` | Transport-agnostic `run/3`: config, result formatting, truncation/spill |
| `Exhub.Lua.Runner` | Monitored, unlinked eval process; wall-clock timeout + heap cap |
| `Exhub.Lua.Sandbox` / `Exhub.Lua.Sandbox.Lua` | Behaviour + the Lua 5.3 implementation |
| `Exhub.Lua.Bridge` | Registers the bridged tool closures, `print`, `parallel`, `parallel_all`, `retrieve_tools`, `describe_tool`, `help` |
| `Exhub.Lua.Help` | Renders the tool index / one tool's docs (`help`) |
| `Exhub.MCP.Tools.Lua.VisibleTools` | The tool set `code_mode` and `help` both expose (exclusions + header filter) |
| `Exhub.Lua.Value` | Lua ↔ Elixir conversion and JSON rendering |
| `Exhub.Lua.Diagnostics` | `SyntaxError` / `UnknownTool` / `ToolFailure` / `TimeoutExceeded` |

The design borrows from [Legion](https://legion.hexdocs.pm/Legion.html) (an Elixir
runtime for code-writing agents) without depending on it: execution goes through
the `Exhub.Lua.Sandbox` behaviour over the pure-Elixir
[`lua`](https://hexdocs.pm/lua) VM, which cannot reach the BEAM except through
the tool closures the bridge registers.

## `code_mode`

**Parameters**

| Parameter | Type    | Required | Default | Description                       |
|-----------|---------|----------|---------|-----------------------------------|
| `code`    | string  | yes      | —       | Lua 5.3 snippet to execute        |
| `timeout` | integer | no       | 600     | Maximum execution time in seconds |

**Calling tools** — every tool is bridged in twice, so either name style works:

- Nested by server: `desktop.read_file({path = "/etc/hosts"})`,
  `web_tools.web_fetch({url = "…"})` (every non-alphanumeric character in the
  server and tool names becomes `_`).
- Flat lookup: `tools["desktop__read_file"]({path = "/etc/hosts"})` — the exact
  `server__tool` name returned by `retrieve_tools`, kept **verbatim** (a
  hyphenated server keeps its dash, e.g. `tools["browser-use__browser_navigate"]`).

**Parallel calls** — `parallel({…})` is the `Promise.all` analog: it fans the
calls out concurrently and returns results index-aligned with the input. It
still aborts on the first failure, but every call has already settled, so the
abort reports each call (descriptor + outcome) and — when at least one call
succeeded — the report comes back as an **ordinary result** rather than an error,
so one bad call cannot replace the successes; a batch in which every call failed
stays an error. `pcall(parallel, {…})` in the script still catches the abort as a
string. `parallel_all({…})` is the all-settled analog (never raises; returns one
`{ok = true, result = …}` / `{ok = false, error = …}` per call).

```lua
local r = parallel({
  {server = "time",      tool = "get_current_time", args = {timezone = "UTC"}},
  {server = "web-tools", tool = "fetch",            args = {url = "https://…"}}
})
return r[1].content[1].text .. r[2].content[1].text
```

A call may also be given as `{name = "server__tool", args = {…}}`.

**Rules**

- Arguments are a Lua table with named keys, decoded to a JSON object; a call
  without a table passes `{}`. A positional table (`{"a", "b"}`) is rejected —
  MCP arguments must be named objects.
- Results are Lua tables — most tools expose their text payload at
  `r.content[1].text`.
- Returned values are UTF-8-repaired before they are formatted. A byte-offset
  slice (`s:sub(1, n)`) can end inside a multibyte codepoint, and a single
  dangling byte made `Jason` reject the whole value; `Exhub.MCP.Encoding.sanitize_utf8/1`
  runs first, so tables stay JSON (one U+FFFD per dangling byte) and the result
  is valid UTF-8 for truncation and spill.
- `return a, b` returns **several** values as a JSON array (`[1, 2, 3]`); a single
  return is rendered as a bare value.
- The `lua` lexer takes **ASCII source only**: a non-ASCII literal in the snippet
  fails to compile (`Unexpected character: 云 (U+4E91)`). Build CJK strings from
  data (`string.char(0xE7, 0x94, 0xA9)`) and slice on codepoint boundaries with
  `utf8.offset` rather than raw byte counts.
- A failing call raises a Lua error, catchable with `pcall`:
  `local ok, r = pcall(desktop.read_file, {path = "/nope"})` (note `pcall` yields
  only the message string). Both failure sources raise: transport errors, and MCP
  results carrying `isError = true`. Set config `raise_on_tool_error: false` to
  return `isError` payloads as ordinary data instead (then check `r.isError`).
- `print(...)` output is collected and returned above the snippet's `return` value.
- Returning nothing yields `Execution completed with no return value.`
- When the result exceeds `max_output_chars`, the returned text is truncated and
  the full output is written to a temp file whose path is included — read it back
  with `desktop.read_file` (set `spill_truncated: false` to disable).
- The sandbox blocks `io`/`os`/`require`/filesystem — the only way out is the
  tools. Loops are bounded by an instruction budget, a wall-clock timeout and a
  heap cap; each evaluation runs in its own short-lived, unlinked process, so a
  runaway script cannot take the server down. `print`, `parallel`, `parallel_all`,
  `retrieve_tools`, `describe_tool` and `help` are reserved globals.

**Example**

```json
{
  "method": "tools/call",
  "params": {
    "name": "code_mode",
    "arguments": {
      "code": "local r = desktop.read_file({path = \"/etc/hosts\"}); return r.content[1].text"
    }
  }
}
```

Tool visibility matches `retrieve_tools`: the `mcp-hub` meta server and the `lua`
server itself are excluded, and `x-include-tools` / `x-exclude-tools` request
headers are honoured (by bare tool name or `server__tool` form). These headers are
a **server-wide** allow/deny list applied to `tools/call` as well, so an
`x-include-tools` list must also name `code_mode` itself or the call is rejected
with `Tool not found`.

## `help`

Reads the bridged tools' documentation **without executing code** — the
[`Legion.Tools.Help`](https://legion.hexdocs.pm/Legion.Tools.Help.html) analog.

| Parameter | Type   | Required | Description                                              |
|-----------|--------|----------|----------------------------------------------------------|
| `tool`    | string | no       | `server.tool` or `server__tool`; omit for the full index |

- No argument -> the index, one line per tool (`server.tool - first description line`).
- `tool` given -> that tool's description, parameter signature and a call hint.
  An unknown path returns an `UnknownTool` message; a bare name matching several
  tools returns `AmbiguousTool` rather than guessing.

The same docs are reachable **inside a snippet** as `help()` / `help("server.tool")`
(a reserved global), so a running script needs no discovery round trip. Both the
tool and the global are rendered by `Exhub.Lua.Help`, and both document exactly
the set `code_mode` bridges (`Exhub.MCP.Tools.Lua.VisibleTools`) — `help` can
never describe a tool the sandbox cannot call.

```json
{
  "method": "tools/call",
  "params": { "name": "help", "arguments": { "tool": "desktop.read_file" } }
}
```

## Configuration

In-code defaults live in `Exhub.Lua.Engine`; override under `config :exhub, :lua`
in `config/config.exs`:

| Key | Default | Meaning |
|-----|---------|---------|
| `enabled` | `true` | Master switch |
| `timeout_ms` | `600_000` | Wall-clock budget per evaluation (aligned with the server `request_timeout`) |
| `max_instructions` | `5_000_000` | Lua VM instruction budget |
| `max_call_depth` | `200` | Lua call-depth cap |
| `max_heap_size` | `268_435_456` | Sandbox process heap cap (bytes) |
| `max_string_bytes` | `8_388_608` | Max Lua string size (bytes) |
| `max_output_chars` | `100_000` | Result truncation, to bound context; overflow is spilled to a file |
| `spill_truncated` | `true` | Write the full result to a temp file when truncation occurs |
| `spill_dir` | `nil` | Directory for spilled results (`nil` → `System.tmp_dir!()`) |
| `max_concurrency` | `8` | Max concurrent calls in a `parallel`/`parallel_all` fan-out |
| `raise_on_tool_error` | `true` | Raise on MCP results with `isError = true` (instead of returning them as data) |
| `tool_docs` | `:on_demand` | `code_mode` description detail: `:on_demand` points at the discovery helpers, `:inline` embeds the namespace catalog |
| `exclude_servers` | `["mcp-hub", "lua"]` | Servers not bridged into the sandbox (meta hub + self) |

## Clients

AiderDesk reaches it by adding to `~/.aider-desk/mcp-servers.json`:

```json
{ "lua": { "url": "http://localhost:9069/lua/mcp" } }
```
