# Standalone Lua Code-Mode Server (`lua` / `code_mode`)

Status: L1 + L2 + L3 implemented and deployed (zero-downtime hot reload);
L4 (persistent sandbox bindings, an AiderDesk `lua` mode) still open.
Date: 2026-10-21.

## 1. Motivation

`code_mode` today is a **component of the `mcp-hub` meta server**
(`Exhub.MCP.Tools.Hub.CodeMode`, registered in `lib/exhub/mcp/hub/server.ex`;
engine `Exhub.MCP.Hub.CodeMode`). That couples a general Lua execution facility
to the hub's transport, store, search index and header filtering, and forces
every consumer to reach it through the hub.

The real consumer is **AiderDesk** (the agent; ExHub is its toolset). AiderDesk
already ships a JavaScript code-mode (`programmatic-tool-calls`) and a
`code-mode-lua` extension that today is **UI-only** — it just syntax-highlights
`mcp-hub`'s `code_mode` messages. Lua deserves to be a **first-class execution
language** available to the agent, not a single hub tool.

The design borrows from **Legion** (`software-mansion-labs/legion`), an Elixir
runtime for code-writing agents:

- a transport-agnostic `Legion.Sandbox` behaviour
  (`check/2`, `execute/5`, `binding_names/1`, `prompt_info/0`);
- `Legion.Sandbox.Runner` — timeout / heap / reduction / priority budgets;
- `Legion.Tools.Help` — on-demand tool docs, instead of an inline catalog;
- (later) `binding_scope` — persistent sandbox bindings.

## 2. Decisions (confirmed)

| # | Decision |
|---|----------|
| 1 | Standalone MCP server named **`lua`**, mounted at **`/lua/mcp`**, exposing tool **`code_mode`**. |
| 2 | Tool bridge scope = **all** tools (built-in + upstream MCP servers). |
| 3 | Tool discovery = **retrieval (TF-IDF) + Smart Decide relevance** (the existing two-stage pipeline). |
| 4 | The old `/mcp-hub` `code_mode` is **removed** (no alias). |

Safety exceptions to "all": the meta hub (`mcp-hub`) and the Lua server itself
(`lua`) are excluded from the bridge, so a script cannot recurse into the hub or
itself.

## 3. Target architecture

```
AiderDesk ──MCP──▶ /lua/mcp  (Exhub.MCP.LuaServer)
                              ├─ code_mode   (Exhub.MCP.Tools.Lua.CodeMode)
                              └─ help        (Exhub.MCP.Tools.Lua.Help)
                                     │
                                     ▼
                          Exhub.Lua.Engine.run/3        (transport-agnostic)
                              ├─ Exhub.Lua.Sandbox      (behaviour)
                              │    └─ Exhub.Lua.Sandbox.Lua
                              ├─ Exhub.Lua.Runner       (resource bounds)
                              ├─ Exhub.Lua.Bridge       (tool closures + globals)
                              ├─ Exhub.Lua.Discovery    (retrieve_tools/describe_tool)
                              └─ Exhub.Lua.Help         (help/1)
                                     │
        bridged tools ▼              ▼ local, no HTTP
   Exhub.MCP.Hub.ClientManager.list_all_tools/0 ──▶ BuiltInRegistry (built-ins)
                                                 └─▶ upstream HTTP MCP servers
```

## 4. L1 — Engine extraction (pure refactor, zero behavior change)

Move everything in `Exhub.MCP.Hub.CodeMode` (engine) into a transport-agnostic
namespace, keeping the current semantics byte-for-byte:

| New module | Responsibility (from current engine) |
|---|---|
| `lib/exhub/lua/engine.ex` — `Exhub.Lua.Engine` | `run/3` orchestration: config merge, `run_bounded/2`, diagnostics, result formatting, truncation + spill. |
| `lib/exhub/lua/runner.ex` — `Exhub.Lua.Runner` | Monitored, unlinked eval process; `max_heap_size`, wall-clock timeout. (L1 = parity; the Legion extras — reductions polling, off-heap binary accounting, `:low` priority, kill-on-parent-death — land here too, as an additive hardening.) |
| `lib/exhub/lua/sandbox.ex` — `Exhub.Lua.Sandbox` + `Exhub.Lua.Sandbox.Lua` | The behaviour (Legion-shaped) and the Lua 5.3 implementation: `check/2` (size guard), `execute/5`, `binding_names/1`, `prompt_info/0` (language/constraints/tool_usage). |
| `lib/exhub/lua/bridge.ex` — `Exhub.Lua.Bridge` | Builds the sandbox: `Lua.new/1`, nested + flat tool binding, reserved globals, `print`, `parallel`/`parallel_all`, discovery globals. |
| `lib/exhub/lua/diagnostics.ex` — `Exhub.Lua.Diagnostics` | `SyntaxError` / `UnknownTool` / `ToolFailure` / `TimeoutExceeded` classification (from `CodeMode.diagnostic/1`). |

Reused unchanged: `Exhub.MCP.Encoding.sanitize_utf8/1`,
`Exhub.MCP.Hub.ToolSearch`, `Exhub.MCP.Hub.ToolRelevance`,
`Exhub.MCP.Tools.Hub.RetrieveTools.compact_params/1`.

L1 keeps `Exhub.MCP.Hub.CodeMode` as a thin delegator to `Exhub.Lua.Engine` **for
one commit**, so the refactor is provably behavior-preserving before anything is
removed.

## 5. L2 — Standalone server + tools

New modules:

- `lib/exhub/mcp/lua_server.ex` — `Exhub.MCP.LuaServer`
  ```elixir
  use Anubis.Server, name: "exhub-lua-server", version: "1.0.0", capabilities: [:tools]
  component(Exhub.MCP.Tools.Lua.CodeMode)
  component(Exhub.MCP.Tools.Lua.Help)
  def init(_ci, frame), do: {:ok, frame}
  def handle_request(req, frame),
    do: Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, req, frame)
  ```
- `lib/exhub/mcp/tools/lua/code_mode.ex` — moved from `tools/hub/code_mode.ex`;
  `name/0 = "code_mode"`; `execute/2` bridges `ClientManager.list_all_tools/0`
  (minus `mcp-hub`/`lua`) and calls `Exhub.Lua.Engine.run/3`.
- `lib/exhub/mcp/tools/lua/help.ex` — new MCP tool `help` (see §6).

Wiring:

| File | Change |
|---|---|
| `lib/exhub/router.ex` | `forward("/lua/mcp", to: Exhub.MCP.LazyPlug, init_opts: [server: Exhub.MCP.LuaServer, request_timeout: 600_000])` |
| `lib/exhub/mcp/hub/built_in_registry.ex` | add `"lua" => Exhub.MCP.LuaServer` to `@built_in_servers` |
| `lib/exhub/application.ex` | add child `{Exhub.MCP.LuaServer, transport: :streamable_http, request_timeout: 600_000, session_idle_timeout: 86_400_000 * 365}` |
| `config/config.exs` | new `config :exhub, :lua, …` (§7) |

## 6. L3 — On-demand tool exposure (help + retrieval + Smart Decide)

The `code_mode` tool description stops inlining a signature catalog. It keeps
only: language rules, how to call bridged tools, and how to discover them. Three
sandbox globals cover discovery, all over the **same** bridged tool set:

- `retrieve_tools({query, limit, filter})` — `ToolSearch.build_index/1` over the
  bridged tools → `ToolSearch.search/3` (TF-IDF) → `ToolRelevance.filter/3`
  (Smart Decide `noul` per candidate, concurrent, fail-open). Returns
  `{tools = {…}, count = n}`, each entry `{name, server, description, params}`.
  Mirrors `Exhub.MCP.Tools.Hub.RetrieveTools`.
- `describe_tool(path)` — full `description` + `input_schema` for one tool.
- `help(name)` / `help()` — Legion-`Help` framing: one tool's description +
  compact signature, or the full index (one-line summaries) when called with no
  argument. Content sourced from the same tool list as the bridge.

The same content is reachable as the **MCP tool `help`** (so an agent can read
docs without executing code), exactly as Legion exposes `Legion.Tools.Help`
both as a tool and as an in-sandbox global.

Config `tool_docs: :on_demand | :inline` selects the new behavior vs the old
inline catalog (default `:on_demand`).

### Implemented (L3)

| Piece | Module |
|---|---|
| Rendering (`index/1`, `render/2`, `resolve/2`) | `Exhub.Lua.Help` |
| Shared visibility (exclusions + `x-include/exclude-tools`) | `Exhub.MCP.Tools.Lua.VisibleTools` |
| MCP tool `help` | `Exhub.MCP.Tools.Lua.Help` (registered in `Exhub.MCP.LuaServer`) |
| In-sandbox global `help()` / `help("server.tool")` | `Exhub.Lua.Bridge`, reserved global |
| Description switch | `Exhub.MCP.Tools.Lua.CodeMode.description/0` reads `tool_docs` |

`description/0` is evaluated at runtime (`Anubis.Server.Component.get_description/1`),
so flipping `tool_docs` needs no rebuild. Because both tools resolve their tool
set through `VisibleTools`, `help` documents exactly what `code_mode` bridges.
`Exhub.Lua.Engine.namespaces/0` — the source of the inline catalog — is now only
consulted in `:inline` mode.

## 7. Config

Replace `:code_mode` with `:lua` (settings unchanged unless noted):

```elixir
config :exhub, :lua,
  enabled: true,
  timeout_ms: 600_000,
  max_instructions: 5_000_000,
  max_call_depth: 200,
  max_heap_size: 268_435_456,
  max_string_bytes: 8_388_608,
  max_output_chars: 100_000,
  spill_truncated: true,
  spill_dir: nil,
  max_concurrency: 8,
  raise_on_tool_error: true,
  tool_docs: :on_demand,                 # NEW
  exclude_servers: ["mcp-hub", "lua"]    # meta hub + self
```

## 8. Removal of the hub `code_mode`

- `lib/exhub/mcp/hub/server.ex` — drop `component(Exhub.MCP.Tools.Hub.CodeMode)`.
- delete `lib/exhub/mcp/tools/hub/code_mode.ex` and `lib/exhub/mcp/hub/code_mode.ex`.
- `docs/modules/mcp-hub.md` — remove the `code_mode` section; add `docs/modules/lua.md`.
- `config/config.exs` — remove the `:code_mode` block.
- `test/exhub/mcp/hub/code_mode_test.exs` — replaced (§11).

## 9. AiderDesk side

- `~/.aider-desk/mcp-servers.json` — add `"lua": {"url": "http://localhost:9069/lua/mcp"}`.
  Keep `mcp-hub` only if `retrieve_tools`/`call_tools` are still wanted.
- `~/.aider-desk/extensions/code-mode-lua/index.ts` — `messageFilter` becomes
  `{ types: ['tool'], serverName: 'lua', toolName: 'code_mode' }`.
- (Later) a `lua` mode mirroring `ptc`: `onToolsetCreated` hides the other tools
  so the model orchestrates everything through Lua.

## 10. Rollout (zero-downtime — never restart the VM)

Code changes reload hot; a new **supervision child** does not, so it is attached
explicitly (per `AGENTS.md`):

1. `MIX_ENV=prod mix release --overwrite`
2. `exhub_hot_reload` (router forward, registry, engine, tools).
3. `bin/exhub rpc 'Application.put_env(:exhub, :lua, %{…})'` (runtime config)
4. `bin/exhub rpc 'Supervisor.start_child(Exhub.Supervisor, {Exhub.MCP.LuaServer, transport: :streamable_http, request_timeout: 600_000, session_idle_timeout: 86_400_000 * 365})'`
5. Verify: `curl -sS localhost:9069/lua/mcp` and an `initialize` + `tools/list`.

> The `application.ex` child spec takes over at the next full boot, so step 4 is
> one-time.

## 11. Test plan

- `test/exhub/lua/engine_test.exs` — from `code_mode_test.exs`: `run/3` with an
  injected `call_fun`; limits; diagnostics; truncation/spill; `parallel`
  strict/soft semantics.
- `test/exhub/lua/discovery_test.exs` — `retrieve_tools` / `describe_tool` /
  `help` over a fixed tool list, with a stubbed relevance filter.
- `test/exhub/mcp/tools/lua/code_mode_test.exs` + `help_test.exs` — schema + execute.
- Run: `mix test --no-start test/exhub/lua/` and
  `mix test --no-start test/exhub/mcp/tools/lua/`.

## 12. Risks / open items

- **Cost**: `retrieve_tools` now may call Smart Decide per candidate (already the
  hub's behavior); concurrent, fail-open, and skippable with `filter: false`.
- **Latency**: bridging "all" includes upstream HTTP servers; built-ins go
  through `BuiltInRegistry` locally.
- **ASCII lexer**: the `lua` lexer takes ASCII source only — build CJK strings
  from data (`string.char`, `"\\231\\148\\169"`) and slice with `utf8.offset`.
- **Breaking change**: removing `/mcp-hub` `code_mode` breaks any client using it
  — coordinate with the `code-mode-lua` extension update (§9).
- **Self-exclusion**: `lua` and `mcp-hub` must never be bridged.
```