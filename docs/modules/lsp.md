# Exhub LSP (MCP)

`/lsp/mcp` exposes ExHub's language-server manager — the
[lsp-bridge port](lsp-bridge.md) — as MCP tools for callers that are **not**
Emacs: AiderDesk, scripts, CI, another editor.

The defining difference from every editor integration is *scope*: a tool call
takes a **file path** and resolves that file's own project root, so there is
no "current workspace" to be confined to. The same `zls`/`gopls`/`elixirLS`
process serves Emacs and the agent, keyed by `{project_root, server}` — one
warm index per project, not one per client.

## Why it exists

An agent that edits files needs the compiler's opinion, cheaply:

- **After an edit** — is it broken, and where? (`lsp_diagnostics`)
- **Before a change** — who calls this, what is its type? (`lsp_references`,
  `lsp_hover`, `lsp_definition`)
- **When lost** — where is that symbol? (`lsp_symbols` with a query)
- **Later, unprompted** — what did I break and never look at again?
  (`lsp_ledger`, fed by every session)

## Tools

| Tool | LSP request | Notes |
|------|-------------|-------|
| `lsp_diagnostics` | push + `textDocument/diagnostic` | One file or a batch. Re-read from disk, mirrored, saved — so build-on-save servers (zls, elixirLS) actually compile. |
| `lsp_ledger` | — | Last error-level diagnostics per file, across sessions and projects; survives closing the file. |
| `lsp_status` | — | Running sessions, documents, advertised capabilities, ledger summary; `shutdown` stops a wedged session. |
| `lsp_definition` | `textDocument/definition` | |
| `lsp_references` | `textDocument/references` | `include_declaration`, capped. |
| `lsp_hover` | `textDocument/hover` | Markdown/`MarkedString` flattened to text. |
| `lsp_symbols` | `documentSymbol` / `workspace/symbol` | File outline, or a project-wide name search. |
| `lsp_code_actions` | `textDocument/codeAction` | Returns the actions and their edit sizes. Never writes to disk. |

Positions are **1-based** (`1:1` is the first character), and every reported
location is 1-based too, so output can be fed straight back into another call.

All tools accept `server` (pin a server by name) and `project_path`
(authoritative root), and `lsp_diagnostics` also takes `severity`,
`max_per_file` and `wait_ms`.

## How a call is resolved

```
file path
  ├─ project root   git rev-parse --show-toplevel
  │                 └─ outside git: build markers (build.zig, mix.exs, Cargo.toml, go.mod, …)
  ├─ language       Exhub.LspBridge.Language.language_for/1  (extension or file name)
  ├─ server         preferred for the language, then any other configured server
  │                 whose executable exists on PATH
  ├─ session        Session.ensure({root, profile}) — shared with Emacs
  └─ document       read from disk, stamped (mtime+size)
                    didOpen → didSave → await publish
```

Three behaviours fall out of this, each deliberate:

- **Disk is authoritative, an editor is not overruled.** A document already
  open in Emacs with unsaved changes reports `sync: editor` and is left
  untouched; its diagnostics describe the buffer, and the headless caller is
  told so rather than silently overwriting it.
- **`didSave` is sent after syncing.** Servers that only type-check on save
  (`zls` with `enable_build_on_save`, `elixirLS`) would otherwise return an
  empty list forever — a green light that means "not checked", not "clean".
- **Waiting is signal-driven.** `Headless` waits for the server to *publish*
  (a verdict, possibly empty) instead of sleeping a fixed time; if nothing
  arrives within `wait_ms`, the result says so explicitly instead of claiming
  the file is clean.

## Vendored server configuration

`priv/lsp_bridge/langserver/*.json` is shared with the Emacs front end, so
settings changed for the MCP path apply there too:

```json
// zls.json
"settings": { "enable_build_on_save": true, "build_on_save_step": "check" }
```

`zls` reads its config flat (`initializationOptions`), but requests the `zls`
section over `workspace/configuration`; `Exhub.LspBridge.Server` answers a
missing section with the whole settings map, so one flat table serves both
paths.

Paths under `Project.@default_ignored_prefixes` (`~/.cache`, `~/.asdf`,
`/opt/homebrew`, `/nix/store`, …) never start a server: the enclosing
"project" would be a tool install, not code.

## Lifecycle

Sessions are `{root, profile}` GenServers holding one OS process per server.
They stop after `idle_stop` (default 300 s) of inactivity and restart lazily on
the next call, re-opening the documents they knew about. `lsp_status` shows
what is live; `lsp_status` with `shutdown` (a file or directory inside the
project) stops a session early — shared with Emacs, so it stops there too.

## Client setup

AiderDesk, `~/.aider-desk/mcp-servers.json`:

```json
{ "mcpServers": { "lsp": { "url": "http://localhost:9069/lsp/mcp" } } }
```

Any MCP client that speaks Streamable HTTP works the same way; the server is
also reachable through the hub as `lsp__*`.

## Module map

| Module | Role |
|--------|------|
| `Exhub.MCP.LspServer` | Anubis MCP server (8 tools), route `/lsp/mcp`, registered in `BuiltInRegistry` |
| `Exhub.MCP.Tools.Lsp.*` | One module per tool; parameter shaping and rendering only |
| `Exhub.LspBridge.Headless` | Root/language resolution, disk sync, synchronous raw requests, diagnostics await |
| `Exhub.LspBridge.Language` | Extension/file-name → `languageId`, preferred server, build markers |
| `Exhub.LspBridge.Ledger` | Out-of-band diagnostics record (`lsp_ledger`) |
| `Exhub.LspBridge.Session` | `ensure_open/5`, `subscribe/2`, `diagnostics_state/2`, `info/1` (additions) |

## See also

- [lsp-bridge.md](lsp-bridge.md) — the Emacs-facing front end and the
  underlying manager
- `docs/plans/2026-10-04-lsp-bridge-elixir-port.md` — original port design