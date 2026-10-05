# exhub-lsp-bridge

The `Exhub.LspBridge` subsystem is ExHub's native **language-server manager** —
an Elixir/OTP port of [lsp-bridge](https://github.com/manateelazycat/lsp-bridge)'s
Python backend (`lsp_bridge.py` + `core/`). Emacs remains the client; ExHub owns
the external language-server processes, the JSON-RPC wire protocol, the
`langserver`/`multiserver` configuration, document sync and diagnostics.

It follows the same shape as the other ExHub features (`exhub-fim`,
`blink-search`): a supervised OTP subtree, driven over the WebSocket with
`["func", ["lsp-bridge", …]]` commands, pushing results back as elisp via
`Exhub.send_message/1`. It is hot-reloadable and starts lazily (no language
server runs until a buffer asks for one).

**Status:** Phase 4 ✅ — session/document lifecycle, diagnostics, the
read-only features, completion and server-driven edits. Emacs opens, edits,
saves and closes buffers; ExHub mirrors content, drives
`didOpen`/`didChange`/`didSave`/`didClose`, caches capabilities, delivers push-
and pull-based diagnostics (rendered with `flymake` in `exhub-lsp.el`), answers
hover, definition/type-definition/implementation/references navigation,
document/workspace symbols and signature help, serves completion candidates
(plus `completionItem/resolve` documentation) to acm through
`acm-backend-exhub-lsp.el`, produces `WorkspaceEdit`/`TextEdit` results for
rename, formatting and code actions (applied to the affected buffers by the
elisp front end), answers call hierarchy (incoming/outgoing), and decodes inlay
hints and semantic tokens for elisp-side decorations. The design and roadmap live in
[docs/plans/2026-10-04-lsp-bridge-elixir-port.md](../plans/2026-10-04-lsp-bridge-elixir-port.md).

## Architecture

| Module | Purpose |
|--------|---------|
| `Exhub.LspBridge.Application` | Supervisor subtree: `Registry` + `Config` + server/session `DynamicSupervisor`s + `ClientManager` |
| `Exhub.LspBridge.Protocol` | `Content-Length` JSON-RPC framing (pure encode/decode) |
| `Exhub.LspBridge.Config` | Loader/cache for vendored `langserver/*.json` + `multiserver/*.json` |
| `Exhub.LspBridge.MultiServer` | Method → ordered server-list resolution from a multi-server profile |
| `Exhub.LspBridge.Project` | Project root (git/override/`projectFiles`) + single-vs-multi server selection |
| `Exhub.LspBridge.Capabilities` | Derives provider flags + sync kind from an `initialize` result; gating |
| `Exhub.LspBridge.Document` | Per-buffer `uri`/`version`/`content`, incremental-change mirroring, `did*` params |
| `Exhub.LspBridge.Diagnostics` | Per-server cache, merge/hide/max, pull params + result handling |
| `Exhub.LspBridge.Server` | One language-server process: `Port`, handshake, capabilities, queued frames, request-id correlation |
| `Exhub.LspBridge.Session` | One project + profile: owns its servers and documents, routes methods, debounces diagnostics |
| `Exhub.LspBridge.Handler` | Behaviour: one feature (method, provider, cancel-on-change, params, response→payload) |
| `Exhub.LspBridge.Handlers` + `Handlers.*` | Compile-time registry and the per-feature handlers (read-only + completion/resolve + edits) |
| `Exhub.LspBridge.Handlers.Completion` | `textDocument/completion` candidate shaping/sorting/filtering (port of `completion.py`) |
| `Exhub.LspBridge.Handlers.CompletionItem` | `completionItem/resolve` documentation/edits (port of `completion_item.py`) |
| `Exhub.LspBridge.Handlers.PrepareRename` / `.Rename` | `textDocument/prepareRename` / `rename` (ports of `prepare_rename.py` / `rename.py`) |
| `Exhub.LspBridge.Handlers.Formatting` / `.RangeFormatting` | `textDocument/formatting` / `rangeFormatting` (ports of `formatting.py` / `range_formatting.py`) |
| `Exhub.LspBridge.Handlers.CodeAction` / `.ExecuteCommand` | `textDocument/codeAction` / `workspace/executeCommand` (ports of `code_action.py` / `execute_command.py`) |
| `Exhub.LspBridge.Handlers.CallHierarchyPrepare` / `.Incoming` / `.Outgoing` | `textDocument/prepareCallHierarchy` + `callHierarchy/incomingCalls`/`outgoingCalls` (port of `call_hierarchy.py`) |
| `Exhub.LspBridge.Handlers.InlayHint` / `.SemanticTokens` | `textDocument/inlayHint` / `semanticTokens/full` (ports of `inlay_hint.py` / `semantic_tokens.py`) |
| `Exhub.LspBridge.ClientManager` | WebSocket command router + Emacs callbacks |
| `Exhub.LspBridge.Elisp` | Renders results as escaped-string elisp forms |
| `Exhub.ResponseHandlers.ExhubLspBridge` | `["func", ["lsp-bridge", …]]` entry point → `ClientManager` |

### Process topology

One `Registry` (unique keys) and two `DynamicSupervisor`s:

```
ClientManager (singleton)
  └─ Session {:session, root, profile}      (SessionSupervisor)
       ├─ Server {:server, root, name}      (Supervisor)   one OS process each
       └─ Document (filepath → struct)      in Session state
```

`{:doc, filepath}` → owning `Session` is also registered, so a command carrying
only a path can find its session. A `Session` is keyed by
`{root, profile}` where `profile` is `{:single, "elixirLS"}` or
`{:multi, "pyright_ruff"}`; it shuts down (and stops its servers) when its last
document closes.

The subtree is added to `Exhub.Supervisor` in `lib/exhub/application.ex`. A new
supervision-tree child will *not* start in an already-running VM without the
recipe in `AGENTS.md` ("Activating new supervision-tree children"); a plain code
change hot-reloads normally.

## Configuration

The language-server tables are **vendored into `priv/lsp_bridge/`**:

- `priv/lsp_bridge/langserver/*.json` — one file per language server, with the
  lsp-bridge schema (`name`, `languageId`, `command`, `projectFiles`, `settings`,
  `support-single-file`).
- `priv/lsp_bridge/multiserver/*.json` — one file per multi-server profile, keyed
  by filename, mapping a feature to an ordered list of server names:

      { "default": "pyright", "diagnostics": ["pyright", "ruff"], "formatting": "ruff" }

`Exhub.LspBridge.Config` resolves the directories via `:code.priv_dir(:exhub)`.
Override (e.g. to point at a live lsp-bridge checkout while developing):

```elixir
# config/*.exs
config :exhub, :lsp_bridge_langserver_dir, "/path/to/lsp-bridge/langserver"
config :exhub, :lsp_bridge_multiserver_dir, "/path/to/lsp-bridge/multiserver"
```

`Config.reload/1` re-reads both directories without a VM restart. Malformed
files are skipped with a debug log — notably the vendored
`qmlls_javascript.json`, which has a trailing comma that is invalid JSON (broken
upstream as well).

## Emacs protocol (P1–P4)

Emacs sends `["func", ["lsp-bridge", action, …]]`; results are pushed as elisp
forms with JSON **string** payloads (parsed with `json-parse-string`).

| Action | Arguments | Callback(s) to Emacs |
|--------|-----------|----------------------|
| `ping` | — | `(exhub-lsp-pong)` |
| `open-file` | `path`, `content`, `opts` (`language-id`, `project-path`, `multi`, `single`, `diag-idle`) | `(exhub-lsp-ready path servers)` |
| `change-file` | `path`, `change` (`range`, `rangeLength`, `text`) | diagnostics push |
| `save-file` / `close-file` | `path` | — |
| `change-cursor` | `path`, `position` | — |
| `request` / `notify` | `path`, `method`, `params` | `(exhub-lsp-response server id json)` / `(exhub-lsp-error-response …)` |
| `diagnostics` / `list-diagnostics` | `path`, `opts` | `(exhub-lsp-diagnostics path json count)` |
| `hover` | `path`, `position` | `(exhub-lsp--hover path markdown)` |
| `find-define` / `find-type-define` / `find-implementation` | `path`, `position` | `(exhub-lsp--locations path kind json)` |
| `find-references` | `path`, `position` | `(exhub-lsp--locations path "references" json)` |
| `document-symbol` | `path` | `(exhub-lsp--symbols path json)` |
| `workspace-symbol` | `path`, `query` | `(exhub-lsp--workspace-symbols query json)` |
| `signature-help` | `path`, `position` | `(exhub-lsp--signature-help path json)` |
| `completion` | `path`, `position`, `char`, `prefix`, `opts` (`match-mode`, `case-mode`, `items-limit`, `auto-import`, `display-label-max-length`, `block-kind-list`) | `(exhub-lsp--completion path server candidates items meta)` |
| `completion-item-resolve` | `path`, `key`, `server`, `item` | `(exhub-lsp--completion-doc path server key doc edits)` |
| `prepare-rename` | `path`, `position` | `(exhub-lsp--rename-range path json)` |
| `rename` | `path`, `position`, `new-name` | `(exhub-lsp--workspace-edit json message)` |
| `format` / `range-format` | `path`, `opts` / `path`, `range`, `opts` | `(exhub-lsp--format path edits)` |
| `code-action` | `path`, `range`, `opts` (`only`) | `(exhub-lsp--code-actions path actions)` |
| `execute-command` | `path`, `command`, `arguments` | `(exhub-lsp--workspace-edit json message)` |
| `call-hierarchy-prepare` | `path`, `position` | `(exhub-lsp--call-hierarchy-items path json)` |
| `call-hierarchy-incoming` / `-outgoing` | `path`, `item` | `(exhub-lsp--call-hierarchy path direction json)` |
| `inlay-hint` | `path`, `range` | `(exhub-lsp--inlay-hints path json)` |
| `semantic-tokens` | `path` | `(exhub-lsp--semantic-tokens path json)` |
| `shutdown` | `path` | — |
| `start-server` / `stop-server` | `name`, `project-path` | raw control (debugging) |

Diagnostics arrive as `(exhub-lsp-diagnostics path json count)` (push on
change, debounced; plus a `textDocument/diagnostic` pull when the server
advertises `diagnosticProvider`). Read-only feature results are described above
— `(exhub-lsp--locations path kind json)` carries normalised
`{uri, path, range, selectionRange}` locations, with `kind` one of `definition`,
`type-definition`, `implementation`, `references`. Other server notifications
are forwarded as `(exhub-lsp-notification server method json)`; informational
text as `(exhub-lsp-message msg)`; errors as `(exhub-lsp-error msg)`.

Each read-only command is gated on the server's advertised provider
(`Exhub.LspBridge.Capabilities`); a request sent before the `initialize`
handshake completes is allowed through and queued by the `Server`, so an
immediate hover/definition right after opening a buffer still works.

`completion` factors each server's candidates the way `completion.py` does
(kind filter, `string_match`, snippet→yasnippet, prefix/score/`sortText` sort,
top-N) and returns them alongside the raw items keyed by candidate `key`, so
`completion-item-resolve` can send the exact item back to the one server that
produced it (a `server` argument pins the target; `handler_targets` honours it).
The completion `context` uses `TriggerCharacter` when the typed char is in the
union of the target servers' advertised `triggerCharacters` (a single fan-out
request cannot carry per-server context — a documented approximation).

The edit commands (`rename`, `format`/`range-format`, `code-action`) return the
server's `WorkspaceEdit`/`TextEdit` to Emacs, which applies them to the target
buffers (sorted last-position-first, under `inhibit-modification-hooks`); the
backend stays authoritative only for document sync. `code-action` passes the
diagnostics overlapping the request range in `context.diagnostics`, and
`execute-command` is the one command **not** capability-gated — servers
routinely attach commands to code actions without advertising
`executeCommandProvider`.

Call hierarchy is a two-step flow driven from Emacs: `call-hierarchy-prepare`
returns the `CallHierarchyItem`s (carrying any server-private `data`), the user
picks one, and that exact item is sent back to `call-hierarchy-incoming` or
`call-hierarchy-outgoing`.

Decorations are **elisp-side**: `inlay-hint` returns the server's `InlayHint[]`
for a range, and `semantic-tokens` returns its `data` array already expanded to
absolute `{line, character, length, type, modifiers}` tokens (the LSP delta
encoding decoded against the server's advertised `legend`, which
`Session` passes in the handler context as `semantic_tokens_legend`), so the
front end only paints overlays. An empty/absent result still emits an empty list
so stale overlays are cleared. Both are refreshed on an idle timer by their
toggle minor modes.

Server-initiated requests during startup (`workspace/configuration`,
`client/registerCapability`, `window/workDoneProgress/create`) are answered
from the server's configured `settings` so startup never blocks; servers also
receive a `workspace/didChangeConfiguration` after `initialized`.

## Emacs front end — `exhub-lsp.el` + `acm-backend-exhub-lsp.el` (P1–P4)

`exhub-lsp.el` is the option-B, ExHub-native front end (no EPC, no
lsp-bridge.el). It implements the **document lifecycle + diagnostics** (P1),
the **read-only features** (P2), **completion** (P3, via acm) and the **edits**
and **decorations** (P4):

- `exhub-lsp-global-mode` / `exhub-lsp-mode` in `exhub-lsp-enabled-modes`
  (default: `elixir-mode`, `elixir-ts-mode`);
- `find-file` → `open-file`, `after-change-functions` → `change-file`,
  `after-save-hook` → `save-file`, `kill-buffer-hook` → `close-file`;
- diagnostics rendered through a **`flymake` backend** built from the pushed
  list;
- read-only commands (`exhub-lsp-mode-map`: `M-.` definition, `M-?` references,
  `M-,` implementation, `C-c C-t` type-definition, `C-c h` hover, `C-c s`
  document symbols, `C-c S` workspace symbols). Navigation reuses the built-in
  **`xref` UI** (a single definition jumps directly, multiple locations open an
  xref buffer), hover renders in `*exhub-lsp-hover*` (`gfm-view-mode`), document
  symbols populate `imenu`, workspace symbols prompt with `completing-read`, and
  signature help shows the active signature in the echo area;
- completion through `acm-backend-exhub-lsp.el`, an **acm backend adapter**. It
  fills acm's buffer-local `acm-backend-lsp-items` with the candidates and keeps
  the raw items for `completionItem/resolve`, so acm's own menu, icons,
  filtering, documentation and expansion all work on ExHub candidates. Completion
  fires automatically on `post-self-insert-hook` (when `exhub-lsp-completion-auto`
  is on) and on demand via `C-c C-c` (`exhub-lsp-completion`). ExHub completion
  and the Python `lsp-bridge` share acm's buffer-locals, so enable only one of
  them per buffer.
- edits (P4): `exhub-lsp-rename` (`C-c C-r`), `exhub-lsp-format` (`C-c C-f`) and
  `exhub-lsp-code-action` (`C-c C-a`). Rename applies the returned
  `WorkspaceEdit` across every affected buffer (both `changes` and
  `documentChanges` shapes); formatting applies the `TextEdit`s to the buffer
  (whole buffer or active region, `exhub-lsp-format-region`); code actions are
  chosen with `completing-read` and either apply their edit or send the attached
  command through `execute-command`. `prepare-rename` pulse-highlights the range
  before the edit lands. Edits are applied on the elisp side, so the backend
  stays a pure request/response layer.
- call hierarchy (P4): `exhub-lsp-call-hierarchy-incoming` (`C-c C-i`) and
  `exhub-lsp-call-hierarchy-outgoing` (`C-c C-o`). A `completing-read` picks the
  prepared `CallHierarchyItem`, then the incoming/outgoing calls, and jumps to
  the chosen one.
- decorations (P4): two toggle minor modes, `exhub-lsp-inlay-hints-mode`
  (lighter `InH`) and `exhub-lsp-semantic-tokens-mode` (lighter `Sem`). Each
  requests on an idle timer after changes (0.4s), paints overlays
  (`after-string` for hints, a `face` for tokens) and clears them when toggled
  off. Both are off by default; enable per buffer or from
  `exhub-lsp-enabled-modes`-style setup. Semantic-token faces come from
  `exhub-lsp-semantic-tokens-faces` (type-name → face).

Reload both files without restarting Emacs (see `AGENTS.md`); load the adapter
first so `require` picks up its changes:

```sh
emacsclient -e '(progn (load-file "~/.emacs.d/site-lisp/exhub/acm-backend-exhub-lsp.el") (load-file "~/.emacs.d/site-lisp/exhub/exhub-lsp.el"))'
```

## Testing

```sh
mix test --no-start test/exhub/lsp_bridge/
```

Covers protocol framing, config loading (vendored langserver + multiserver
profiles), `Capabilities` derivation, `Project` root/selection, `Document`
change mirroring (incl. UTF-16 positions), `Diagnostics` merge/pull, the
read-only `Handler`s (`handlers_test.exs`: location normalisation incl.
`LocationLink`, hover contents, request params, symbol/signature shaping), the
completion handlers (`completion_test.exs`: fuzzy/substring/case matching,
block-kind filtering, snippet→yas conversion, score/`sortText` sorting,
item-key/auto-import, TriggerCharacter vs Invoked context, resolve
documentation), the edit handlers (`edits_test.exs`: prepare-rename range
normalisation, rename/format/range-format params and payloads, code-action
diagnostic-range filtering and `only`, execute-command shapes), the
call-hierarchy handlers (`call_hierarchy_test.exs`: prepare/incoming/outgoing
params and payloads), the decoration handlers (`inlay_hint_test.exs` /
`semantic_tokens_test.exs`: hint payloads, LSP delta/legend decoding incl.
modifier bitmasks and missing-legend degradation), the `Server` handshake,
`Session` handler dispatch
(capability gating, `cancel_on_change`) and
a full `Session` open→change→close cycle. The
integration tests boot a fake stdio LSP server written in Elixir (run as a
script file so it exits on EOF), so no external language server is required.

### Real-server integration (`:e2e_lsp`)

`test/exhub/lsp_bridge/e2e_test.exs` is the P1 deliverable check: it creates a
scratch `mix new` project, boots the LspBridge subtree by hand (the app is not
booted under `--no-start`), opens a file containing a deliberate syntax error
through `Session.open_file/4`, and asserts elixirLS pushes an error-severity
diagnostic tagged `elixirLS`. It is tagged `:e2e_lsp` and **excluded by
default** (`test/test_helper.exs`), since it needs a real `language_server.sh`
on `PATH` and is slower:

```sh
mix test --no-start --include e2e_lsp test/exhub/lsp_bridge/e2e_test.exs
```

elixirLS first publishes an empty diagnostics list (clearing stale state)
before compiling, so the test waits past empty updates for the real one.

**elixirLS capability reality:** elixirLS advertises `rename`,
`rangeFormatting`, `callHierarchy`, `inlayHint` and `semanticTokens` as
**absent**, so those paths are covered by the fake-LSP `session_test.exs`
integration tests (which advertise and answer them); `formatting` and
`code-action` are the edit paths verified against a real server.

## Roadmap

| Phase | Scope |
|-------|-------|
| P0 ✅ | Skeleton stabilised: config vendoring, multiserver parsing, tests, this doc |
| P1 ✅ | Project/Session/Document lifecycle, document sync, diagnostics (push + pull), capability gating, multi-server routing; `exhub-lsp.el` flymake slice |
| P2 ✅ | Read-only features: hover, definition, type-definition, implementation, references, document/workspace symbols, signature help (`Handler` behaviour + registry; `exhub-lsp.el` jump/hover/symbol commands) |
| P3 ✅ | Completion + resolve (`Completion`/`CompletionItem` handlers; `acm-backend-exhub-lsp.el` acm adapter) |
| P4 ✅ | Rename, code action, formatting, inlay hints, semantic tokens, call hierarchy |
| P5 | Optional parity: remote/tramp, devcontainer, ctags/AI completion backends |