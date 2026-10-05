# lsp-bridge → Elixir Port on ExHub — Design & Plan

**Date**: 2026-10-04
**Status**: Approved — P0 done (2026-10-04), P1 done (2026-10-05, verified against
real elixirLS), P2 done (2026-10-05, verified against real elixirLS), P3 done
(2026-10-05, verified against real elixirLS), P4 done (2026-10-05: P4a edits,
P4b call hierarchy, P4c decorations)
**Scope**: Port the lsp-bridge Python backend to `Exhub.LspBridge.*`, keep a thin
elisp frontend in the ExHub idiom (option B) whose completion menu is an **acm
adapter**. `langserver/*.json` / `multiserver/*.json` are **vendored into
`priv/lsp_bridge/`**.
**Namespace**: `Exhub.LspBridge.*` inside the existing `exhub` OTP app

---

## 1. Overview

[lsp-bridge](https://github.com/manateelazycat/lsp-bridge) is an Emacs LSP
client whose defining idea is to run all LSP I/O, document sync and result
munging in an external **multi-threaded backend**, leaving Emacs free of blocking
work and GC pressure. Today that backend is Python (`lsp_bridge.py` + `core/`),
talking to Emacs over EPC.

ExHub already plays exactly this role for other features (FIM, blink-search,
translation, chat): an Elixir/OTP backend attached to Emacs over a WebSocket,
with a thin elisp front end. The goal here is to **re-implement lsp-bridge's
backend as an `Exhub.LspBridge.*` subsystem** — the same way `Exhub.GenClaw` and
`Exhub.Toonflow` were built inside the `exhub` app — while reusing lsp-bridge's
language-agnostic assets (the JSON config tables, the LSP wire protocol, the
feature set) and keeping an lsp-bridge-flavoured elisp client.

This is **not** a rewrite of lsp-bridge's elisp frontend (it cannot be: that
half must live in Emacs). It is a backend replacement plus a frontend adapter.

An incomplete skeleton already exists in this repo (uncommitted — see §5). This
document defines the target architecture and the phased path from that skeleton
to feature parity for the features that matter day-to-day.

## 2. Goals & Non-Goals

### Goals

1. **G1 — Backend port.** Reimplement `lsp_bridge.py` + `core/`
   (`lspserver.py`, `fileaction.py`, `handler/*`) as OTP processes under
   `Exhub.LspBridge`, covering process management, capabilities, document sync,
   diagnostics and the LSP request/response features.
2. **G2 — Reuse the config model.** Vendor lsp-bridge's `langserver/*.json` and
   `multiserver/*.json` into `priv/lsp_bridge/` and load them unchanged
   (user override still honoured); project-root, language-id and multi-server
   fusion follow the same rules.
3. **G3 — ExHub idiom.** Commands arrive as `["func", ["lsp-bridge", …]]` over
   the WebSocket, are handled by a `ResponseHandlers.ExhubLspBridge` module, and
   results are pushed back as elisp via `Exhub.send_message/1`. Hot-reloadable,
   supervised like every other ExHub feature.
4. **G4 — Useful parity first.** Ship the features used interactively:
   diagnostics, hover, definition/references, document/workspace symbols,
   completion (+ resolve) and rename/code-action/format, before chasing exotic
   ones.
5. **G5 — Testable without a real language server.** A fake stdio LSP child in
   Elixir exercises the protocol/session layers; real `elixirLS` is the
   integration target.

### Non-Goals

1. **N1 — Elisp frontend rewrite.** `lsp-bridge.el` / `acm/` are referenced, not
   ported. We add a thin ExHub-native client (option B, §6) rather than moving
   8800 LOC of UI.
2. **N2 — Non-LSP completion backends in v1.** ctags, Copilot, Codeium, TabNine,
   Tabby, Citre, org-roam, SDCV backends are out of scope for the first pass
   (P5 optional).
3. **N3 — Remote/tramp/devcontainer in v1.** lsp-bridge's SSH/devcontainer file
   transport is deferred (P5).
4. **N4 — Byte-for-byte behavioural clone.** Behaviour parity for the covered
   features, not implementation parity.

## 3. Reference: how lsp-bridge works

### 3.1 Backend (Python)

| File | Role |
|------|------|
| `lsp_bridge.py` | EPC RPC surface; project/root + lang-server selection; starts/stops `LspServer`; `eval_in_emacs` callbacks |
| `core/lspserver.py` | One external LSP process: spawn, `initialize`/`initialized`, capability merge, document notifications, diagnostics routing, work-done progress, dynamic registration, watched files |
| `core/fileaction.py` | Per-open-buffer coordinator: content versions, `change_file` diffing, dispatching a `Handler` per request, diagnostics cache |
| `core/handler/*.py` | One class per LSP method (`name`, `method`, `cancel_on_change`, `process_request`, `process_response`), registered via `Handler.__subclasses__()` |
| `core/utils.py` | Paths, URIs, sexp/path helpers |
| `core/{ctags,copilot,codeium,tabnine,search_file_words,remote_file}.py` | Non-LSP backends |

Transport: EPC. Emacs calls `lsp-bridge-call-file-api` / `lsp-bridge-call-async`;
the backend calls back through `eval-in-emacs`, `get-emacs-vars`,
`get-buffer-content`, etc.

### 3.2 Feature inventory (`core/handler/*`)

`completion`, `completion_item` (resolve), `find_define`, `find_type_define`,
`find_implementation`, `find_references`, `peek` (find_define/find_references),
`hover`, `signature_help`, `prepare_rename`, `rename`, `code_action`,
`formatting`, `range_formatting`, `execute_command`, `workspace_symbol`,
`completion_workspace_symbol`, `call_hierarchy`
(incoming/outgoing/prepare), `document_symbol`, `imenu`, `inlay_hint`,
`semantic_tokens`, `workspace_diagnostics`, `diagnostic`, `breadcrumb`, plus
per-language URI resolvers (jdt/deno/csharp) and rust-specific helpers.

### 3.3 Frontend (elisp)

`lsp-bridge.el` (3270 LOC) owns buffer lifecycle, project detection and the EPC
connection; `acm/*.el` is the completion menu + backends;
`lsp-bridge-{ref,peek,imenu,diagnostic,code-action,inlay-hint,semantic-tokens,call-hierarchy,breadcrumb}.el`
are feature UIs.

### 3.4 Config

- `langserver/<name>.json`: `{name, languageId, command: [...], projectFiles:
  [...], settings, support-single-file}`.
- `multiserver/<name>.json`: `{default, completion: [...], diagnostics: [...],
  code_action: [...], …}` — maps a feature to an ordered server list.

## 4. Target: the ExHub pattern

```
Emacs (elisp)                    ExHub (Elixir/OTP)
  │  ["func",["lsp-bridge",…]]      │
  ├────────── WebSocket ──────────▶ Exhub.SocketHandler
  │                                    │
  │                          Exhub.DefaultResponseHandler
  │                                    │
  │                    Exhub.ResponseHandlers.ExhubLspBridge
  │                                    │
  │                        Exhub.LspBridge.ClientManager  (session coordinator)
  │                          ├─ Exhub.LspBridge.Session  (per project+language)
  │                          ├─ Exhub.LspBridge.Document (per buffer)
  │                          └─ Exhub.LspBridge.Server   (per LSP process → Port)
  │                                    │
  │◀──── Exhub.send_message(elisp) ────┘   (results, diagnostics, notifications)
```

Command envelope: Emacs sends `["func", ["lsp-bridge", action, …args]]`; results
come back as forms like `(exhub-lsp--completion …)`. Servers run as children of
a `DynamicSupervisor`; the whole feature is a supervisor subtree added to
`Exhub.Supervisor`, hot-reloadable.

## 5. Current state (uncommitted skeleton)

Already present under `lib/exhub/lsp_bridge/` (Phase 1 skeleton, ~930 LOC) plus
`lib/exhub/response_handlers/exhub_lsp_bridge.ex`, wired into `application.ex`
and `default_response_handler.ex`, with tests under `test/exhub/lsp_bridge/`.

| Module | Status |
|--------|--------|
| `Exhub.LspBridge.Application` | Supervisor subtree: Registry + Config + DynamicSupervisor + ClientManager |
| `Exhub.LspBridge.Protocol` | Content-Length JSON-RPC framing (encode/decode) — pure, tested |
| `Exhub.LspBridge.Config` | `langserver/*.json` loader/cacher (ETS), by name/language |
| `Exhub.LspBridge.Server` | GenServer owning one LSP `Port`; initialize handshake, id correlation, default answers to `workspace/configuration` & `registerCapability` |
| `Exhub.LspBridge.ClientManager` | Command dispatch: `ping`, `start-server`, `stop-server`, `request`, `notify` |

**Gaps**: no `multiserver` support; no project/root/language detection; no
document sync (`didOpen`/`didChange`/`didSave`/`didClose`); no diagnostics; no
capability gating; no feature handlers; no elisp frontend; `server_test.exs` is
currently **failing** (the Elixir fake LSP child never starts — see P0).

## 6. Decisions: frontend strategy, completion UI, config source

**Frontend — Option B (`exhub-lsp.el`).** A thin ExHub-native frontend following
the `exhub-fim` / `blink-search-exhub` exemplars. No EPC emulation; the
lsp-bridge Python path is replaced rather than shadowed.

**Completion UI — an acm adapter.** Rather than reusing the acm-derived
`exhub-fim-menu.el`, `exhub-lsp.el` registers an **acm backend** (e.g.
`acm-backend-exhub-lsp.el`) that sources candidates from ExHub and hands them to
`acm.el`, keeping acm's menu, icons, filtering, documentation and `resolve`.
acm becomes a dependency of the ExHub frontend; the adapter implements acm's
backend protocol directly, calling `exhub-lsp.el` instead of
`lsp-bridge-call-file-api` (we do not load `lsp-bridge.el`). Other features
(hover, locations, symbols) use small ExHub-native UIs, mirroring
`lsp-bridge-*.el`.

**Config — vendored into `priv/`.** Copy `langserver/` and `multiserver/` into
`priv/lsp_bridge/{langserver,multiserver}/`; `Exhub.LspBridge.Config` resolves
that path via `:code.priv_dir(:exhub)`, with a configurable override (and, for
development, the lsp-bridge checkout) taking precedence. This makes the port
self-contained for a release and decoupled from the lsp-bridge install path.

**Option A (EPC shim) is rejected**: it would mean faithfully emulating a
process-managing elisp layer and keeping two overlapping backends for the same
Emacs.

## 7. Architecture (Elixir)

| Module | Purpose | Maps to |
|--------|---------|---------|
| `Exhub.LspBridge.Protocol` | JSON-RPC stdio framing | `core/lspserver.py` sender/receiver |
| `Exhub.LspBridge.Config` | `priv/lsp_bridge/*.json` (langserver + multiserver) loader | `SINGLE_SERVER_INFO_DICT` |
| `Exhub.LspBridge.Project` | Root detection, language-id, single vs multi server | `find_project_root`, `load_single_lang_server`, `pick_multi_server_names` |
| `Exhub.LspBridge.Document` | Per-buffer uri/version/content + diffing | `core/fileaction.py` (content) |
| `Exhub.LspBridge.Session` | Per project+language: owns servers + documents, capability cache | `FileAction` + `LspServer.attach` |
| `Exhub.LspBridge.Server` | One LSP process (Port); handshake, capabilities, notifications, diagnostics intake | `core/lspserver.py::LspServer` |
| `Exhub.LspBridge.Handler` (behaviour) | `name`, `method`, `cancel_on_change`, `process_request/2`, `process_response/3` | `core/handler/__init__.py` |
| `Exhub.LspBridge.Handlers.*` | One module per feature (§3.2) | `core/handler/*.py` |
| `Exhub.LspBridge.Diagnostics` | publish/pull aggregation, version filtering, push | `handle_publish_diagnostics`, `pull_diagnostics` |
| `Exhub.LspBridge.MultiServer` | Method→server-list fusion from `multiserver/*.json` | `get_method_server_names` |
| `Exhub.LspBridge.ClientManager` | Command routing + Emacs callbacks | `lsp_bridge.py::LspBridge` |

### 7.1 Command surface (elisp → ExHub)

High-level verbs mirroring lsp-bridge's file-api, not raw JSON-RPC:

| Command | Args | Callback to Emacs |
|---------|------|-------------------|
| `open-file` | `path`, `content`, `language-id` | `(exhub-lsp-ready …)`, `(exhub-lsp-diagnostics …)` |
| `change-file` | `path`, `version`, `changes` | — |
| `save-file` / `close-file` | `path` | — |
| `change-cursor` | `path`, `position` | — |
| `completion` | `path`, `position`, `char`, `prefix` | `(exhub-lsp--completion id candidates)` |
| `completion-item-resolve` | `path`, `item-key` | `(exhub-lsp--completion-resolved key doc edits)` |
| `hover` / `find-define` / `find-references` / `find-type-define` / `find-implementation` | `path`, `position` | `(exhub-lsp--hover …)`, `(exhub-lsp--locations …)` |
| `prepare-rename` / `rename` | `path`, `position`, `new-name` | `(exhub-lsp--rename-result …)` |
| `code-action` / `format` | `path`, `range`/`action-kind` | `(exhub-lsp--code-action …)` |
| `document-symbol` / `workspace-symbol` | `path` / `query` | `(exhub-lsp--symbols …)` |
| `inlay-hint` / `semantic-tokens` / `signature-help` | per feature | `(exhub-lsp--… )` |
| `diagnostics` / `list-diagnostics` | `path` | `(exhub-lsp--diagnostics …)` |
| `shutdown` | `path`/`name` | — |

Raw `request`/`notify` stay available for debugging.

## 8. Phased plan

- **P0 — Stabilise the skeleton.**
  Fix the failing `server_test` (the fake Elixir LSP child invocation), vendor
  `langserver/` + `multiserver/` into `priv/lsp_bridge/` and repoint `Config`,
  add `multiserver` parsing + tests, and write `docs/modules/lsp-bridge.md`.
  Deliverable: green `mix test --no-start test/exhub/lsp_bridge/`.

- **P1 — Session, documents, lifecycle, diagnostics.** ✅ **Done (2026-10-05).**
  `Project` (root/language/single-vs-multi), `Session`, `Document`
  (open/change/save/close, versions, diffing), `Diagnostics` (publish + pull,
  version filtering), capability merge + gating, `MultiServer` fusion.
  Deliverable: live diagnostics for a real Elixir project via elixirLS.

  Implemented as: `Exhub.LspBridge.{Project, Capabilities, Document,
  Diagnostics, Elisp}` (pure) + `Exhub.LspBridge.Session` (GenServer, keyed by
  `{root, profile}`, owning its `Server`s and `Document`s) wired into a
  reworked `ClientManager` router; `Server` gained the `{:server, root, name}`
  key, capability derivation and settings-aware client responses. The Emacs
  slice is `exhub-lsp.el` (lifecycle + `flymake` diagnostics), per the frontend
  decision B and the P1 answers: front end included, `flymake`, full pull
  diagnostics. The P1 deliverable was verified end-to-end against **real
  elixirLS**: `test/exhub/lsp_bridge/e2e_test.exs` (tagged `:e2e_lsp`, excluded
  by default) opens a syntax-error file in a scratch `mix new` project and
  receives an error-severity diagnostic tagged `elixirLS`.

  Deployed 2026-10-05 (release build + hot reload, no VM restart): the live
  Emacs smoke test first exposed two crashes the `:e2e_lsp` test had **masked**.
  `Session.init/1` used `Map.get(opts, key, default)`, so the `nil` values
  `ClientManager.open_in_session/4` always inserts overrode the defaults —
  making `schedule_pull/2` call `Process.send_after/3` with `nil` (fresh
  `open_file` died, taking `ClientManager` with it) and `Diagnostics.merge/2`
  call `MapSet.new(nil)` (pushed diagnostics never reached Emacs). Fixed by
  `|| default` normalization in `Session.init/1`, a nil guard in
  `Diagnostics.merge/2`, and catching session-call exits in `ClientManager`
  (so a dying language server can't take the subtree down). A regression test
  now reproduces the exact production option shape.

- **P2 — Read-only features.** ✅ **Done (2026-10-05).**
  `Handler` behaviour + registry; hover, definition, type-definition,
  implementation, references, document symbols, workspace symbols, signature
  help. Deliverable: jump/hover/symbol commands from Emacs.

  Implemented as: `Exhub.LspBridge.Handler` (behaviour) + `Exhub.LspBridge.Handlers`
  (compile-time registry) with one pure module per feature under
  `Exhub.LspBridge.Handlers.*` (`Hover`, `Definition`, `TypeDefinition`,
  `Implementation`, `References`, `DocumentSymbol`, `WorkspaceSymbol`,
  `SignatureHelp`) plus `Handlers.Locations` for `Location`/`LocationLink`
  normalisation. Handlers stay pure (params in, payload out) and `ClientManager`
  renders the payload as an elisp form — unlike the Python `eval_in_emacs`
  handlers — so they unit-test without Emacs. `Session.perform/4` gates each
  request on the server's advertised provider (treating *unknown* capabilities
  as "still initializing" and letting the request queue), fans it out per
  `MultiServer`, correlates the response and drops it when `cancel_on_change?`
  and the document changed. Emacs commands (`exhub-lsp-hover`,
  `exhub-lsp-find-definition`, `-find-type-definition`, `-find-implementation`,
  `-find-references`, `-document-symbols`, `-workspace-symbols`,
  `-signature-help`) receive `(exhub-lsp--hover …)`, `(exhub-lsp--locations …)`,
  `(exhub-lsp--symbols …)`, `(exhub-lsp--workspace-symbols …)` and
  `(exhub-lsp--signature-help …)`; navigation reuses the `xref` UI, hover renders
  in `*exhub-lsp-hover*`, document symbols populate `imenu`. Verified end-to-end
  against real elixirLS (definition jump, hover markdown, imenu symbols) and
  deployed by release + hot reload (no VM restart).

- **P3 — Completion + resolve.** ✅ **Done (2026-10-05).**
  `completion` and `completion_item` handlers, candidate scoring/sorting (parity
  with `core/handler/completion.py`), exposed to Emacs and surfaced through the
  **acm adapter** (`acm-backend-exhub-lsp.el`) + `exhub-lsp.el`.
  Deliverable: interactive acm completion.

  Implemented as: `Exhub.LspBridge.Handlers.Completion` (kind filter,
  `string_match` fuzzy/substring, snippet→yas conversion, prefix/score/`sortText`
  sort, top-N, FNV-1a candidate keys) and `Handlers.CompletionItem`
  (`completionItem/resolve` documentation/edits) — both pure `Handler`s in the
  registry. `Session.do_perform/4` gained `handler_targets/3` (an optional
  `server` argument pins resolve to the originating server) and passes
  `version`/`triggerCharacters`/server names to handlers; `completion` fans out
  per server and returns candidates plus the raw items keyed by candidate `key`.
  `ClientManager` dispatches `completion` / `completion-item-resolve` and renders
  `(exhub-lsp--completion …)` / `(exhub-lsp--completion-doc …)`. On the Emacs
  side, `acm-backend-exhub-lsp.el` is an acm backend adapter that fills
  `acm-backend-lsp-items` (transformed candidates) and `exhub-lsp--raw-items`
  (raw items for resolve). Verified against real elixirLS (completion returned
  the expected candidate for a scratch project) and deployed by release + hot
  reload (0 errors, no VM restart); `mix test --no-start test/exhub/lsp_bridge/`
  → 99 tests, 0 failures.

- **P4 — Edits & advanced.** ✅ **Done (2026-10-05).**
  `prepare_rename`/`rename`, `code_action`, `formatting`/`range_formatting`,
  `execute_command`, `inlay_hint`, `semantic_tokens`, `call_hierarchy`,
  `document_symbol`/`imenu` surfaces, `breadcrumb`.

  - **P4a — Edits.** ✅ **Done (2026-10-05).** `PrepareRename`, `Rename`,
    `Formatting`, `RangeFormatting`, `CodeAction` and `ExecuteCommand` handlers
    (pure, in the registry); `Capabilities` gained `call_hierarchy` +
    `execute_command` provider paths and `Handler.provider/0` may now be `nil`
    to skip capability gating (`execute-command`). `Session` passes the
    document's cached diagnostics into the handler ctx (so `code-action` can fill
    `context.diagnostics` for the request range) and `ClientManager` renders
    `(exhub-lsp--rename-range …)`, `(exhub-lsp--workspace-edit …)`,
    `(exhub-lsp--format …)` and `(exhub-lsp--code-actions …)`. On the elisp
    side `exhub-lsp.el` gained rename / format / format-region / code-action
    commands (`C-c C-r` / `C-c C-f` / `C-c C-a`), TextEdit/WorkspaceEdit
    application helpers and the code-action prompt. `edits_test.exs` covers the
    new handlers; `mix test --no-start test/exhub/lsp_bridge/` → 120 tests, 0
    failures.
  - **P4b — Call hierarchy.** ✅ **Done (2026-10-05).** `CallHierarchyPrepare`,
    `CallHierarchyIncoming` and `CallHierarchyOutgoing` handlers (registry);
    `ClientManager` renders `(exhub-lsp--call-hierarchy-items …)` /
    `(exhub-lsp--call-hierarchy path direction …)`; `MultiServer` gained aliases
    for the three methods. Two-step elisp flow: `call-hierarchy-prepare` →
    pick the item → `call-hierarchy-incoming`/`-outgoing` (the item, including
    server-private `data`, is round-tripped verbatim). Elixir front end:
    `exhub-lsp-call-hierarchy-incoming`/`-outgoing` (`C-c C-i`/`C-c C-o`).
    Covered by `call_hierarchy_test.exs` and fake-LSP `session_test.exs` cases.
  - **P4c — Decorations.** ✅ **Done (2026-10-05).** `InlayHint` and
    `SemanticTokens` handlers (`textDocument/inlayHint` /
    `textDocument/semanticTokens/full`; registry). `SemanticTokens.decode/2`
    expands the LSP delta encoding to absolute `{line, character, length, type,
    modifiers}` maps against the legend, which `Session.semantic_token_legend/2`
    pulls from the first target advertising one and threads into the handler ctx
    (`semantic_tokens_legend`). `ClientManager` renders
    `(exhub-lsp--inlay-hints …)` / `(exhub-lsp--semantic-tokens …)`. The elisp
    front end adds two toggle minor modes — `exhub-lsp-inlay-hints-mode`,
    `exhub-lsp-semantic-tokens-mode` — that request on a 0.4s idle timer after
    changes, paint overlays (`after-string` hints / `face` tokens) and clear
    them (and their timers) when disabled or when the buffer closes. Off by
    default. Covered by `inlay_hint_test.exs`, `semantic_tokens_test.exs` and
    fake-LSP `session_test.exs` cases (`mix test --no-start
    test/exhub/lsp_bridge/` → 153 tests, 0 failures). Note: elixirLS does not
    advertise either provider, so the real-server path is limited to
    `formatting`/`code-action`.
  - **Deferred.** `breadcrumb` (derivable from document symbols + imenu).

- **P5 — Optional parity.**
  Remote/tramp + devcontainer transport, watched files, ctags and AI completion
  backends, `lsp-bridge-get-project-path-by-filepath`-style customization.

## 9. Decisions

**Resolved:**

1. **Frontend strategy — B** (`exhub-lsp.el`).
2. **Completion UI — acm adapter** (`acm-backend-exhub-lsp.el`).
3. **Config source — vendored into `priv/lsp_bridge/`**, with an override.

**Defaulted (confirm if you disagree):**

4. **API level** — expose the high-level verbs in §7.1 rather than only raw
   `request`/`notify`.
5. **Document sync** — incremental `didChange` with Emacs authoritative (as
   lsp-bridge); full-text behind a fallback flag.
6. **Multi-server** — implemented in P1 (handlers depend on the method→server
   mapping).
7. **Process lifetime & hot-reload** — servers are long-lived children of the
   `DynamicSupervisor`; reload only swaps BEAM code (new children need the
   supervision-tree recipe in `AGENTS.md`).

## 10. Risks

- **Multi-server fusion + capability gating** is fiddly and load-bearing for
  every handler — get it right in P1.
- **Hot-reloading while language-server children hold state** — reload timing and
  supervision.
- **elixirLS availability** (`language_server.sh`) — verify in the target env
  before P1 integration testing.
- **Option A's EPC emulation** is subtle if we ever need it.

## 11. Testing

- Pure modules (`Protocol`, `Config`, `MultiServer`, handler
  request/response shaping) unit-tested with `mix test --no-start`.
- A fake stdio LSP child written in Elixir drives `Server`/`Session`
  end-to-end without an external dependency (fix the P0 test).
- Integration against real `elixirLS` on a scratch mix project for P1+.
- Elisp side (`exhub-lsp.el` + the acm adapter) exercised manually via the
  running Emacs (hot-reload the `.el`).

## 12. References

- lsp-bridge: `~/.emacs.d/site-lisp/lsp-bridge/` (`lsp_bridge.py`,
  `core/lspserver.py`, `core/fileaction.py`, `core/handler/*`,
  `langserver/*.json`, `multiserver/*.json`).
- ExHub exemplars: `docs/modules/fim.md`, `docs/modules/blink-search.md`,
  `lib/exhub/fim/`, `lib/exhub/blink_search/`.
- Deploy/hot-reload: `AGENTS.md`.