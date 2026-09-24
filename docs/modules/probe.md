# Exhub Probe — WebSocket Code Search

Exhub Probe is a code-search frontend for Emacs backed by the Exhub Elixir server. It replaces the subprocess transport of [probe.el](https://github.com/probelabs/probe) with Exhub's existing WebSocket connection while keeping probe.el's UX shape (per-query+directory buffers, file collapse, syntax highlighting, `RET` to jump).

All three `search_files` modes are exposed — `semantic` (probe-backed, AST-aware BM25), `glob`, and `content` — through a single request/response channel.

## Overview

Instead of spawning a search process from Emacs, the frontend sends a WebSocket command and the Exhub BEAM runs the search in-process:

- Emacs sends `(exhub-call "exhub-search" ACTION REQ-ID PARAMS)`.
- `Exhub.ResponseHandlers.ExhubSearch` calls `BuiltInRegistry.call_tool("desktop", "search_files", PARAMS)` — the built-in `desktop` MCP server in the same VM (no HTTP loop or session handshake).
- The result is pushed back as evaluable elisp: `(exhub-probe--receive REQ-ID IS-ERROR JSON)`.

The registry call runs inside the handler `Task` started by `Exhub.SocketHandler`, so a multi-second probe run never blocks the Cowboy process. `Exhub.MCP.Hub.ClientManager` is deliberately not used: it is a GenServer that would serialize the whole search behind one call.

## Components

| Module / File                            | Role                                                                                          |
|------------------------------------------|-----------------------------------------------------------------------------------------------|
| `exhub-probe.el`                         | Emacs frontend — commands, per-query+dir result buffers, rendering, collapse, navigation      |
| `Exhub.ResponseHandlers.ExhubSearch`     | WebSocket handler for `["exhub-search", action, req_id, params]`; runs `search_files` in-VM    |
| `Exhub.DefaultResponseHandler`           | Dispatches `exhub-search` messages to the probe handler                                       |
| `Exhub.MCP.Tools.Desktop.SearchFiles`    | The `desktop` `search_files` tool that performs the actual search (see [desktop.md](desktop.md)) |

## Actions

| Frontend search type | WebSocket action | `search_files` `search_type` | Result rendering                                   |
|----------------------|------------------|------------------------------|----------------------------------------------------|
| semantic (default)   | `"search"`       | `"semantic"`                 | Parsed into sections (file header + highlighted code) |
| glob                 | `"glob"`         | `"glob"`                     | TOON-decoded `results` list of relative paths      |
| content              | `"content"`      | `"content"`                  | TOON-decoded `results`, grouped per file           |

The handler always sends an explicit `search_type`, and for semantic search an explicit `filter` (default `true`) — the frontend never relies on the server-side default, which is off in the test environment. `glob`/`content` results are TOON-decoded by the handler so Emacs can render structured output instead of parsing TOON itself; semantic results stay plain text (the `Pattern:`/`---`/`File: … (symbol, lines A-B)` render) and are parsed in elisp.

### Reply payload

The handler replies with a single-line JSON object:

```json
{"ok": true, "text": "…", "data": null, "error": null, "reqId": 7}
```

- `ok` — `true` on success, `false` on error.
- `text` — the raw tool text (nil on error).
- `data` — structured results for `glob`/`content` (nil for `semantic`).
- `error` — error message when `ok` is `false`.
- `reqId` — the request id echoed back.

Replies for stale or superseded requests are dropped (`exhub-probe--pending-id`), so a slow earlier search cannot override a newer one.

## Usage

```elisp
(add-to-list 'load-path (expand-file-name "site-lisp/exhub" user-emacs-directory))
(require 'exhub-probe)

;; Semantic search (query + optional purpose)
M-x exhub-probe-search
```

Requires `exhub.el` (the WebSocket client) and an Exhub build with the `exhub-search` response handler.

### Commands

| Command                     | Description                                                       |
|-----------------------------|-------------------------------------------------------------------|
| `exhub-probe-search`        | Semantic search (query + optional natural-language purpose)       |
| `exhub-probe-at-point`      | Semantic search for the symbol at point                           |
| `exhub-probe-region`        | Semantic search for the active region                             |
| `exhub-probe-glob`          | Glob search for file/directory paths                              |
| `exhub-probe-content`       | Content (ripgrep) search                                          |
| `exhub-probe-dir`           | Change the search directory, then rerun                           |
| `exhub-probe-parent-dir`    | Rerun in the parent of the current search directory               |
| `exhub-probe-rerun`         | Rerun the last search                                             |

`probe-search` and `probe-query` are aliased to `exhub-probe-search` when probe.el has not already defined them.

The search root defaults to the `project.el` root, falling back to the enclosing `.git`, then `default-directory`.

### Result Buffer Keybindings

| Key   | Action                                    |
|-------|-------------------------------------------|
| `RET` | Visit the file (and line) at point        |
| `n` / `p` | Next / previous line                  |
| `TAB` | Collapse / expand the file section at point |
| `g`   | Rerun the current search                  |
| `s`   | New search                                |
| `t`   | Toggle inclusion of test files, then rerun |
| `f`   | Toggle the Smart Decide filter, then rerun (semantic only) |
| `r`   | Select the display-only reranker          |
| `D`   | Change the search directory               |
| `^`   | Search the parent directory               |
| `C`   | Toggle the configuration block            |
| `q`   | Quit the window                           |

## Configuration

```elisp
(defcustom exhub-probe-max-buffers 10
  "Maximum number of live `exhub-probe-mode' result buffers (nil = no limit).")

(defcustom exhub-probe-include-tests nil
  "Whether new searches include test files by default.")

(defcustom exhub-probe-filter t
  "Whether new semantic searches run the Smart Decide relevance filter.")

(defcustom exhub-probe-reranker 'bm25
  "Reranker shown in the state summary (display-only).")

(defcustom exhub-probe-max-results nil
  "Optional maximum results (`max_results' param; nil = tool default).")

(defcustom exhub-probe-show-config nil
  "Whether new result buffers show the configuration block initially.")
```

The frontend maps these onto the `search_files` parameters (`allow_tests`, `filter`, `purpose`, `max_results`); `exhub-probe-reranker` is display-only because `search_files` exposes no reranker parameter — changing it only updates the state summary.

## Caveats

- **Semantic filter default is explicit.** `f` toggles `filter`, and every semantic request carries it, because the server-side default is off in `config/test.exs`.
- **Unrecognized semantic output falls back to raw.** Without the Smart Decide filter the tool returns probe's own render (whose `File:` lines carry no line range); that output is shown verbatim rather than half-parsed.
- **One search per request channel.** Concurrent searches in different result buffers are separate requests; a newer search in the *same* buffer supersedes the previous one.

## Hot Reload

Both sides support zero-downtime updates:

- Elixir: `exhub_hot_reload` MCP tool (see [AGENTS.md](../../AGENTS.md)).
- Elisp: `emacsclient -e '(load-file "~/.emacs.d/site-lisp/exhub/exhub-probe.el")'` — an active session holds old closures, so quit it (`C-g`) and start a fresh search.

## Testing

There is currently no test file for `Exhub.ResponseHandlers.ExhubSearch` or `exhub-probe.el`. When adding one, follow the WebSocket-handler convention used by `test/exhub/response_handlers/exhub_blink_search_test.exs`:

```sh
mix test --no-start test/exhub/response_handlers/
```