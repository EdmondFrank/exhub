# Blink Search

Blink Search is an incremental, multi-backend searcher for Emacs, powered by the Exhub Elixir server. It is an Elixir/OTP port of the original [blink-search](https://github.com/manateelazycat/blink-search) package — the Python/EPC backend is replaced by Elixir processes communicating over Exhub's WebSocket connection.

## Overview

Instead of a Python subprocess, all search coordination runs inside the Exhub BEAM:

- Concurrent per-backend searches via Erlang/Elixir tasks (true parallelism)
- Stale-result suppression via a search ticker (only the latest keyword's results render)
- Results stream back to Emacs as elisp payloads and render progressively

## Components

### Emacs Frontend (`blink-search-exhub.el`)

Window layout, input monitoring, rendering, keybindings, and data sync (buffer list, recent files, imenu, elisp symbols) pushed to the server over WebSocket.

### Elixir Backend (`lib/exhub/blink_search/`)

| Module                                    | Role                                                                                                                                                      |
|-------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------|
| `Exhub.BlinkSearch.Server`                | GenServer coordinator (supervision tree child): dispatches searches, aggregates results, executes actions                                                 |
| `Exhub.BlinkSearch.Renderer`              | Pure pagination/aggregation logic mirroring upstream's `BlinkSearch` class                                                                                |
| `Exhub.BlinkSearch.Backend`               | Behaviour (`search_match`, `do_action`, `copy`, `parent`, `select`, `continue_search`, …) + shared helpers (fuzzy regex, rg JSON parsing, process runner) |
| `Exhub.ResponseHandlers.ExhubBlinkSearch` | WebSocket dispatch for `["blink-search", action, ...]` messages                                                                                           |

## Available Backends

| Backend          | Source                                | Notes                                                                          |
|------------------|---------------------------------------|--------------------------------------------------------------------------------|
| History          | `~/.emacs.d/blink-search/history.txt` | Replays previous actions; delegates to the recorded backend                    |
| Buffer List      | Pushed from Emacs                     | `*` / ` *` / trailing `*` prefix filters (special / hidden / modified)         |
| Common Directory | `blink-search-exhub-common-directory` | Alias-prefixed entries (e.g. `HOME src`)                                       |
| Find File        | `fd --regex --full-path`              | Git project root of the start directory; empty prefix lists directory contents |
| Recent File      | Pushed from Emacs (`recentf-list`)    |                                                                                |
| IMenu            | Pushed from Emacs                     | Jumps to imenu positions in the start buffer                                   |
| Elisp Symbol     | Idle-synced obarray + local bindings  | Commands run, functions/variables described, faces customized                  |
| Google Suggest   | Google complete API                   | Honors `Application.get_env(:exhub, :proxy)`; detects URLs                     |
| Key Value        | `~/.emacs.d/blink-search-kv.txt`      | `set key value` / `del key` commands; plain key copies its value               |
| Grep File        | `rg --json`                           | Content search under project root (`!` prefix)                                 |
| Grep PDF         | `rga --json`                          | Multi-directory PDF content search (`;` prefix); `$D0/$D1` path compression    |
| Current Buffer   | `rg --json` on temp copy              | Line/column navigation in the start buffer (`#` prefix)                        |
| PDF              | `rga --json` on current PDF           | Single-file mode (`:` prefix)                                                  |
| Envless          | `envless list` (`ENVLESS_ROOT` vault) | Key names only (`$` prefix); action copies the key's value to the clipboard    |
| OTP              | `cotp list --json`                    | Pass from SecretVault (`cotp_pass`); `?` prefix; action copies the code to the clipboard |

Default backends (no explicit list): History, Buffer List, Common Directory, Find File, Recent File, IMenu, Elisp Symbol, Google Suggest, Key Value.

## Usage

```elisp
(add-to-list 'load-path (expand-file-name "site-lisp/exhub" user-emacs-directory))
(require 'blink-search-exhub)

;; Start a search (C-u: seed with symbol at point; region: seed with region text)
M-x blink-search-exhub
```

Search prefixes typed into the input window:

| Prefix | Scope                |
|--------|----------------------|
| (none) | All default backends |
| `#`    | Current Buffer only  |
| `!`    | Grep File only       |
| `;`    | Grep PDF only        |
| `:`    | PDF only             |
| `$`    | Envless only         |
| `?`    | OTP only             |

## Keybindings

| Key                   | Action                                                               |
|-----------------------|----------------------------------------------------------------------|
| `C-g` / `ESC ESC ESC` | Quit and restore window layout                                       |
| `C-n` / `C-p`         | Next / previous candidate                                            |
| `M-n` / `M-p`         | Next / previous item within focused backend                          |
| `M-j` / `M-k`         | Jump to next / previous backend group                                |
| `C-m`                 | Execute action for selected candidate                                |
| `C-M-m`               | Preview selected candidate                                           |
| `C-M-n` / `C-M-p`     | Preview next / previous candidate                                    |
| `C-j`                 | Navigate to parent context                                           |
| `C-l`                 | Continue search inside subdirectory                                  |
| `M-w`                 | Copy candidate text                                                  |
| `M-<quick-key>`       | Directly execute Nth candidate (see `blink-search-exhub-quick-keys`) |

## Configuration

```elisp
(defcustom blink-search-exhub-search-backends nil
  "Restrict default backends. Nil means all defaults.")

(defcustom blink-search-exhub-common-directory '(("HOME" "~/"))
  "Alias/directory pairs for the Common Directory backend.")

(defcustom blink-search-exhub-grep-pdf-search-paths nil
  "Directories searched by the Grep PDF backend. Nil means current directory.")

(defcustom blink-search-exhub-history-path
  (expand-file-name (concat user-emacs-directory "blink-search" "/history.txt"))
  "Path to store search history.")

(defcustom blink-search-exhub-flash-line-delay 0.3
  "Seconds to flash the target line after navigation.")

(defcustom blink-search-exhub-elisp-symbol-update-idle 5
  "Idle seconds between elisp symbol synchronization.")

(defcustom blink-search-exhub-envless-root (expand-file-name "~/GTD")
  "ENVLESS_ROOT vault directory for the Envless backend.")

(defcustom blink-search-exhub-envless-env "dev"
  "Envless environment name for the Envless backend.")

(defcustom blink-search-exhub-cotp-pass-key "cotp_pass"
  "SecretVault secret name holding the cotp database password.")

(defcustom blink-search-exhub-cotp-db-path nil
  "Optional cotp database path. Nil uses cotp's default.")
```

## External Tools

Optional, per backend:

- `rg` (ripgrep) — Grep File, Current Buffer
- `fd` or `fdfind` — Find File
- `rga` (ripgrep-all) — Grep PDF, PDF
- `envless` — Envless (vault access via `ENVLESS_ROOT`)
- `cotp` — OTP (`brew install cotp`)
- `pbcopy` — clipboard sink for the Envless action (macOS)

Missing tools degrade gracefully: the affected backend returns no candidates.

## Secrets

The OTP backend reads the cotp database password from SecretVault — never from
the command line or a plaintext file:

```sh
# One-time: encrypt the existing envless COTP_PASS as SecretVault `cotp_pass`
ENVLESS_ROOT=~/GTD envless exec -- sh -c 'MIX_ENV=prod mix run --no-start -e "
  {:ok, c} = SecretVault.Config.fetch_from_current_env(:exhub)
  :ok = SecretVault.put(c, \"cotp_pass\", System.get_env(\"COTP_PASS\"))"'
```

`exhub_reload_keys` makes it live at runtime (`Exhub.Router.Config.reload_from_scr/0`
refreshes the `:persistent_term` copy), so no VM restart is needed. Never print
an envless value or an OTP code: the Envless and OTP actions only ever copy to
the clipboard.

## Hot Reload

Both sides support zero-downtime updates without restarting Emacs or the ExHub VM:

- Elixir: `exhub_hot_reload` MCP tool (see [AGENTS.md](../../AGENTS.md))
- Elisp: `emacsclient -e '(load-file "~/.emacs.d/site-lisp/exhub/blink-search-exhub.el")'`

## Testing

```sh
mix test --no-start test/exhub/blink_search/
mix test --no-start test/exhub/response_handlers/exhub_blink_search_test.exs
```
