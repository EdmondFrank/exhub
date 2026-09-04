# exhub-fim

The `exhub-fim` package provides LLM-powered code completion with dual modes: specialized prompts and various enhancements for chat-based LLMs on code completion tasks, and fill-in-the-middle (FIM) completion for compatible models. Completions are also collected from the words
of every open buffer file, see [Cross-buffer Word Candidates](#cross-buffer-word-candidates).

## Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-fim)
```

## Usage

### Code Completion

- `exhub-fim-show-suggestion`: Show code suggestion at point, presented with
  `exhub-fim-menu-display-function`.
- `exhub-fim-complete`: Request completions and pick one from the dropdown menu
  at point (minibuffer on a text terminal).
- `exhub-fim-next-suggestion`: Cycle to next suggestion.
- `exhub-fim-previous-suggestion`: Cycle to previous suggestion.
- `exhub-fim-accept-suggestion`: Accept the current suggestion.
- `exhub-fim-dismiss-suggestion`: Dismiss the current suggestion.
- `exhub-fim-accept-suggestion-line`: Accept N lines of the current suggestion.
- `exhub-fim-complete-with-minibuffer`: Complete using minibuffer interface.

### Dropdown Menu

`exhub-fim-menu-display-function` (default `dropdown`) controls how candidates
are presented: a candidate menu popped up at point in a child frame, inline
ghost text, or the minibuffer. With a menu, the selected candidate is previewed
as ghost text (`exhub-fim-menu-preview`). While the menu is visible:

| Key | Action |
|-----|--------|
| `M-n` / `M-p`, `<down>` / `<up>` | Next / previous candidate (wraps) |
| `M->` / `M-<` | Last / first candidate |
| `C-v` / `M-v` | Next / previous page |
| `TAB` / `M-RET`, or mouse click | Accept the candidate |
| `C-g` / `M-l` | Dismiss |

Options: `exhub-fim-menu-max-items` (rows per page, default 10) and
`exhub-fim-menu-max-width` (candidate truncation, default 60). The menu hides
itself when the cursor moves, the window scrolls, or the buffer is killed, and
defers to the lsp-bridge completion menu when it is open.

### Automatic Suggestion

- `exhub-fim-auto-suggestion-mode`: Toggle automatic code suggestions.

### Cross-buffer Word Candidates

Alongside the answers the LLM returns, `exhub-fim` completes the words of every
open buffer file, as lsp-bridge's search-words backend
(`lsp-bridge-enable-search-words`) does. The candidates are merged into the same
list, so they appear in the dropdown, as ghost text and in the minibuffer, and
they still show up when the provider is slow, errors out, or answers with
nothing.

| Option | Default | Purpose |
|--------|---------|---------|
| `exhub-fim-enable-word-candidates` | `t` | Complete words from the open buffers |
| `exhub-fim-word-candidates-max-number` | `10` | Candidates added per completion |
| `exhub-fim-word-candidates-min-prefix-length` | `1` | Length the symbol at point must reach first |
| `exhub-fim-word-candidates-min-word-length` | `4` | Words this short, and numbers, are never indexed |
| `exhub-fim-word-candidates-scope` | `all-buffers` | `same-mode` restricts collection to buffers in the current `major-mode` |
| `exhub-fim-word-candidates-max-buffers` | `20` | Buffers searched, taken in most-recently-used order |
| `exhub-fim-word-candidates-max-buffer-chars` | `100000` | Bigger buffers are left out |
| `exhub-fim-word-candidates-prohibit-file-extensions` | `("png" "jpg" "jpeg" "gif" "pdf")` | Extensions that never contribute words |

A candidate carries the whole word, so the dropdown labels it with the word and
annotates it `Word` while only the missing tail is inserted after point; that is
also why accepting a word candidate is no different from accepting any other.
Matching ignores case and retries on the last `-` or `_` separated segment of
what was typed (a segment of at least three characters), so `exhub-widget` still
completes to `widget-factory-size`.

The words of a buffer are collected once and reused until the buffer changes.
An idle timer (`exhub-fim--word-index-refresh-delay`, 0.3s) does the collecting
for at most `exhub-fim--word-index-time-budget` seconds (0.1s) per pass and stops
itself when everything is current, so typing never waits for a scan; a completion
request only reads what is already collected. The exception is the first request
of a session, which fills the empty index inline, and buffers opened since the
last completion are searched by the next one.

Every open file contributes its words, notes and logs included, which is what
makes an identifier used only in another file complete. To keep the candidates
to the code being written, set `exhub-fim-word-candidates-scope` to `same-mode`,
add the extensions to leave out to
`exhub-fim-word-candidates-prohibit-file-extensions`, or lower
`exhub-fim-word-candidates-max-buffers`; buffers over
`exhub-fim-word-candidates-max-buffer-chars` (log dumps, transcripts) are already
left out.

### Provider Configuration

- `exhub-fim-configure-provider`: Configure a exhub-fim provider interactively.

## Asynchronous Elixir Backend (FIM Providers)

The `codestral` and `openai-fim-compatible` providers do **not** send HTTP
requests from Emacs anymore. Instead they are routed through the ExHub
WebSocket (like `blink-search-exhub`): Emacs sends a `["func", ["exhub-fim",
"complete", …]]` command, `Exhub.Fim.Server` runs the completion requests
concurrently on the Elixir side, and results are pushed back to Emacs as elisp
payloads evaluated over the WebSocket.

- Requires the ExHub WebSocket connection (`exhub.el`); Emacs is never blocked
  on the LLM request.
- Chat-based providers (`openai`, `claude`, `gemini`,
  `openai-compatible`) keep the original in-Emacs request path unchanged.
- Provider configuration (`:model`, `:end-point`) still comes from the Emacs
  `exhub-fim-*-options`; the API key is resolved on the Elixir side, in order:
  1. an explicit `:api-key` option (used by `exhub-fim-configure-provider`),
  2. the `:exhub, :llms` entry (`codestral/codestral-latest` for Codestral),
  3. `Application.get_env(:exhub, :codestral_api_key)` / `:deepseek_api_key`,
  4. the `CODESTRAL_API_KEY` / `DEEPSEEK_API_KEY` environment variable.
- Timeout for FIM requests defaults to 60s on the Elixir side (the old 3s
  Emacs streaming timeout does not apply to this path).
- Remote endpoints are dialled through the shared ExHub egress proxy
  (`:exhub, :proxy`, the same setting the router's proxy routes use) when one is
  configured — `codestral.mistral.ai` is unreachable without it on some
  networks. Machine-local endpoints (Ollama, llama.cpp) always stay direct.
- Streamed answers come back in two chunk shapes and both are decoded:
  `choices[0].delta.content` (Codestral streams `chat.completion.chunk` objects)
  and `choices[0].text` (DeepSeek-style FIM endpoints).
- Cancellation: dismissing a suggestion (or cursor move) sends
  `["func", ["exhub-fim", "cancel", request-id]]`, killing in-flight tasks.

## Using a Custom Gemini Proxy

If you are running the Elixir proxy server locally (default port 9069), set the Gemini provider to use the proxy endpoint:

```elisp
(setq exhub-fim-provider 'gemini)
;; The default :end-point in exhub-fim-gemini-options is already
;; "http://localhost:9069/google/v1/models", so no further change is needed.
```

Ensure the environment variable `GEMINI_API_KEY` is exported in the shell that launches Emacs:

```bash
export GEMINI_API_KEY="your-gemini-key"
```
