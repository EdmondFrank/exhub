# exhub-fim

The `exhub-fim` package provides LLM-powered code completion with dual modes: specialized prompts and various enhancements for chat-based LLMs on code completion tasks, and fill-in-the-middle (FIM) completion for compatible models.

## Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-fim)
```

## Usage

### Code Completion

- `exhub-fim-show-suggestion`: Show code suggestion using overlay at point.
- `exhub-fim-next-suggestion`: Cycle to next suggestion.
- `exhub-fim-previous-suggestion`: Cycle to previous suggestion.
- `exhub-fim-accept-suggestion`: Accept the current overlay suggestion.
- `exhub-fim-dismiss-suggestion`: Dismiss the current overlay suggestion.
- `exhub-fim-accept-suggestion-line`: Accept N lines of the current suggestion.
- `exhub-fim-complete-with-minibuffer`: Complete using minibuffer interface.

### Automatic Suggestion

- `exhub-fim-auto-suggestion-mode`: Toggle automatic code suggestions.

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
