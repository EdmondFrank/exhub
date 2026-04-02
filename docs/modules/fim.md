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
