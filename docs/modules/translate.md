# exhub-translate

The `exhub-translate` package provides translation functionality for Emacs using Exhub.

## Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-translate)
```

## Custom Translation Model

By default, translations use the default LLM model configured for Exhub. A
dedicated model can be assigned to the `exhub-translate` module:

- **Config file** (`config.exs` / `runtime.exs` / env-specific config):
  ```elixir
  config :exhub, translate_llm: "codestral/codestral-latest"
  ```
- **Environment variable** (`EXHUB_TRANSLATE_LLM`), read at runtime, e.g.:
  ```sh
  EXHUB_TRANSLATE_LLM=openai/deepseek-v4-flash
  ```

The value must be a model name from the Exhub `llms` config map (see
`Exhub.LLMModels.build_llms_config/1`). When no custom model is set — or the
configured name is not a valid LLM name — the default LLM model is used, so
existing behavior is unchanged.

## Usage

### Insert Translations

- `exhub-translate-insert`: Insert translation based on the current mode.
- `exhub-translate-insert-original-translation`: Insert original translation.
- `exhub-translate-insert-with-line`: Insert translation with line style.
- `exhub-translate-insert-with-underline`: Insert translation with underline style.
- `exhub-translate-insert-with-camel`: Insert translation with camel case style.

### Replace Translations

- `exhub-translate-replace`: Replace the current symbol with its English translation.
- `exhub-translate-replace-with-line`: Replace with line style.
- `exhub-translate-replace-with-underline`: Replace with underline style.
- `exhub-translate-replace-with-camel`: Replace with camel case style.
- `exhub-translate-replace-zh`: Translate and replace the selected region to Chinese.

### Fix Grammar

- `exhub-translate-fix-grammar`: Fix grammar and spelling errors in the selected region (or symbol at point). Removes the original text and replaces it with the corrected version — minimal changes only, preserving the original meaning, tone, style, and language (inspired by the "Fix Grammar & Spelling" mode of the Ai-rewrite extension). Uses the same `:exhub, :translate_llm` model selection as translations.

### Posframe Translation

- `exhub-translate-posframe`: Show translation in a posframe.
