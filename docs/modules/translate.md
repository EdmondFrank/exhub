# exhub-translate

The `exhub-translate` package provides translation functionality for Emacs using Exhub.

## Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-translate)
```

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

### Posframe Translation

- `exhub-translate-posframe`: Show translation in a posframe.
