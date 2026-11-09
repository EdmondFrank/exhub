# exhub-file

The `exhub-file` package provides file operations for Emacs using Exhub.

## Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-file)
```

## Usage

### Markdown Rendering

- `exhub-preview-markdown`: Preview the current buffer with markdown using Exhub.

### Mermaid Diagrams

Markdown previews render ```` ```mermaid ```` code fences server-side with
[MDExMermex](https://mdex-mermex.hexdocs.pm/readme.html) (Mermex Rust NIF) and
embed each diagram as a base64 SVG image, so EWW/`shr` shows the drawing
without any client-side JavaScript. The plugin's browser toolbar is stripped
for the Emacs preview; zoom/pan/fullscreen are not available there.

If a diagram cannot be rendered (for example an invalid Mermaid body), the
handler falls back to plain Markdown rendering and the fence stays a code
block, so one broken diagram never breaks the whole preview.
