# exhub-file

The `exhub-file` package provides file operations for Emacs using Exhub.

## Setup

Add the following to your Emacs configuration file (e.g., `~/.emacs.d/init.el`):
```elisp
(require 'exhub-file)
```

## Usage

### Markdown Preview

- `exhub-preview-markdown`: Preview the buffer, or the active region, with
  markdown using Exhub. The active region takes precedence over the whole
  buffer, so a snippet can be previewed without copying it elsewhere.
- `exhub-preview-markdown-in-eww`: Preview in a dedicated EWW buffer (same as
  `exhub-preview-markdown` with a prefix argument). The buffer keeps the
  selected window: the preview opens in a side window, so you can keep editing
  while it is visible. Bound to `C-c C-v` in `markdown-mode` buffers
  (`markdown-mode-map`, so `gfm-mode` and the other derived modes inherit it).

The preview is shown in a posframe near point. The frame is sized from the
rendered content — a wide or tall Mermaid diagram gets the room it needs — and
is dismissed by the next key press. Where posframe cannot work (terminal
Emacs, no graphical frame) or when `exhub-markdown-preview-use-posframe` is
nil, the preview opens in a reusable `*eww-<buffer>*` buffer instead.

The EWW preview never replaces the buffer you are editing unless you ask for
it. The buffer is displayed through `display-buffer` according to
`exhub-markdown-preview-eww-display` (a right side window by default), and a
window already showing the preview is reused, so previewing again just
updates its content. It is rendered *after* being displayed, so `shr` wraps
text and scales Mermaid images to the width of the preview window. Press `q`
in the preview window to delete it and bury the buffer.

Customization (`M-x customize-group RET exhub-file`):

| Option | Default | Meaning |
|--------|---------|---------|
| `exhub-markdown-preview-use-posframe` | `t` | Preview in a posframe rather than an EWW buffer. |
| `exhub-markdown-preview-posframe-width` | `84` | Frame width in characters. Markdown is re-wrapped to this width and Mermaid images are clamped to it, so nothing spills out of the frame. |
| `exhub-markdown-preview-posframe-max-height` | `0.7` | Height cap — a ratio of the frame height or an integer number of lines. Taller content is clipped: the posframe does not scroll. |
| `exhub-markdown-preview-eww-display` | `side-window` | How the EWW preview buffer is displayed: `side-window` (right side window, 45% wide), `bottom` (full-width window at the bottom, 40% high), `popup` (split below the selected window), or `switch` (switch the selected window to the preview buffer, the historical behavior). |

### Mermaid Diagrams

Markdown previews render ```` ```mermaid ```` code fences server-side with
[MDExMermex](https://mdex-mermex.hexdocs.pm/readme.html) (Mermex Rust NIF) and
embed each diagram as a base64 SVG image, so EWW/`shr` and the posframe show
the drawing without any client-side JavaScript. The plugin's browser toolbar is
stripped for the Emacs preview; zoom/pan/fullscreen are not available there.

`shr` only rescales images for buffers that are already displayed in a
window, so `exhub-file` clamps every embedded image to the preview width
itself (`exhub-markdown-preview--clamp-images`); otherwise a wide diagram
would keep its natural size and be clipped by the posframe edge. The EWW
preview does not need that workaround: its buffer is displayed first, so `shr`
scales images against the preview window.

If a diagram cannot be rendered (for example an invalid Mermaid body), the
handler falls back to plain Markdown rendering and the fence stays a code
block, so one broken diagram never breaks the whole preview.