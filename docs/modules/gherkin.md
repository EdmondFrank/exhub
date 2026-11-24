# exhub-gherkin

`exhub-gherkin` renders a Gherkin `.feature` file as a read-only Org
buffer, so scenarios, tags, doc strings and data tables can be read as a
formatted document instead of raw text.

The parser (`exhub-gherkin-parse.el`) is dependency free and never signals
on malformed input; the renderer (`exhub-gherkin-render.el`) produces the
Org markup and `exhub-gherkin.el` provides the preview buffer and commands.

## Setup

```elisp
(require 'exhub-gherkin)
```

The module is loaded by `exhub-keys` as well, so the `C-c e d` tree works
without the explicit `require`.

To get a preview key and refresh-on-save inside `.feature` buffers, enable
the minor mode:

```elisp
(add-hook 'find-file-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string-match-p "\\.feature\\'" (buffer-file-name)))
              (exhub-gherkin-mode 1))))
```

## Usage

| Command | Key | Description |
|---------|-----|-------------|
| `exhub-gherkin-preview` | `C-c C-v` (minor mode) / `C-c e d g` | Render the current buffer as Org in a preview buffer. |
| `exhub-gherkin-preview-file` | — | Render any `.feature` file. |
| `exhub-gherkin-preview-refresh` | `g` | Re-render the preview from its source. |
| `exhub-gherkin-goto-source` | `RET`, `C-c C-o` | Jump to the `.feature` line behind the heading or step at point. |
| `quit-window` | `q` | Dismiss the preview window. |

The preview buffer is read-only and derives from `org-mode`, so all Org
navigation, folding and export commands keep working. Every heading and
step carries an `exhub-gherkin-source-line` text property used by
`exhub-gherkin-goto-source`.

With `exhub-gherkin-mode` enabled, saving the `.feature` buffer refreshes
its open previews (`exhub-gherkin-auto-refresh`).

## Rendered mapping

| Gherkin | Org |
|---------|-----|
| `Feature:` | level 1 heading (`#+TITLE` carries the name) |
| `Rule:` | level 2 heading |
| `Background:`, `Scenario:`, `Scenario Outline:` | level 2/3 heading |
| `Examples:` | heading below its scenario |
| tags (`@smoke`) | Org tags on the following heading (`:smoke:`) |
| description text | indented paragraphs (never mistaken for Org syntax) |
| steps | list items with a bold keyword (`- *Given* ...`) |
| `<placeholder>` in an outline | marked as code (`~<term>~`) |
| doc strings (`"""` / ```` ``` ````) | `#+begin_example` block |
| data tables and `Examples:` tables | Org tables |

`#+STARTUP: showall` is emitted, so the preview opens fully expanded.

## Supported Gherkin

* `# language:` header — English (`en`, the default) and Simplified Chinese
  (`zh-CN`); English keywords are accepted as a fallback in either dialect.
* `Feature:`, `Rule:`, `Background:`, `Scenario:`,
  `Scenario Outline:` / `Scenario Template:`,
  `Examples:` / `Scenarios:`.
* Steps: `Given`/`When`/`Then`/`And`/`But` and `*` (and the `zh-CN`
  equivalents).
* Tags, comments, doc strings (triple quotes or backticks, with an optional
  media type) and data tables with the `\|`, `\n` and `\\` escapes.
* Missing or malformed `Feature:` headers degrade to a synthetic feature;
  unrecognised lines are kept as description text.

Not handled: Gherkin's `# language:` dialects beyond `en`/`zh-CN`, step
arguments validation, and test execution (see Hercules for running
features).

## Customization

`M-x customize-group RET exhub-gherkin`:

| Option | Default | Meaning |
|--------|---------|---------|
| `exhub-gherkin-preview-display` | `side-window` | `side-window`, `bottom`, `popup`, `switch` or `none` (no window, for scripting/tests). |
| `exhub-gherkin-preview-width` | `0.45` | Width of the right side window (ratio or columns). |
| `exhub-gherkin-auto-refresh` | `t` | Refresh open previews when a `.feature` buffer is saved. |
| `exhub-gherkin-render-tags` | `t` | Render Gherkin tags as Org tags. |
| `exhub-gherkin-render-bold-keywords` | `t` | Render `Given`/`When`/`Then` in bold. |
| `exhub-gherkin-render-step-tables` | `t` | Render data tables attached to a step. |
| `exhub-gherkin-render-highlight-placeholders` | `t` | Mark outline placeholders as code. |

## Tests

```sh
emacs -Q --batch -L . -L test/elisp -l ert -l exhub-gherkin-test \
      -f ert-run-tests-batch-and-exit
```

The ERT suite covers English and `zh-CN` parsing, scenario outlines and
`Examples:`, doc strings, data tables and escapes, malformed input,
Org validity of the rendered output, source-line properties and the
preview buffer lifecycle.
