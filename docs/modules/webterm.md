# exhub-webterm

`exhub-webterm` drives a **browser-hosted web terminal** — JumpServer / Luna
xterm.js, Guacamole, any canvas-rendered TTY — through the `chrome` upstream
MCP server (Browser MCP extension).

Endpoint: `POST /webterm/mcp` · Tool: `webterm`.

## Why it exists

The `browser` upstream tool speaks a newline-separated command DSL in which
text is delimited by double quotes:

```
type "ls -la"
```

Any payload containing a `"` — awk/sed/python string literals, JSON, a
`grep "two words"` — terminates the string early and the call fails with
`type takes at most 1 argument` or `type has no -h flag`. Long payloads are
also typed character by character, which is slow and lossy.

`webterm` never uses that path. Text is base64-encoded and injected with
`document.execCommand('insertText')`:

- **no quoting rules at all** (quotes, newlines, `$`, backticks survive);
- **UTF-8 safe**: the payload is decoded with `TextDecoder('utf-8')`, so CJK
  and other non-ASCII commands are not double-encoded (`atob` alone hands the
  page one Latin-1 code point per byte);
- one round trip for a whole command instead of per-keystroke typing;
- `chunk_size` (default 3000 chars) splits larger payloads across calls.

## Reading the screen: the terminal's own buffer

The canvas exposes no DOM text, but it does expose its **terminal API**. Luna
mounts a Vue app (`#__nuxt.__vue_app__`) whose component tree holds the live
xterm `Terminal`, and that object has the exact, already-parsed screen:

```js
terminal.buffer.active.getLine(i).translateToString(true)   // exact line
terminal.buffer.active.getLine(i).isWrapped                 // continuation
terminal.buffer.active.type                                 // normal | alternate
terminal.buffer.active.baseY + terminal.buffer.active.cursorY
```

`webterm` injects a small probe (`eval "eval(atob('…'))"`) that walks the Vue
app (with a visited set, depth and node caps), picks the largest *connected*
terminal for pages with several panes, and returns a line range plus
`len`/`type`/`cursor`.

Consequences:

- **no screenshot and no vision call** on the read path;
- exact characters, immune to OCR glyph slips and mid-paint frames;
- the echoed command is dropped **by content**, not by heuristics — see below;
- `eval --max all` lifts the DSL's 10 000-character reply cap, and `run`
  fetches output in 100-line chunks so a reply stays small.

A screenshot + `look` OCR remains as a fallback (`source: ocr`) for pages
where no terminal instance can be found — non-xterm terminals, or an app shell
that changed shape.

## Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `action` | string | ✓ | — | `tabs`, `attach`, `paste`, `run`, `keys`, `read` |
| `command` | string | | — | `run`: shell command (may contain anything) |
| `text` | string | | — | `paste`: raw text to inject |
| `tab` | string | | — | `attach`: tab id or a title substring, e.g. `攻击分析主机` |
| `keys` | string | | — | `keys`: chord, e.g. `Control+c`, `Enter` |
| `enter` | boolean | | `false` | `paste`: press Enter afterwards |
| `timeout_s` | integer | | `60` | `run`: wait budget (max 600) |
| `max_output_lines` | integer | | `1000` | `run`: cap on returned output lines (max 5000) |
| `allow_alt_screen` | boolean | | `false` | `run`: allow capturing a full-screen program |
| `readback` | string | | `auto` | `run`: `auto` \| `buffer` (require exact) \| `ocr` (legacy) |
| `max_lines` | integer | | `40` | `read` / timeout dump: lines returned |
| `chunk_size` | integer | | `3000` | chars per `insertText` call |

## Actions

### `tabs`

```json
{ "action": "tabs" }
```

Lists tabs (`id "title" — url`); `*` marks the attached one.

### `attach`

```json
{ "action": "attach", "tab": "攻击分析主机" }
```

Selects the tab and focuses `.xterm-helper-textarea`. Required once per
session; without it `paste`/`run` will report *no terminal input element*.
The reply also reports the readback channel, e.g.
`attached to tab 931804611 (focus: ok, buffer: exact 252x42 (normal))`.

### `run`

```json
{ "action": "run", "command": "awk '{print $1}' /data/an/list.txt | head -3", "timeout_s": 30 }
```

1. checks the buffer type — an `alternate` screen (less/vim/top) is refused
   unless `allow_alt_screen: true`;
2. wraps the command as `command; printf '\nWTDONE<nonce> %s\n' $?`
   (nonce = 6 characters from an OCR-safe alphabet, unique per invocation);
3. pastes it and presses Enter;
4. polls the buffer tail every 120 ms until a line is exactly
   `WTDONE<nonce> <exit_code>` — metadata-only reads, ~20 ms each, no vision;
5. reads the output window ending at the marker, drops everything up to and
   including the echoed command line, and returns:

```
exit_code: 0
polls: 1
source: buffer
--- output ---
copycat-v1.4.0-image.tgz
ums-admin-original.tar
```

Why it is echo-proof and deterministic:

- the marker regex is line-anchored and requires a number, while the echoed
  command carries the literal `%s` — the echo can never match;
- the nonce makes each invocation's marker unique, so older markers in the
  scrollback are ignored even after xterm trims its buffer;
- the echo line is identified by the literal `WTDONE<nonce> %s` and dropped by
  **content**, so no output line is ever swallowed (the old OCR path's
  "missing first line" failure mode);
- multiple polls keep the transcript cheap: each poll reads only the last few
  lines and costs one `eval`.

### `paste` / `keys` / `read`

```json
{ "action": "paste", "text": "docker ps", "enter": true }
{ "action": "keys", "keys": "Control+c" }
{ "action": "read", "max_lines": 24 }
```

`read` returns the buffer tail (no input, no vision); it falls back to a
single screenshot + OCR only when the buffer is unreachable.

## Benchmark: exact buffer vs screenshot + OCR

Measured on a live JumpServer/Luna tab (2026-11-09, 252x42, `hostname` =
`prod-attack-check`). 15 commands × 3 buffer runs / 2 OCR runs, screen cleared
before each run, `readback` forced per run; vision calls counted with a
temporary counter inside the tool.

| | buffer | OCR |
|---|---|---|
| correctness | **42/45** (all failures are the tab case) | **21/30** |
| latency min / median / mean / max | **689 / 705 / 744 / 1227 ms** | 3408 / 4328 / 5937 / 19821 ms |
| vision calls | **0** | 38 for 30 runs (1.27/run) |

Median speedup **6.1×**, mean **8.0×**; worst OCR run (19.8 s) was 16× the
slowest buffer run.

Where OCR loses correctness (measured):

| case | buffer | OCR |
|---|---|---|
| `seq 1 80` | 80 lines, exact | 38 lines (screen height) |
| `seq 1 200` | 200 lines, exact | 38 lines |
| `seq 1 3000` | last 1000 lines + explicit truncation note, 0.79 s | last 37 lines, 4.6 s |
| `false` (no output) | `exit_code: 1`, empty output | echoed command leaks into output |
| 300-char wrapped output | one logical line (isWrapped joined) | 2 lines |
| `less` (alternate screen) | refused in ~0.7 s with a typed error | 13.4 s, then `timed out after 8s` |
| `printf '攻击分析主机 OK\n'` (CJK) | exact | exact (after the UTF-8 paste fix) |
| `printf 'a\tb\n'` | `a    b` (tab rendered as spaces) | `a       b` (different spacing) |

The one genuine buffer limitation is the last row: xterm stores the *rendered*
screen, so a tab becomes spaces. Re-encode when tabs matter (`| cat -A`,
`| tr '\t' '|'`) or fall back to `readback: "ocr"` (which is not better here).

Two robustness details worth knowing before tuning:

- `timeout_s` is clamped to `[5, 600]`; a long-running command (`docker load`)
  needs a larger budget rather than a re-issue.
- Output is capped by `max_output_lines`; when the window does not contain the
  echo anymore the reply starts with
  `…(output truncated to the last N lines)`.

## Failure modes

| Symptom | Cause | Fix |
|---------|-------|-----|
| `chrome upstream server is not configured/enabled` | Browser MCP not enabled in `~/.config/exhub/mcp_servers.json` | enable the `chrome` server |
| `no terminal input element … run action "attach" first` | tab not attached / page still loading | `attach`, then retry |
| `the terminal is in the alternate screen …` | less/vim/top is open | exit it, or pass `allow_alt_screen: true` |
| `the command switched to the alternate screen …` | the command itself opened a full-screen program | same |
| `lost the terminal buffer …` | page navigated / session closed | re-`attach` |
| `timed out after Ns …` | command still running | raise `timeout_s`; `read` to inspect |
| reply says `source: ocr` | no xterm instance found on the page | expected on non-xterm terminals; OCR may glitch |