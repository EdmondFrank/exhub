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
- one round trip for a whole command instead of per-keystroke typing;
- `chunk_size` (default 3000 chars) splits larger payloads across calls.

## Why it reads the screen with OCR

A canvas terminal exposes **no DOM text**: `.xterm-rows` is empty, there is no
accessibility tree, and the `.xterm` element carries no React fiber or back
reference to the `Terminal` instance. Capturing the instance would require
injecting a `Terminal` constructor hook *before* page load
(`Page.addScriptToEvaluateOnNewDocument`), which the extension does not expose.
So the only readback channel is screenshot + vision OCR — `webterm` keeps that
cost inside one call (`look` with `mode: "text"`).

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
| `max_lines` | integer | | `40` | lines transcribed per screen read |
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

### `run`

```json
{ "action": "run", "command": "awk '{print $1}' /data/an/list.txt | head -3", "timeout_s": 30 }
```

1. wraps the command as `command; printf '\nWTDONE<nonce> %s\n' $?`
   (nonce = 6 chars from an OCR-safe alphabet, no `0/O/1/I/L`);
2. pastes it and presses Enter;
3. polls `screenshot` → `look(mode: "text", max_lines: N)` — 0.7 s then every
   1.2 s — until the sentinel appears or `timeout_s` elapses;
4. returns:

```
exit_code: 0
polls: 2
--- output ---
copycat-v1.4.0-image.tgz
ums-admin-original.tar
```

The echoed command line is stripped best-effort (`clean_output/2`); if the
command wrapped across terminal lines a fragment of the echo can remain.

### `paste` / `keys` / `read`

```json
{ "action": "paste", "text": "docker ps", "enter": true }
{ "action": "keys", "keys": "Control+c" }
{ "action": "read", "max_lines": 24 }
```

`read` is the cheap path for "what does the screen say right now" — one
screenshot, one OCR call, no input.

## Cost model

Polling reads the screen with `look` (`mode: "text"`), so the per-command cost
is dominated by vision calls, not by the browser:

| Step | Cost |
|------|------|
| Ctrl+L + base64 paste | ~0.6 s |
| first poll (full transcription, doubles as the result) | ~1.7 s |
| each further poll | ~1.7 s (marker-only after 3 misses: ~1.5 s) |

A command that has finished by the first poll therefore costs **one** `look`
call and ~3 s end to end. On a benchmark of five commands against a live
JumpServer tab (2026-11-07) that came to 1 tool call / 4.5 s per command,
against 4.6 calls / 6.5 s for the hand-rolled `browser` DSL flow, with 5/5 vs
3/5 correct.

Two robustness details worth knowing before tuning:

- The sentinel is matched with one OCR edit of tolerance
  (`WTDONE` → `WDONE`, `WTD0NE`, exit code `O` → `0`), because a screenshot
  taken mid-paint misreads the word. An `:exact` match is trusted as the
  transcript; a fuzzy match triggers one confirming read on the settled screen.
- A `docker load` of several gigabytes will take many polls — raise
  `timeout_s` rather than re-issuing the command.

Use the `look` options instead of re-polling when you only need a fragment:

- `max_lines` bounds the transcription (fewer tokens, fewer OCR slips);
- `only_new` returns just the lines that changed since the previous read;
- `expect` appends `[expect: ok]` / `[expect: MISSING …]` so a caller can
  self-check instead of trusting the OCR blindly.

## Failure modes

| Symptom | Cause | Fix |
|---------|-------|-----|
| `chrome upstream server is not configured/enabled` | Browser MCP not enabled in `~/.config/exhub/mcp_servers.json` | enable the `chrome` server |
| `no terminal input element … run action "attach" first` | tab not attached / page still loading | `attach`, then retry |
| `timed out after Ns` | command still running (or sentinel lost to OCR) | raise `timeout_s`; `read` to inspect |
| output missing first line | echo stripping heuristic hit a data line | re-read with `read` |