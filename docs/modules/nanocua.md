# Nanocua — macOS Computer Use MCP

## Overview

[Nanocua](https://github.com/justrach/nanoui/tree/main/apps/nanocua) gives agents
eyes and hands on a real Mac **without taking over the screen**. It is a small
Zig app (`nanocua.app`: a control panel, the policy, and an activity log) plus an
MCP server any agent can add:

```
agent ──stdio──▶ nanocua mcp ──unix socket──▶ nanocua.app
                 (a pipe, nothing more)        the panel, the policy, the log,
                                               accessibility, input, screenshots
```

The app does all the work; `nanocua mcp` only carries lines. That split is the
point on macOS: **Accessibility and Screen Recording permissions belong to the
app that asks**, so they are granted to `nanocua.app` once and can be revoked in
one place — not to every terminal or agent that happens to start a CLI.

In ExHub it is registered as an **external hub upstream** named `nanocua`, so
agent-facing tools are `nanocua__*` (`nanocua__get_ui_tree`, `nanocua__click`, …).
The built-in [`mac-use`](mac-use.md) server (axcli) stays available for the gaps
listed in [Limitations](#limitations).

## Prerequisites

1. **`nanocua.app`** in `~/Applications` (or anywhere; the hub config points at
   its binary). Build it below, or take it from a `graff` macOS release.
2. **Permissions**, granted to `nanocua.app` — macOS keeps them only for the same
   signing identity, so an ad-hoc build asks again after every rebuild:
   - System Settings → Privacy & Security → **Accessibility**
   - System Settings → Privacy & Security → **Screen Recording**
3. **Input is off at launch.** Agents may look (`list_apps`, `get_ui_tree`,
   `screenshot`) but not act until the user turns on **Allow input** in the
   nanocua panel. Leave **Allow whole screen** off unless the agent genuinely
   needs the real pointer or the whole display.

## Build from source

`apps/nanocua` requires **Zig 0.17+** and is built against the surrounding
`nanoui` tree (its `build.zig.zon` depends on `../..`), so build inside a clone:

```sh
git clone --depth 1 https://github.com/justrach/nanoui ~/Code/nanoui
cd ~/Code/nanoui
ASDF_ZIG_VERSION=0.17.0 VERSION=0.1.0 \
  sh apps/nanocua/macos/package.sh aarch64-macos ~/Applications   # or x86_64-macos
```

`package.sh` builds `ReleaseFast` for macOS 13+, bundles `nanocua.app`, signs
with `$CODESIGN_IDENTITY` when set (else ad hoc), and writes it to the output
directory. The submodules (`apps/nanoterm`, `site`, …) are **not** needed.

Health check (no GUI needed):

```sh
~/Applications/nanocua.app/Contents/MacOS/nanocua ctl status --json
# {"installed":true,"running":true,"stopped":false,"input":true,"wholeScreen":false,
#  "accessibility":true,"screenRecording":true,"agents":1,"calls":12,"stopKey":true,...}
```

## Registering it in the hub

The hub reads `~/.config/exhub/mcp_servers.json` (`$EXHUB_MCP_SERVERS_CONFIG`
overrides) and **external entries take precedence over built-ins**. Add:

```json
{
  "name": "nanocua",
  "transport": "stdio",
  "command": "/Users/you/Applications/nanocua.app/Contents/MacOS/nanocua",
  "args": ["mcp"],
  "enabled": true,
  "expose_route": "nanocua",
  "instructions": "nanocua lets you use apps on this Mac, inside one app, in the background. Look first: get_ui_tree returns exact elements with refs; act on refs. Use screenshot when the tree is not enough. Coordinates are screen points from the top-left. If a call is denied, tell the user once and carry on."
}
```

Or register it on the **running** node, no restart (this also persists the JSON):

```sh
_build/prod/rel/exhub/bin/exhub rpc '
  Exhub.MCP.Hub.ClientManager.add_server(%{
    "name" => "nanocua", "transport" => "stdio",
    "command" => System.get_env("HOME") <> "/Applications/nanocua.app/Contents/MacOS/nanocua",
    "args" => ["mcp"], "enabled" => true, "expose_route" => "nanocua"})'
```

Verify: `ClientManager.list_servers()` shows `{"nanocua", :connected, 10}` and
`retrieve_tools` finds `nanocua__get_ui_tree`. `initialize` answers without
starting the app; the first real call opens it in the background.

## Tools

| Tool | Does | Needs |
|------|------|-------|
| `list_apps` | running apps **with windows**: name, bundle id, pid, frontmost | — |
| `get_ui_tree` | one line per element with a `ref`, role, title, value, frame (screen points) | Accessibility |
| `screenshot` | JPEG of an app's **main window**, even behind others; `whole_screen` for the display/region | Screen Recording |
| `click` | by `ref` (pressed through accessibility) or `x,y`; `button: left/right`, `count: 2` | Allow input |
| `type_text` | Unicode text into the focused element, or into `ref` | Allow input |
| `press_key` | `cmd+s`, `shift+tab`, `enter`, `escape`, `cmd+shift+left` … | Allow input |
| `scroll` | at `x,y` or over a `ref`, by `dy`/`dx` lines | Allow input |
| `move_mouse` | hover at `x,y` (the user's pointer stays put) | Allow input |
| `set_value` | set a field's value through accessibility — exact, no typing | Allow input |
| `open_app` | open an app by name behind the user's work, and make it the one you act in | Allow input |

Shared arguments: `app` (name) or `pid`, and `whole_screen: true` to leave the
app-scoped default.

## Workflow

```
list_apps → get_ui_tree → act on a ref → get_ui_tree again (or screenshot) → verify
```

- Elements carry a ref like `[12]`; **refs stay valid until the next
  `get_ui_tree`**. In practice they are stable across consecutive reads of an
  unchanged app (only icon frames jitter a point or two), so re-read after every
  action and before the next.
- `get_ui_tree` makes that app **the one you act in**; `app`/`pid` are optional
  afterwards.
- Prefer the tree over screenshots: it is exact and cheap. Measured on this
  machine, a Finder tree is ~10 k chars for 400 elements (`max_nodes`, up to
  2000) versus ~4 k for the axcli snapshot — but the axcli tree has no refs and
  no coordinates, so it needs extra `get_attribute` calls to act.
- Screenshot replies include the mapping, e.g.
  `pixel (px, py) is the point (213 + px*0.5000, 77 + py*0.5000)` — use it to
  convert a located pixel back to a click coordinate.
- Verification: text fields only expose `value=` once non-empty, so re-reading
  the tree right after `type_text` shows the new text; for anything else take a
  `screenshot` (`look` can read it).

## Denials

The policy answers with a plain reason and no side effects:

| Message | Meaning |
|---------|---------|
| `nanocua is stopped: the user pressed Stop.` | kill switch (⌃⌥⌘.); wait for the user |
| `Input is off in nanocua: you may look … but not act.` | ask the user to turn on **Allow input** |
| `The whole screen is off in nanocua: work inside one app` | drop `whole_screen` |
| `nanocua has no Accessibility permission: ask the user to grant it` | TCC grant missing (e.g. after a rebuild) |

Tell the user once and carry on without it — never retry in a loop and never try
to grant permissions yourself.

## Limitations

- **Electron/WebView apps (WeChat, Feishu, Docker Desktop)** expose a nearly empty
  AX tree: a window and its three traffic-light buttons. Neither nanocua nor
  axcli can see inside them. Fall back to the coordinate workflow in the
  `mac-use`/`axcli` skills: `screenshot` → locate (Vision OCR / `look`) → click
  by `x,y` (nanocua posts events to the app, so no pointer movement).
- A window on another Space or minimized is out of reach: macOS shows neither its
  picture nor its elements.
- Calls are serialized app-wide, and one "app you act in" is shared by every
  session on that socket. Concurrent agents can invalidate each other's refs.
- Nothing stops a call that a well-meaning agent launches; the gates are a
  guard-rail, not a sandbox.
- `screenshot` returns an MCP **image block** (`{"type":"image","data":…,"mimeType":"image/jpeg"}`).
  Clients that cannot render images see base64 only — decode it if you need a file.

## Compared with `mac-use` (axcli)

| | `nanocua` | `mac-use` (axcli) |
|---|---|---|
| Permissions | granted to `nanocua.app`, independent of the driving agent | bound to the terminal/ExHub host process |
| Screenshot | real JPEG + pixel→point mapping | `image_base64: ""` unless the host has Screen Recording |
| Element handles | integer refs from the tree | CSS-like selectors, re-resolved per call |
| Tree content | role, title, value, frame, `disabled`/`focused`/`selected` | role + title (frames via `get_attribute`) |
| Coverage | 10 tools | 15 tools, incl. `wait`, `scroll_to`, `focus`, `get_attribute`, foreground `activate` |
| Policy | Stop / Allow input / Allow whole screen, activity log | none |
| License | codegraff's modified AGPLv3 (do not vendor into ExHub) | axcli is MIT |

## Troubleshooting

- Everything denied → `nanocua ctl status --json`: `accessibility` /
  `screenRecording` false means the TCC grant is missing; `input` false means the
  panel switch is off. `nanocua ctl grant screen` opens the Screen Recording
  prompt.
- Rebuilt the app → re-grant both permissions (ad-hoc signature changes).
- Socket: `~/Library/Application Support/nanocua/mcp.sock` (`NANOCUA_SOCKET`
  overrides); Linux uses `$XDG_RUNTIME_DIR/nanocua.sock`.
- `nanocua` also answers `ctl status|stop|off|allow|grant|open` — `stop` and
  `off` work from anywhere, `allow` only *asks* in the panel.
- No ExUnit surface: the server is external, so there is nothing in
  `test/exhub/`. Probe it through the hub instead (see snippets above).
