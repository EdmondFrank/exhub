# Mac-use MCP Server

## Overview

The Mac-use MCP Server provides Playwright/Puppeteer-style control over any native macOS application via the Accessibility API. Explore UI trees, interact with elements, take screenshots, and automate native apps — all from MCP.

Built on top of [axcli](https://github.com/andelf/axcli), a Rust CLI that wraps Apple's Accessibility and ScreenCaptureKit frameworks.

## Prerequisites

1. **Install axcli**:
   ```bash
   cargo install axcli
   ```

2. **Accessibility permission**: System Settings → Privacy & Security → Accessibility — add your terminal app (Terminal.app, iTerm2, Alacritty, etc.)

3. **Screen Recording permission** (for screenshots): System Settings → Privacy & Security → Screen Recording — add your terminal app

4. Restart your terminal after granting permissions.

## Architecture

- **Server module**: `Exhub.MCP.MacUseServer` — registers 17 tool components via `use Anubis.Server`
- **Tool modules**: All tools live under `Exhub.MCP.Tools.MacUse.*`
- **Shared helpers**: `Exhub.MCP.MacUse.Helpers` provides:
  - `run_axcli/2` — Execute axcli via `Exile.stream/2` with timeout and error handling
  - `app_args/1` — Build `--app` or `--pid` targeting arguments
  - `strategy_args/1` — Build optional `--strategy` arguments
  - `cursor_args/1` — Build optional `--no-visual-cursor` flag
  - `toon_response/2` — TOON-encode response data (30–60% token reduction vs JSON)
- **HTTP Endpoint**: `/mac-use/mcp`

## Tools Reference

### App Discovery

#### list_apps

List running macOS applications visible to the Accessibility API.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| *(none)*  |      |          |             |

**Return**: `{apps: [{name, pid}, ...]}`

---

### Inspection

#### snapshot

Print the accessibility tree of an app or element. Shows the hierarchical structure of UI elements (roles, titles, values).

| Parameter  | Type    | Required | Description                             |
|------------|---------|----------|-----------------------------------------|
| `app`      | string  | yes*     | Application name                        |
| `pid`      | integer | yes*     | Process ID (alternative to app)         |
| `selector` | string  | no       | CSS-like selector to scope the snapshot |
| `depth`    | integer | no       | Maximum tree depth                      |
| `all`      | boolean | no       | Show all matches (default: first only)  |

\* One of `app` or `pid` is required.

#### get_attribute

Get an attribute value from an element.

| Parameter   | Type    | Required | Description                                                                                                                  |
|-------------|---------|----------|------------------------------------------------------------------------------------------------------------------------------|
| `app`       | string  | yes*     | Application name                                                                                                             |
| `pid`       | integer | yes*     | Process ID                                                                                                                   |
| `selector`  | string  | yes      | CSS-like selector for the target element                                                                                     |
| `attribute` | string  | yes      | Attribute name (`text`, `value`, `title`, `desc`, `classes`, `position`, `size`, `enabled`, `focused`, `role`, `identifier`) |

---

### Interaction (background-safe by default)

All interaction tools use `CGEventPostToPid` by default — no focus steal, no cursor movement.

#### click

Click an element.

| Parameter          | Type    | Required | Default  | Description                         |
|--------------------|---------|----------|----------|-------------------------------------|
| `app`              | string  | yes*     |          | Application name                    |
| `pid`              | integer | yes*     |          | Process ID                          |
| `selector`         | string  | yes      |          | CSS-like selector                   |
| `strategy`         | string  | no       | `"auto"` | `auto`, `ax`, `cg`, `cg-pid`        |
| `hover`            | boolean | no       | `false`  | Pre-move cursor to trigger hover UI |
| `activate`         | boolean | no       | `false`  | Bring app to foreground             |
| `no_visual_cursor` | boolean | no       | `false`  | Disable crosshair overlay           |

#### dblclick

Double-click an element.

| Parameter          | Type    | Required | Description               |
|--------------------|---------|----------|---------------------------|
| `app`              | string  | yes*     | Application name          |
| `pid`              | integer | yes*     | Process ID                |
| `selector`         | string  | yes      | CSS-like selector         |
| `strategy`         | string  | no       | Click strategy            |
| `no_visual_cursor` | boolean | no       | Disable crosshair overlay |

#### input

Focus an element and type text (appends to existing content).

| Parameter  | Type    | Required | Description       |
|------------|---------|----------|-------------------|
| `app`      | string  | yes*     | Application name  |
| `pid`      | integer | yes*     | Process ID        |
| `selector` | string  | yes      | CSS-like selector |
| `text`     | string  | yes      | Text to type      |

#### fill

Clear a field then type text (Cmd+A, Delete, type). Replaces all existing content.

| Parameter  | Type    | Required | Description                 |
|------------|---------|----------|-----------------------------|
| `app`      | string  | yes*     | Application name            |
| `pid`      | integer | yes*     | Process ID                  |
| `selector` | string  | yes      | CSS-like selector           |
| `text`     | string  | yes      | Text to type after clearing |

#### press

Press a key combination.

| Parameter  | Type    | Required | Description                                         |
|------------|---------|----------|-----------------------------------------------------|
| `app`      | string  | yes*     | Application name                                    |
| `pid`      | integer | yes*     | Process ID                                          |
| `keys`     | string  | yes      | Key combo (`Enter`, `Command+a`, `Control+Shift+v`) |
| `strategy` | string  | no       | `hid` (default, activates) or `pid` (background)    |

#### hover

Move the mouse cursor to an element's center.

| Parameter          | Type    | Required | Description               |
|--------------------|---------|----------|---------------------------|
| `app`              | string  | yes*     | Application name          |
| `pid`              | integer | yes*     | Process ID                |
| `selector`         | string  | yes      | CSS-like selector         |
| `no_visual_cursor` | boolean | no       | Disable crosshair overlay |

#### focus

Focus an element (AXFocused + click fallback).

| Parameter  | Type    | Required | Description       |
|------------|---------|----------|-------------------|
| `app`      | string  | yes*     | Application name  |
| `pid`      | integer | yes*     | Process ID        |
| `selector` | string  | yes      | CSS-like selector |

---

### Scrolling

#### scroll_to

Scroll an element into view (`AXScrollToVisible`).

| Parameter  | Type    | Required | Description       |
|------------|---------|----------|-------------------|
| `app`      | string  | yes*     | Application name  |
| `pid`      | integer | yes*     | Process ID        |
| `selector` | string  | yes      | CSS-like selector |

#### scroll

Scroll within an element (background-safe via cg-pid by default).

| Parameter   | Type    | Required | Description                                  |
|-------------|---------|----------|----------------------------------------------|
| `app`       | string  | yes*     | Application name                             |
| `pid`       | integer | yes*     | Process ID                                   |
| `selector`  | string  | yes      | CSS-like selector for the scrollable element |
| `direction` | string  | yes      | `up`, `down`, `left`, `right`                |
| `amount`    | integer | yes      | Scroll amount in pixels                      |
| `strategy`  | string  | no       | `auto`, `cg-pid`, `cg`                       |

---

### Screenshots

#### screenshot

Capture a screenshot via ScreenCaptureKit (occlusion-proof — works even when behind other windows).

| Parameter  | Type    | Required | Default | Description                                |
|------------|---------|----------|---------|--------------------------------------------|
| `app`      | string  | yes*     |         | Application name                           |
| `pid`      | integer | yes*     |         | Process ID                                 |
| `selector` | string  | no       |         | Capture a specific element                 |
| `ocr`      | boolean | no       | `false` | Extract text via OCR (Vision framework)    |
| `output`   | string  | no       |         | Output file path (default: returns base64) |
| `legacy`   | boolean | no       | `false` | Use legacy screenshot method               |

---

### Utility

#### wait

Wait for an element to appear or sleep for a duration.

| Parameter      | Type    | Required | Description                          |
|----------------|---------|----------|--------------------------------------|
| `app`          | string  | yes*     | Application name                     |
| `pid`          | integer | yes*     | Process ID                           |
| `selector`     | string  | no**     | CSS-like selector to wait for        |
| `milliseconds` | integer | no**     | Duration to sleep in ms              |
| `timeout`      | integer | no       | Max wait time in ms (default: 10000) |

\** One of `selector` or `milliseconds` is required (mutually exclusive).

#### activate

Bring a macOS application to the foreground.

| Parameter | Type    | Required | Description      |
|-----------|---------|----------|------------------|
| `app`     | string  | yes*     | Application name |
| `pid`     | integer | yes*     | Process ID       |

---

### Global Input

#### mouse

Global mouse control — ignores `--app`/`--pid`.

| Parameter | Type    | Required | Description                                                             |
|-----------|---------|----------|-------------------------------------------------------------------------|
| `action`  | string  | yes      | `pos`, `move`, `click`, `dblclick`, `scroll`                            |
| `x`       | integer | no       | X coordinate (for `move`/`click`/`dblclick`) or x offset (for `scroll`) |
| `y`       | integer | no       | Y coordinate or y offset                                                |

#### keyboard

Global keyboard input — ignores `--app`/`--pid`.

| Parameter | Type   | Required | Description                        |
|-----------|--------|----------|------------------------------------|
| `action`  | string | yes      | `type` or `press`                  |
| `text`    | string | yes      | Text to type or key combo to press |

---

## Locator Syntax

axcli uses a CSS-like selector syntax to target elements in the accessibility tree:

| Pattern             | Meaning             | Example                          |
|---------------------|---------------------|----------------------------------|
| `#id`               | DOM ID              | `#root`, `#modal`                |
| `.class`            | DOM class           | `.SearchButton`, `.msg-item`     |
| `.class1.class2`    | Multiple classes    | `.message-item.message-self`     |
| `Role`              | AX role             | `AXButton`, `button`, `textarea` |
| `Role.class`        | Role + class        | `AXGroup.feed-card`              |
| `Role[attr="val"]`  | Exact match         | `AXButton[title="Send"]`         |
| `Role[attr*="val"]` | Contains            | `radiobutton[name*="Tab Title"]` |
| `text="VALUE"`      | Exact text          | `text="Hello"`                   |
| `text~="VALUE"`     | Contains text       | `text~="partial"`                |
| `text=/regex/`      | Regex text          | `text=/\d+ unread/`              |
| `L >> R`            | Chain (scope)       | `.sidebar >> AXButton`           |
| `L > R`             | Direct child        | `AXWindow > AXGroup`             |
| `>> nth=N`          | Nth match           | `.item >> nth=0`                 |
| `:has-text("…")`    | Subtree text        | `.card:has-text("meeting")`      |
| `:has(sel)`         | Has descendant      | `.item:has(.reaction)`           |
| `:visible`          | Non-zero size       | `AXButton:visible`               |
| `:nth-child(N)`     | Nth child (0-based) | `AXGroup:nth-child(0)`           |

## Testing

Tests live under `test/exhub/mcp/mac_use/`:

```bash
# Run all mac-use tests
mix test test/exhub/mcp/mac_use/

# Run with verbose output
mix test test/exhub/mcp/mac_use/ --trace
```

79 tests across 3 test files:
- `helpers_test.exs` — Argument builders, toon_response, run_axcli
- `tools_metadata_test.exs` — Tool names, descriptions, uniqueness, component count
- `tools_execution_test.exs` — All 17 tools execute and return proper responses; edge cases
