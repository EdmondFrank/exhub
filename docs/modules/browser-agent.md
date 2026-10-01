# exhub-browser-agent

The `exhub-browser-agent` module is a **Jev-style browser agent**: it observes a
page, asks the Smart Decide (System One) decision model to pick an *operation*
and a *target*, and executes only the chosen operation. It is the ExHub port of
[browser-use/jev-ultrafast](https://github.com/browser-use/jev-ultrafast) — *"a
browser agent that chooses instead of generating"* — built on the existing
`browser-use` (kuri/CDP) tools and the `smart-decide` module.

The model never emits selectors, coordinates, shell commands, or JavaScript. It
chooses from the operations and numbered elements the loop observes.

## The loop

```
kuri daemon ──▶ indexed element table ──▶ Smart Decide (one request)
 /snapshot                                      │  operation + target
 (or DOM fallback)                              ▼
                                      executor ──▶ kuri /action | /evaluate
                                                    │
                            TYPE_TEXT ──▶ TextHelper (small LLM, JSON {text})
```

Each cycle:

1. **Observe** — the kuri daemon's accessibility snapshot, plus page text and
   URL/title; parse into nodes and number the actionable elements. When CDP's
   accessibility tree fails (wide DOMs — see *Notes*), fall back to the
   stamped-DOM table and record `observation: "dom"`. Each observation also
   settles the previous step: it stamps `page_changed` on that history entry and,
   when a target action changed nothing, may slide the offered candidate window
   (see *Candidate window*).
2. **Choose** — one `POST /v1/systemone` request carries the `operation`
   question plus a speculative `<op>_target` head per available operation.
   Only the selected operation's target head is consumed.
3. **Execute** — the chosen operation maps to a kuri call. `TYPE_TEXT` asks
   the text helper for the field value first.
4. Repeat until `DONE`, `BLOCKED`, an error, or the step budget.

Operations: `CLICK`, `TYPE_TEXT`, `SCROLL_UP`, `SCROLL_DOWN`, `WAIT`, `DONE`,
`BLOCKED`. `CLICK` targets are clickable roles (`button`, `link`, `menuitem`,
`tab`, `option`, `checkbox`, `radio`, `switch`, …); `TYPE_TEXT` targets are
editable roles (`textbox`, `searchbox`, `combobox`, `spinbutton`).

## Backends

The `Exhub.BrowserAgent.Kuri` facade delegates to one of two interchangeable
backends, selected by `config :exhub, Exhub.BrowserAgent, backend:`:

| Backend | Module | Transport | Notes |
|---------|--------|-----------|-------|
| `:http` (default) | `Exhub.BrowserAgent.KuriHttp` | `Exhub.KuriDaemon` HTTP API (`127.0.0.1:18080`) | Refs survive between calls; no per-call `Exile` spawn |
| `:cli` | `Exhub.BrowserAgent.KuriCli` | `kuri-agent` via `Exile` | Refs do **not** survive between processes (see *Notes*) |

Both backends expose the same surface (`snap`, `dom_snapshot`, `text`, `eval`,
`click`, `fill`, `type`, `select`, `scroll(:up | :down)`, `go`, `tabs`, `status`,
`use`), so `Agent`, `Executor` and `Policy` are backend-agnostic. `KuriHttp.use/1`
delegates to `KuriCli` because attaching a tab means writing `~/.kuri/session.json`.

## Prerequisites

- The kuri daemon is normally already running: `Exhub.KuriDaemon` supervises the
  `kuri` binary on `127.0.0.1:18080` (headless), with its token in
  `~/.kuri/api.token`.
- A Chrome tab attached with `browser_tabs` (`use`) or opened with `kuri-agent
  open`; the attached tab is read from `~/.kuri/session.json`.
- The Smart Decide key (`:exhub, :giteeai_api_key`) is used for decisions, and
  the same key is used by the text helper (`api.moark.com`).
- The `:cli` backend additionally needs `kuri-agent` on `PATH`.

## MCP endpoint

```
POST /browser-agent/mcp
```

Built-in hub server name: `browser-agent`.

## Tool: `browser_agent`

| Parameter    | Type    | Required | Description |
|--------------|---------|----------|-------------|
| `command`    | string  | ✓        | `run` \| `start` \| `step` \| `status` \| `stop` \| `list` |
| `goal`       | string  |          | Natural-language goal (required for `run` and `start`) |
| `url`        | string  |          | Navigate before the first observation |
| `session_id` | string  |          | Session id from `start` (required for `step`/`status`/`stop`) |
| `max_steps`  | integer |          | Decision-cycle budget for `run` (default 15) |
| `model`      | string  |          | System One model override (default `Intern-Decision-4B`) |

- `run` — runs the loop synchronously and returns the full trace.
- `start` / `step` / `status` / `stop` — interactive control; state lives in
  `Exhub.BrowserAgent.Store` between calls.

### Response

```json
{
  "session_id": "ba_1f3c…",
  "result": {
    "status": "done",
    "goal": "Find one-way flights …",
    "url": "https://www.google.com/travel/flights",
    "title": "Flights",
    "observation": "a11y",
    "elements": "[1] button \"Change ticket type\"\n[2] combobox \"Where from?\" = Zürich",
    "text": "Flights\nChange ticket type\nWhere from?",
    "step": 6,
    "history": [
      {"step": 1, "operation": "TYPE_TEXT", "target": "2", "ref": "e12",
       "label": "Where from?", "text": "Zürich", "confidence": 0.93,
       "page_changed": true, "elapsed_ms": 1840}
    ],
    "error": null
  }
}
```

`observation` is `"a11y"` or `"dom"` and says which source produced `elements`
(and therefore which ref scheme — `eN`/`e1_24` vs `dN` — the history's `ref`s use).
`text` is the bounded page text the model was shown, so an API-lookup style goal
returns what the agent read (and nothing else is generated). Each `history` entry
carries `page_changed` (`true`/`false` once the next observation settles it, `nil`
for the last step), so a caller can see whether an action actually moved the page.

## Modules

| Module | Job |
|--------|-----|
| `Exhub.BrowserAgent.Snapshot` | Parse an a11y or DOM snapshot; build the indexed element table and windowed per-operation target heads |
| `Exhub.BrowserAgent.Policy` | Build the Smart Decide questions; validate the choice; produce the decision |
| `Exhub.BrowserAgent.TextHelper` | Generate the `TYPE_TEXT` value (small JSON-mode LLM) |
| `Exhub.BrowserAgent.Executor` | Map a decision to a kuri action |
| `Exhub.BrowserAgent.Agent` | The observe → choose → execute loop and state machine |
| `Exhub.BrowserAgent.Kuri` | Backend facade (`:http` / `:cli`) |
| `Exhub.BrowserAgent.KuriHttp` | Default backend: the kuri daemon HTTP API |
| `Exhub.BrowserAgent.KuriCli` | Fallback backend: the `kuri-agent` CLI |
| `Exhub.BrowserAgent.DomBridge` | Stamped-DOM observation + `dN` action replay for wide pages |
| `Exhub.BrowserAgent.Scroll` | Scrolls the element that actually scrolls (a docs pane, not the window) |
| `Exhub.BrowserAgent.Store` | Session state for interactive control |
| `Exhub.MCP.Tools.BrowserAgent` / `Exhub.MCP.BrowserAgentServer` | MCP surface |
| `Exhub.MCP.Hub.BuiltInRegistry` | Maps the `browser-agent` name to its server so the hub discovers the tool |

Every collaborator is injectable (`:kuri`, `:decider`, `:generator`), so the
loop is unit-tested without a browser or network.

## Notes and limits

- **Refs and the backend.** The daemon keeps its accessibility ref registry
  server-side, so `snap` → `click` works across calls. The `kuri-agent` CLI
  assigns refs *per process*, so a ref printed by one invocation cannot be
  resolved by the next (`ref 'eN' not found. Run kuri-agent snap first.`) — which
  is why `:http` is the default.
- **Wide-DOM fallback.** Chrome's CDP `Accessibility.getFullAXTree` fails on
  wide DOMs (measured between ~4,000 and ~6,000 nodes — the range documentation
  sites like DevDocs/hexdocs fall into), returning `CDP command failed`. The loop
  then observes via `DomBridge`: it stamps visible actionable elements with
  `data-kuri-dom-ref="dN"` and returns the same shape as an a11y snapshot, and
  replays `dN` actions through `/evaluate`. Refs are page-scoped, so a navigation
  invalidates them; a stale ref reports `missing` rather than no-op'ing.
- **Observation budget.** Smart Decide accepts at most **8,191 input tokens**, so
  `Agent.observe/1` bounds the element table to `opts[:max_elements]` (default
  **120**, document order) and `Agent` bounds the page text to 4,000 chars. The
  offered targets come from one 16-element window of each role (see *Candidate
  window*). Without this, a large docs page (the DevDocs `Enum` page has 400
  actionable elements out of 726 candidates) produced a 15,456-token prompt and
  failed with HTTP 422.
- **Candidate window.** A target head offers at most 16 elements, so the first
  sixteen of a docs sidebar are boilerplate navigation and the article's own link
  sits outside the offer — unchoosable, however good the model is. `Agent` slides
  the window (`Snapshot.action_space/2` `:offset`) past a head that made no
  progress: when a `CLICK`/`TYPE_TEXT` changes nothing and more candidates remain
  (`Snapshot.candidate_ceiling/1`), the next observation offers the *next*
  sixteen. A page change resets the window. On the DevDocs Rails `ActionCable`
  page the goal's `…::Streams#stream_from` link is element `[63]`, reachable once
  the window reaches offset 48.
- **Goal-relevant candidate order.** Windowing alone is not enough when a click
  *does* navigate (the window resets on every page change), so the head is also
  *ranked*: `Snapshot.goal_terms/1` extracts the significant terms of the goal
  (lowercased, `_`-preserving, boilerplate like "open"/"documentation"/"sidebar"
  dropped) and `Snapshot.action_space/2`'s `:prefer` offers the candidates whose
  label shares the most of them first. It is a stable sort, so unmatched
  candidates keep document order and an unmatched goal behaves exactly as before —
  this only *orders* the offer, the model still chooses. On the DevDocs
  `ActionCable` page the goal "…`Streams#stream_from`" lifts element `[63]` to the
  top of the head, so it is choosable on the first step (`before_action` on the
  Rails `AbstractController` page works the same way).
- **Main-content text.** Documentation sites put thousands of characters of
  navigation/sidebar ahead of the article, so whole-body text sliced from the top
  hides the content a goal is about (DevDocs' Rails `before_action` sits ~5,600
  chars in). `Agent.fetch_page/2` reads the main content pane (`main`,
  `[role=main]`, `article`) first and falls back to the whole-page text when it is
  empty or unreadable.
- **`/evaluate` values are not always strings.** `window.scrollBy(...)` returns
  `undefined`, which the daemon sends as `{"type":"object","value":{}}`. The
  backend JSON-encodes non-string results and treats a missing `value` as empty,
  so `SCROLL_UP`/`SCROLL_DOWN` succeed instead of failing with *carried no value*.
- **Scrolling follows the real scroller.** `window.scrollBy` is a no-op on
  container-scrolling pages — DevDocs scrolls its `<main>` pane (`window.scrollY`
  stays `0` while `main.scrollTop` moves). `Exhub.BrowserAgent.Scroll` picks the
  document when it scrolls, else the first scrollable descendant, and returns the
  new offset as a string. Both directions go through `kuri.scroll(:up | :down)`.
- **Fallback table cap.** `DomBridge` stamps at most 150 actionable elements;
  `Agent` trims that to `max_elements` (120).
- A Smart Decide `choice` question needs **at least two options**; a target head
  with a single candidate is auto-selected instead of asked.
- System One accepts between **2 and 16** candidates per `choice` question
  (enforced by `Policy.max_choice_options/0`), so a target head offers at most
  16 elements. The head is **windowed** rather than simply truncated — see
  *Candidate window*.
- `kuri-agent` runs through `Exile` with a 60 s budget per call; an overrun is
  killed instead of blocking the loop (`Exile.stream/2` has no `:timeout`).
- kuri truncates names by bytes and can split a `\uXXXX` escape, producing
  invalid JSON; the text tree is parsed instead and escapes are decoded
  leniently.
- **Page-change fingerprint.** `Agent` fingerprints each observation (element
  refs, field values, non-focus element states, the URL without its fragment, and
  the main text) and compares it with the previous one to decide whether the last
  action changed the page. Focus and URL fragments are deliberately excluded:
  clicking moves focus and a docs sidebar anchor only changes the fragment, so
  counting either would make every click look like a navigation and reset the
  candidate window. The verdict is stamped
  as `page_changed` on the history entry and surfaced to the model in
  `recent_actions`, giving the "do not repeat satisfied steps" rule real evidence.
  `@max_stalls` (3) consecutive no-progress target actions block the run instead
  of burning the step budget.
- `TYPE_TEXT` uses `fill` (clear + fill), replacing the field value.

## Configuration

```elixir
config :exhub, Exhub.BrowserAgent,
  backend: :http   # :http (kuri daemon, default) | :cli (kuri-agent)

config :exhub, Exhub.BrowserAgent.TextHelper,
  endpoint: "https://api.moark.com/v1/chat/completions",
  model: "deepseek-v4.1-flash"
```

In-code defaults apply for any missing key.

## Activating on a running release

New code needs a build + hot reload; the new supervised children (`Store`,
`BrowserAgentServer`) need the zero-downtime recipe (see `AGENTS.md`):

```sh
MIX_ENV=prod mix release --overwrite
_build/prod/rel/exhub/bin/exhub rpc "Exhub.HotReload.reload()"
_build/prod/rel/exhub/bin/exhub rpc "Supervisor.start_child(Exhub.Supervisor, Exhub.BrowserAgent.Store)"
_build/prod/rel/exhub/bin/exhub rpc "Supervisor.start_child(Exhub.Supervisor, {Exhub.MCP.BrowserAgentServer, [transport: :streamable_http, request_timeout: 600_000, session_idle_timeout: 86_400_000 * 365]})"
```

After the next full boot, `application.ex` takes over automatically.