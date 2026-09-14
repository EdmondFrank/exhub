# exhub-think

The `exhub-think` module provides MCP-based reasoning scratchpad tools for LLMs.

## Setup

The think server is built into the Exhub application and exposes an MCP endpoint at `/think/mcp`.

## Features

- **External scratchpad**: Both tools are backed by a per-session scratchpad store (`Exhub.MCP.ScratchpadStore`). Every call appends its entry to a persistent journal keyed on the session id and returns all accumulated entries, giving the model consolidated working memory it can re-read on each turn instead of re-deriving its reasoning from context.
- **Think Tool**: Records a thought for complex reasoning without obtaining new information or changing anything.
- **Plan Tool**: Records ordered next steps as a plan journal; the model works the steps in order and revises the plan explicitly (by calling `plan` again) when reality diverges.
- **Visibility & anti-looping**: The response includes a `recorded` counter that makes runaway thinking loops visible, plus a `next` nudge telling the model to act on what it has already written and to call again only with materially new state.
- **MCP Tools**: Two MCP tools are available:
  - `think`: Append a thought to the reasoning scratchpad
  - `plan`: Append a plan of next steps to the plan journal

## Response Format

Both tools return a JSON envelope rather than echoing the input back verbatim:

```json
{
  "recorded": 3,
  "scratchpad": ["first entry", "second entry", "third entry"],
  "next": "<nudge text encouraging action over redundant re-thinking>"
}
```

- `recorded` — number of entries currently stored in the scratchpad
- `scratchpad` — all accumulated entries (oldest first)
- `next` — a short directive: resolve obligations already listed / execute the next recorded step, and call again only for materially new reasoning state

## State & Guarantees

Entries live in an external, session-keyed store — `Exhub.MCP.ScratchpadStore` (a GenServer over a public ETS table) — rather than the MCP frame's `assigns`. Each bucket is keyed by `{session_id, key}`, where `session_id` comes from the transport-independent `frame.context.session_id` and `key` is `:think_notes` or `:plan_steps`.

Why not frame assigns? ExHub serves every `tools/call` through `Exhub.MCP.ConcurrentToolDispatcher`, which builds a fresh frame per request and discards the frame returned by the tool. Frame-backed state therefore never accumulated (each call started empty and always reported `recorded: 1`). Keying on `session_id` makes accumulation work under both the concurrent dispatcher and the `Anubis.Server.Session` path. Appends happen atomically inside the store, so concurrent calls against the same session can't lose updates.

The store is supervised in `Exhub.Application` (started just before `Exhub.MCP.ThinkServer`) and prunes buckets idle for more than 2 hours via a periodic cleanup task, mirroring `Exhub.MCP.TodoStore`. Because it is in-memory, journals reset on a full VM restart — acceptable for ephemeral reasoning notes. If a call arrives without a session id, entries fall back to a shared `"__default__"` bucket instead of crashing.

Bounds (per tool, per session):

- Each entry is truncated to 2,000 characters (codepoint-safe, with a `…[truncated]` suffix).
- At most 50 entries are retained; the oldest are dropped first.
- Malformed arguments never crash the tool: non-string input is normalized (numbers/booleans stringified, empty values recorded as `"empty"`) or replaced with a placeholder note.

## Configuration

No additional configuration is required. The server starts automatically with the Exhub application.

## MCP Endpoint

The think server is accessible at:

```
POST /think/mcp
```

This endpoint accepts MCP protocol messages for tool invocations.

## Tool Parameters

### think

- `thought` (required): A thought to think about. Restate the problem, break it into sub-problems, resolve each explicitly with intermediate results, then check your work (including at least one boundary case) before producing the final answer.

### plan

- `plan` (required): A plan of next steps, recorded as an ordered list to be worked through and revised explicitly.
