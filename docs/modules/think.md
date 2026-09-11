# exhub-think

The `exhub-think` module provides MCP-based reasoning scratchpad tools for LLMs.

## Setup

The think server is built into the Exhub application and exposes an MCP endpoint at `/think/mcp`.

## Features

- **External scratchpad**: Both tools are backed by a per-session scratchpad (`Exhub.MCP.Tools.Scratchpad`). Every call appends its entry to a persistent journal and returns all accumulated entries, giving the model consolidated working memory it can re-read on each turn instead of re-deriving its reasoning from context.
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

Entries are kept in the MCP session's frame assigns (`Exhub.MCP.Tools.Scratchpad`, keys `:think_notes` and `:plan_steps`). The Anubis session process persists the frame between requests, so the journal survives for the lifetime of the session without any extra supervision tree members. No restart or cleanup endpoint is needed — the scratchpad ends with the session.

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
