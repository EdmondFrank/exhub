# Smart Decide MCP Server Design

## Overview

A new MCP server exposing System One structured decision making via Gitee AI /
moark. The `smart_decide` tool evaluates a `state` (the content to judge)
against a flat map of typed `questions` and returns one structured answer per
question, with calibrated probabilities. It is compatible with the TypeSafe
Jev contract and served by the Bespoke Nimble model (`Bespoke-Nimble-9B`).

Unlike the existing chat/vision tools, this tool does not generate free-form
text or reasoning: the backend scores the allowed answer tokens directly and
builds the structured output from the resulting probabilities.

## Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌──────────────────────┐
│   MCP Client    │────▶│ SmartDecideServer│────▶│  Gitee AI / moark    │
│  (Claude/etc)   │     │ /smart-decide/mcp│     │  POST /v1/systemone  │
└─────────────────┘     └──────────────────┘     └──────────────────────┘
                               │
                               ▼
                        ┌──────────────────┐
                        │ Tools.SmartDecide│
                        │  - state         │
                        │  - questions     │
                        │  - model/compact │
                        └──────────────────┘
```

**Pattern:** follows the existing `LookServer` + `Tools.Look` and
`ListenServer` + `Tools.Listen` structure (Anubis server + one tool component),
configured with the shared `giteeai_api_key`.

## Tool: `smart_decide`

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `state` | string \| object \| array | ✓ | — | Content to judge. JSON object/array strings are decoded. |
| `questions` | object | ✓ | — | Map of question-id => typed question. |
| `model` | string | | `Bespoke-Nimble-9B` | System One model |
| `compact` | boolean | | `false` | Drop probability/confidence detail from answers |

### Question shape

```json
{
  "<id>": {
    "type": "noul" | "choice" | "score",
    "instructions": "What the model should decide",
    "criteria": { "option": "rubric description" }   // choice (or list of options)
                   | ["level 0", "level 1", ...]       // score (>= 2 levels)
                   | { "true": "...", "false": "..." } // noul (optional)
  }
}
```

## API request format

```elixir
POST https://api.moark.com/v1/systemone
Headers: Authorization: Bearer {giteeai_api_key}
         Content-Type: application/json
         X-Failover-Enabled: true

{
  "model": "Bespoke-Nimble-9B",
  "state": <string | object | array>,
  "questions": { "<id>": { "type": ..., "instructions": ..., "criteria": ... } }
}
```

## Response format

```json
{
  "model": "Bespoke-Nimble-9B",
  "answers": {
    "<id>": { "type": "noul",   "noul": 0.92 },
    "<id>": { "type": "choice", "choice": "technical",
              "probabilities": { "billing": 0.08, "technical": 0.85, "sales": 0.07 },
              "confidence": 0.82 },
    "<id>": { "type": "score",  "score": 1.6,
              "legend": { "0": "Calm", "1": "Frustrated", "2": "Very angry" },
              "probabilities": { "0": 0.05, "1": 0.3, "2": 0.65 },
              "confidence": 0.78 }
  }
}
```

With `compact: true` the tool strips `probabilities`, `confidence`, and
`legend`, keeping only `type` and the chosen value.

## Validation and normalization (pure helpers)

Unit-testable, no network:

- `normalize_state/1` — decodes JSON object/array strings; leaves plain text
  (including scalar-looking strings such as `"123"`) untouched.
- `normalize_questions/1` — accepts a map or JSON string; validates that every
  question has a `type` in `noul | choice | score` and the criteria required by
  that type; expands a list of option strings into an all-`nil` `choice`
  criteria map (Nimble-style shorthand). Returns `{:ok, map}` or
  `{:error, message}`.
- `compact_answers/1` — trims each answer to its chosen value.

## Files

| File | Purpose |
|------|---------|
| `lib/exhub/mcp/smart_decide_server.ex` | MCP server using Anubis.Server |
| `lib/exhub/mcp/tools/smart_decide.ex` | Tool implementation + pure helpers |
| `lib/exhub/mcp/hub/built_in_registry.ex` | Built-in server registration |
| `lib/exhub/router.ex` | `/smart-decide/mcp` route + moduledoc |
| `lib/exhub/application.ex` | Supervision-tree child spec |
| `lib/exhub/mcp/hub/client_manager.ex` | Hub upstream registration |
| `test/exhub/mcp/tools/smart_decide_test.exs` | Unit tests |
| `docs/modules/smart-decide.md` | User documentation |

## Error handling

- Missing `giteeai_api_key` → instruct to run `mix scr.insert dev giteeai_api_key "your-key"`
- Missing/empty `state`, `questions`, or `model` → descriptive tool error
- Invalid question type or criteria → error naming the offending question id
- API error → propagate HTTP status and body
- HTTP failure → propagate the transport reason

## Configuration

Uses the existing shared key:

```elixir
api_key = Application.get_env(:exhub, :giteeai_api_key, "")
```

## Route

Server accessible at: `/smart-decide/mcp` (built-in hub name `smart-decide`).