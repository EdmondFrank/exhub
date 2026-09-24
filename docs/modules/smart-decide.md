# exhub-smart-decide

The `exhub-smart-decide` module provides MCP-based **structured decision
making** using the System One approach — the same primitive popularised by
TypeSafe's [Jev](https://docs.typesafe.ai/primitives/choice) and implemented by
[Bespoke Nimble](https://github.com/bespokelabsai/nimble). It is served by Gitee
AI / [moark](https://moark.com) as the `APUS-OpenJev-v1-9B` model (8k-token
context), with `Bespoke-Nimble-9B` (2k context) still available.

Unlike a chat model, Smart Decide generates **no reasoning and no free-form
text**. It reads the logits for the allowed answer tokens, turns them into
probabilities, and returns a typed answer per question. This makes it fast and
cheap for high-frequency micro-decisions: routing, policy checks, yes/no
judgments, and rubric scoring.

## Setup

### Configuration

Uses the existing Gitee AI key (shared with `look`, `listen`, etc.):

```bash
mix scr.insert dev giteeai_api_key "your-api-key"
```

Get your API key from [Gitee AI](https://moark.com).

## Tool: `smart_decide`

Evaluate a `state` against a set of typed `questions`.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `state` | string \| object \| array | ✓ | — | The content to judge. JSON object/array strings are decoded automatically. |
| `questions` | object | ✓ | — | Map of question-id => typed question. JSON strings are decoded automatically. |
| `model` | string | | `APUS-OpenJev-v1-9B` | System One model. Defaults to `APUS-OpenJev-v1-9B` (8k-token context); `Bespoke-Nimble-9B` (2k context) is also served. |
| `compact` | boolean | | `false` | Drop `probabilities`/`confidence`/`legend` from the answers |

### Question types

Every question has a `type` and a **required, non-empty** `instructions`
description (the API rejects blank descriptions), plus a type-specific
`criteria`:

| `type` | `criteria` | Answer |
|--------|------------|--------|
| `noul` | optional object mapping `"true"`/`"false"` to descriptions | `noul` — probability the answer is yes (0–1) |
| `choice` | **required** object mapping each option to its rubric description, or a non-empty list of option strings (≥2 options; a blank description defaults to the option name) | `choice` + `probabilities` + `confidence` |
| `score` | **required** ordered list of levels (at least 2) | `score` (probability-weighted) + `legend` + `probabilities` + `confidence` |

The question keys are opaque identifiers you choose; answers come back under the
same keys. Keys are not sent to the model and do not affect inference.

### Response

```json
{
  "model": "APUS-OpenJev-v1-9B",
  "answers": {
    "is_urgent":   { "type": "noul",   "noul": 0.92 },
    "department":  { "type": "choice", "choice": "technical",
                     "probabilities": { "billing": 0.08, "technical": 0.85, "sales": 0.07 },
                     "confidence": 0.82 },
    "frustration": { "type": "score",  "score": 1.6,
                     "legend": { "0": "Calm", "1": "Frustrated", "2": "Very angry" },
                     "probabilities": { "0": 0.05, "1": 0.3, "2": 0.65 },
                     "confidence": 0.78 }
  }
}
```

With `compact: true` the probability detail is dropped, leaving only `type` and
the chosen value (`noul`, `choice`, or `score`).

## Usage examples

### Route a support request

```json
{
  "state": "Help! My payouts have been failing for 3 days.",
  "questions": {
    "is_urgent": { "type": "noul", "instructions": "Does this convey urgency?" },
    "department": {
      "type": "choice",
      "instructions": "Which team should handle this?",
      "criteria": {
        "billing": "Payments, invoicing, refunds",
        "technical": "Bugs, outages, integrations",
        "sales": "Pricing, upgrades, new accounts"
      }
    },
    "frustration": {
      "type": "score",
      "instructions": "How frustrated is the customer?",
      "criteria": ["Calm", "Frustrated", "Very angry"]
    }
  }
}
```

### Apply a policy to structured state

`state` can be an object, so you can pass a record directly:

```json
{
  "state": { "order_total": 1200, "customer_tier": "gold", "coupon": "SAVE20" },
  "questions": {
    "eligible_for_discount": { "type": "noul", "instructions": "Is this order eligible for the SAVE20 discount?" }
  }
}
```

### Token-economical output

```json
{
  "state": "The item was bought 12 days ago; the store accepts returns within 30 days.",
  "questions": {
    "eligible": { "type": "noul", "instructions": "Is this item within the store return window?" }
  },
  "compact": true
}
```

## Programmatic API

`Exhub.MCP.Tools.SmartDecide.decide/3` exposes the same decision without the MCP
frame — it returns `{:ok, %{"model" => …, "answers" => …}}` or `{:error, message}`
and accepts `:model`, `:compact`, and `:api_key` options. The MCP Hub uses it to
filter `retrieve_tools` candidates and the Brain server to filter
`brain_search_vault` results (see [`docs/modules/mcp-hub.md`](mcp-hub.md) →
*Smart Decide relevance filtering* and [`docs/modules/brain.md`](brain.md) →
*Relevance filtering*).

```elixir
{:ok, %{"answers" => %{"relevant" => %{"noul" => 0.93}}}} =
  Exhub.MCP.Tools.SmartDecide.decide(
    "Tool: desktop__read_file\nDescription: Read a file's contents",
    %{"relevant" => %{"type" => "noul", "instructions" => "Does this tool help read a file?"}}
  )
```

## Notes and limits

- The schema is **flat**: each question's answer is independent and cannot see
  the answers to other questions.
- The model only picks from the answers you supply — it cannot write text or
  return nested JSON.
- Each `choice` question supports at most 26 options.
- `APUS-OpenJev-v1-9B` accepts prompts up to ~8k tokens; the older
  `Bespoke-Nimble-9B` rejects prompts exceeding 2048 tokens.
- `instructions` is required for every question and must be non-empty.
- The model is selected by name; the API serves `APUS-OpenJev-v1-9B` (default)
  and `Bespoke-Nimble-9B`.

## Endpoint

MCP endpoint: `/smart-decide/mcp` (built-in server name `smart-decide`).

Backend: `POST https://api.moark.com/v1/systemone`.