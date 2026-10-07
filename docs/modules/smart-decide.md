# exhub-smart-decide

The `exhub-smart-decide` module provides MCP-based **structured decision
making** using the System One approach — the same primitive popularised by
TypeSafe's [Jev](https://docs.typesafe.ai/primitives/choice) and implemented by
[Bespoke Nimble](https://github.com/bespokelabsai/nimble). It is served by Gitee
AI / [moark](https://moark.com) as the `Intern-Decision-4B` model (8K context)
by default. The same endpoint serves the whole decision-model family (see
[Model selection](#model-selection)).

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
| `model` | string | | `Intern-Decision-4B` | System One model. Defaults to `Intern-Decision-4B` (8K context); see [Model selection](#model-selection) for the rest of the family (incl. `APUS-OpenJev-v1-9B`, 128K). |
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
  "model": "Intern-Decision-4B",
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

## Model selection

Moark's *Decision Model* category (and the equivalent Gitee AI listing) serves a
whole family behind the same `/v1/systemone` contract. They are drop-in
selectable with the `model` field — verified live against the running release —
and differ mainly in context, modality, calibration, and price:

| Model | Base | Context | ￥/M in | Notes |
|-------|------|---------|--------|-------|
| `laya-multilingual` | mmBERT 322M | 8K | 0.01 | 100+ languages, ~2× faster |
| `Intern-Decision-4B` | Qwen3.5-4B | 8K | 0.10 | **default**; best-calibrated (ECE 0.065); Apache-2.0 |
| `NeoHorse-Jev-4B` | NeoHorse-1-4B | 32K | 0.10 | prefill-only; strong on routing/tool choice |
| `SemIf-OpenJev-4B` | Qwen3.5-4B | 128K | 0.10 | long-context |
| `DiffusionGemma-26B-A4B-it-Jev` | Gemma 26B MoE | 64K | 0.30 | diffusion LM |
| `APUS-OpenJev-v1-4B` | Qwen3.5-4B | 128K | 0.10 | lightweight APUS |
| `APUS-OpenJev-v1-9B` | Qwen3.5-9B | 128K | 0.20 | fastest median; 128K context |
| `Bespoke-Nimble-9B` | Qwen3.5-9B | 2K | 0.20 | older Open-Jev LoRA |

The `Intern-Decision-4B`, `NeoHorse-Jev-4B`, and `DiffusionGemma` checkpoints are
multimodal upstream, but **Moark's hosted deployments are text-only today** —
sending an `images` field returns `422` (`Intern-Decision`: "This MetaX
deployment supports text only"; `NeoHorse-Jev`: "Unknown request fields:
images"; `APUS`: "Expected state, questions, model and optional effort only").
Likewise APUS's advertised `effort="low"` compute depth is **not enabled** on the
hosted deployment (returns `422 unsupported_effort`).

### Per-scenario recommendations

| Scenario | Suggested model | Why |
|----------|-----------------|-----|
| Default (all ExHub decision sites) | `Intern-Decision-4B` | top accuracy tier at 0.10 ￥/M; best-calibrated probabilities; 8K fits the truncation limits |
| Long prompts / wide browser pages | `APUS-OpenJev-v1-9B` / `APUS-OpenJev-v1-4B` | 128K context |
| Cheapest acceptable filtering | `NeoHorse-Jev-4B` / `APUS-OpenJev-v1-4B` | 0.10 ￥/M, 32K/128K |
| Multilingual / non-English content | `laya-multilingual` | 100+ languages, ~2× faster, 0.01 ￥/M (weak on English) |

These are informed by ExHub's own labelled benchmark (177 cases spanning the
hub/brain/web/desktop relevance filters, the working-dir and browser-agent
decisions, and the `noul`/`choice`/`score` types): `Intern-Decision-4B` and
`Bespoke-Nimble-9B` led on accuracy (97.2% / 97.7%) — `Intern-Decision-4B` at
half the price and a usable 8K context — while `DiffusionGemma` ran ~7× slower
and `laya-multilingual` (52%) is only viable off-English. The default can be
overridden per scenario via the module's config (`:model` under
`config :exhub, <Relevance module>`) or the tool's `model` argument.

## Notes and limits

- The schema is **flat**: each question's answer is independent and cannot see
  the answers to other questions.
- The model only picks from the answers you supply — it cannot write text or
  return nested JSON.
- Each `choice` question supports at most 26 options.
- `APUS-OpenJev-v1-9B`, `APUS-OpenJev-v1-4B`, and `SemIf-OpenJev-4B` accept up to
  128K tokens; `DiffusionGemma-26B-A4B-it-Jev` 64K, `NeoHorse-Jev-4B` 32K,
  `Intern-Decision-4B` and `laya-multilingual` 8K, and the older
  `Bespoke-Nimble-9B` rejects prompts exceeding 2048 tokens.
- `instructions` is required for every question and must be non-empty.
- The model is selected by name; the API serves `Intern-Decision-4B` (default)
  and the rest of the family listed above.

### Answers are bound to the evidence in `state` and `instructions`

The model sorts the evidence you give it; it cannot supply facts that are not
there. Both halves of the prompt move the answer, which matters when a decision
depends on environment rather than on the text of the task:

- Measured on the mainland-China proxy question (`Exhub.MCP.Desktop.ProxyEnv`):
  a real `curl https://www.google.com` timeout scored `needs_proxy` **0.033**
  with plain instructions, and **0.992** once `instructions` stated the premise
  that this host sits behind the Great Firewall — same evidence, same command.
- Confidence then tracks the *facts*: with the premise but no measurement of the
  target, a domestic `www.baidu.com` timeout over-scored at 0.958; adding the
  measured direct TCP probe of the target host moved it to 0.182 while the
  genuinely blocked case held at 0.9998.
- Prose is not a constraint. Told that an endpoint is listed in `NO_PROXY`, the
  model proxied it anyway (0.706) — so treat policy statements in the prompt as
  advisory and enforce what must hold in code.

Practical consequence: put the assumptions that decide the answer into
`instructions`, feed the observable facts into `state`, and never let a
high-probability answer stand in for a measurement you could have taken.
See `docs/modules/desktop.md` ("Why the wording of the prompt is load-bearing").

## Endpoint

MCP endpoint: `/smart-decide/mcp` (built-in server name `smart-decide`).

Backend: `POST https://api.moark.com/v1/systemone` — one endpoint serves the
whole decision-model family; the `model` field chooses which one.