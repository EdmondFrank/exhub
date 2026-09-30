# Memory Layer (Beacon-style) Design

## Overview

Add an ExHub **memory layer** inspired by
[agent-beacon](https://github.com/Asymptote-Labs/agent-beacon): a review-gated
loop that turns agent-session knowledge into durable project memory, built on the
Brain (Obsidian) vault and Smart Decide (System One).

Beacon's loop is *capture → evaluate (Jev/System One) → distill → review →
reuse → promote*. ExHub already has the store (Brain) and the evaluator
(`smart_decide`, the same Jev-compatible `/v1/systemone` endpoint). The memory
layer adds the lifecycle and review discipline, storing memories as vault notes.

## Architecture

```
Memory MCP tools ──▶ Exhub.Memory.{Store,Evaluator,Recall,Review,Promote,SecretScan}
                          │                     │
                    vault: memory/*.md    SmartDecide.decide/3 (System One)
```

A memory is a markdown note under `memory/` whose frontmatter carries the whole
lifecycle; the body is the lesson. This keeps memories greppable/linkable and
visible to every existing Brain tool, and makes recall reuse the Brain ranking
scorers (`Exhub.MCP.Brain.Ranking.Ranker`) and the Smart Decide precision filter
(`Exhub.MCP.Brain.Search.Relevance`).

## Data model

Frontmatter (string keys): `memory_id`, `status` (`candidate|approved|rejected|
superseded`), `kind` (`workflow|correction|debugging_pattern|gotcha|
convention`), `title`, `applicability`, `project`, `tags`, `source`, `evidence`
(JSON), `evaluation` (JSON), `supersedes`, `superseded_by`, `created_at`,
`updated_at`. One id (`memory_<hex>`) per record; filenames are `<id>.md`.

`Exhub.Memory.Frontmatter` encodes/decodes this without a YAML dependency:
scalars as `key: value`, string lists (tags) as `[a, b]`, maps/nested lists as
inline JSON; ambiguous scalars are JSON-quoted for round-trip fidelity.

## Modules

| Module | Purpose |
|--------|---------|
| `Exhub.Memory.Frontmatter` | Encode/decode memory frontmatter (pure) |
| `Exhub.Memory.SecretScan` | Pre-write credential detection (pure) |
| `Exhub.Memory.Store` | Vault CRUD for memory notes, ids, filtering |
| `Exhub.Memory.Evaluator` | Beacon 3-question System One gate over `SmartDecide` |
| `Exhub.Memory.Recall` | Approved-only, ANDed-keyword recall via Brain ranking |
| `Exhub.Memory.Review` | approve/reject/supersede lifecycle (review-gated) |
| `Exhub.Memory.Promote` | memory → `memory/skills/<slug>/SKILL.md` |

## Tools (server `Exhub.MCP.MemoryServer`, `/memory/mcp`)

`memory_distill`, `memory_candidates`, `memory_show`, `memory_approve`,
`memory_reject`, `memory_supersede`, `memory_search`, `memory_context`,
`memory_promote` — implemented under `lib/exhub/mcp/tools/memory/`.

## Evaluation gate

Three `noul` questions: `task_success`, `reusable`, `evidence_supported`.
Promoted when `task_success >= 0.50` **and** mean `>= 0.60` (Beacon's gate).
The evaluator never writes lesson text; the agent drafts the lesson and the user
approves it.

## Configuration

`config :exhub, :memory` — `vault_folder`, `skill_folder`, `kinds`, `statuses`,
`evaluator` (enabled/model/thresholds), `recall` (limit/filter). Uses the shared
`:giteeai_api_key`. See `docs/modules/memory.md`.

## Files

| File | Purpose |
|------|---------|
| `lib/exhub/memory/*.ex` | Layer modules |
| `lib/exhub/mcp/memory_server.ex` | MCP server |
| `lib/exhub/mcp/tools/memory/*.ex` | Tool components |
| `lib/exhub/mcp/hub/built_in_registry.ex` | Built-in registration (`memory`) |
| `lib/exhub/router.ex` | `/memory/mcp` route + moduledoc |
| `lib/exhub/application.ex` | Supervisor child spec |
| `config/config.exs` | `:exhub, :memory` block |
| `test/exhub/memory/*_test.exs` | Unit tests |
| `docs/modules/memory.md` | User documentation |

## Testing

Pure/unit tests with a temporary vault (`:obsidian_vault_path` pointed at a tmp
dir) and an injected evaluator decider, so no network or app boot is needed:

```sh
mix test --no-start test/exhub/memory/
```

Covers frontmatter round-trips, secret scanning, store CRUD/filtering, the
evaluation gate, approved-only/ANDed recall with project scoping, review
transitions (including placeholder/secret refusal), and promotion.

## Phases

1. Store + Evaluator + distill/candidates/show (done)
2. Recall + scoping (done)
3. Review + Promote + supersede chains (done)
4. Wiring + docs (done)
5. *(Future)* auto-capture: feed traces from Sagents session state / a
   `~/.config/exhub/traces/` JSONL into the same evaluate → review loop.
