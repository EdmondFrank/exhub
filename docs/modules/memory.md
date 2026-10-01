# Memory (Beacon-style layer on the Brain vault)

## Overview

The Memory layer turns reusable knowledge from agent sessions into durable,
reviewable **project memory**, stored as notes in the Brain (Obsidian) vault.
It is ExHub's take on [Agent Beacon](https://github.com/Asymptote-Labs/agent-beacon):
the same *capture → evaluate → distill → review → reuse → promote* loop, built on
the two halves ExHub already owns — the **Brain** vault as the knowledge store
and **Smart Decide** (System One, Jev-compatible) as the evaluator.

A memory is a plain markdown note, so it stays greppable, linkable in Obsidian,
and visible to every existing Brain tool. Nothing is written to memory silently:
a candidate only becomes memory when a person approves it.

## Why

A problem solved by one agent shouldn't need to be learned from scratch by
another. Beacon captures cross-harness session history and distills reviewed
lessons. ExHub already has the store (Brain) and the evaluator (`smart_decide`);
this layer adds the **lifecycle and review discipline**.

| Beacon | ExHub memory layer |
|---|---|
| Memory store | Brain vault notes under `memory/` |
| Jev / System One evaluation | `smart_decide` (`Exhub.Memory.Evaluator`) |
| Recall ranking | Brain scorers via `Exhub.Memory.Recall` |
| Relevance precision pass | `Exhub.MCP.Brain.Search.Relevance` |
| Promote to Agent Skill | `memory_promote` → `memory/skills/<slug>/SKILL.md` |

## Architecture

```
┌────────────────┐   memory_distill    ┌──────────────────────┐
│  Agent / user  │────────────────────▶│ Exhub.Memory.Evaluator│──▶ Smart Decide
│                │                     │  (3 noul questions,   │    (System One)
│                │◀────────────────────│   task_success gate)  │
└───────┬────────┘   candidate          └──────────┬───────────┘
        │                                          │
        │ memory_approve / reject / supersede       ▼
        │                                ┌──────────────────────┐
        │                                │ Exhub.Memory.Store    │
        │                                │  vault: memory/*.md   │
        ▼                                └──────────┬───────────┘
┌────────────────┐   memory_search /               │
│  Future agent  │◀── memory_context ──────────────┘
└────────────────┘   Exhub.Memory.Recall (approved only, Brain ranking)
```

Endpoint: `/memory/mcp` — built-in hub name `memory`.

## Storage and data model

Memories live under `:exhub -> :memory -> :vault_folder` (default `memory/`) in
the vault. `start_at` is the frontmatter and the lesson body follows:

```markdown
---
memory_id: memory_1f2a9c3b4d5e6f70
status: approved
kind: debugging_pattern
title: Restart the kuri daemon after changing the Cloak config
applicability: when the kuri daemon exits right after a config change
project: exhub
tags: [project/exhub, area/browser]
source: session:abc123
evidence: [{"session":"abc123","events":[14,19]}]
evaluation: {"model":"Intern-Decision-4B","promoted":true,"mean":0.82,"probabilities":{"task_success":0.9,"reusable":0.8,"evidence_supported":0.76}}
created_at: 2026-09-30T10:00:00Z
updated_at: 2026-09-30T10:05:00Z
---

The daemon reads the Cloak config once at boot; restart it after editing.
```

* `status` — `candidate | approved | rejected | superseded` (only `approved`
  memories are recalled or promoted).
* `kind` — `workflow | correction | debugging_pattern | gotcha | convention`.
* `project` — scope key; recall matches either this field or a
  `project/<name>` tag (both are supported).
* `evaluation` / `evidence` — JSON; `tags` is a plain list so Brain tag search
  still finds memories.
* `supersedes` / `superseded_by` — provenance links; superseding never deletes
  the old note.

## Configuration

```elixir
config :exhub, :memory,
  vault_folder: "memory",
  skill_folder: "memory/skills",
  kinds: ~w(workflow correction debugging_pattern gotcha convention),
  statuses: ~w(candidate approved rejected superseded),
  evaluator: [
    enabled: true,
    model: "Intern-Decision-4B",
    task_success_min: 0.50,
    mean_min: 0.60,
    state_char_limit: 16_000
  ],
  recall: [limit: 5, filter: true]
```

The evaluator uses the shared `:exhub -> :giteeai_api_key`.

## MCP tools

| Tool | Stage | Description |
|------|-------|-------------|
| `memory_distill` | capture | Turn a session/fix into an evaluated candidate |
| `memory_candidates` | review | List the review queue |
| `memory_show` | inspect | Full memory body + evidence + provenance |
| `memory_approve` | review | Approve a candidate into memory (never automatic) |
| `memory_reject` | review | Reject a candidate with a reason |
| `memory_supersede` | review | Replace a memory with a newer one, linked |
| `memory_search` | reuse | Recall approved memory by keywords |
| `memory_context` | reuse | Recall memory relevant to a task |
| `memory_promote` | promote | Install an approved memory as an Agent Skill |

## Workflow

1. **Distill.** `memory_distill` scores a session against three `noul`
   questions — did the task succeed, is there a reusable lesson, is it
   evidence-backed — and creates a candidate only when the gate passes
   (`task_success >= 0.50` and mean `>= 0.60`), the same gate Beacon uses.
   With no evaluator key, pass `evaluate: false` to record the candidate
   directly (or `force: true` to override a failed gate).
2. **Review.** A person calls `memory_approve` (optionally supplying the
   reviewed `body`/`title`), `memory_reject`, or `memory_supersede`. Approving a
   memory with no lesson body is refused, and nothing that looks like a
   credential may be stored.
3. **Reuse.** Before a non-trivial task, call `memory_context` with two or three
   distinctive keywords. Terms are **ANDed** (a full sentence over-constrains
   the search). Results are ranked with the Brain scorers and filtered by Smart
   Decide. Apply a memory only when its `applicability` matches, cite its
   `memory_id`, and when it conflicts with the user's instructions or the
   repository docs, follow those and say so.
4. **Promote.** `memory_promote` writes `memory/skills/<slug>/SKILL.md` for an
   approved memory, carrying provenance in its frontmatter.

## Guardrails

* **Review-gated.** Candidates are never approved automatically; `memory_approve`
  is the only path to `approved`.
* **No placeholder lessons.** Approving an empty body (or the evaluator's
  "no lesson text was extracted") is refused.
* **Secrets never stored.** `memory_distill` and `memory_approve` refuse content
  matching common credential patterns (`Exhub.Memory.SecretScan`).
* **Local-first.** Everything lives in the vault; only the evaluator call is
  networked (`smart_decide`), and it is opt-out via `evaluate: false`.
* **Provenance and supersede.** Every memory keeps its evaluation, evidence and
  supersede links rather than losing history.

## See Also

- `docs/modules/brain.md` — the vault store and search
- `docs/modules/smart-decide.md` — the System One evaluator
- `docs/plans/2026-09-30-memory-layer.md` — design/implementation plan
