# Brain Hyper Search Policy Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a configurable *search policy* layer to `brain_search_vault` — named bundles of retrieval + ranking hyper-parameters (retrieval channels, semantic mode, scorers, fusion, weights, min_score, top_n) selectable per call or auto-selected from query heuristics, so callers no longer hand-tune `fusion`/`weights`/`semantic` per search.

**Architecture:** Introduce three small modules under `lib/exhub/mcp/brain/search/`: `Policy` (struct + behaviour), `Policies` (registry: built-ins merged over `:exhub -> :brain_search` config), and `Selector` (query heuristics for `auto` policy selection). Refactor `SearchVault.execute/2` to resolve the active policy and derive retrieval + ranking decisions from it, while keeping every existing parameter backward-compatible (explicit call args win over policy, policy wins over config defaults).

**Tech Stack:** Elixir, ExUnit, Anubis MCP component, existing `Ranker`/`Fusion`/`Scorers` pipeline.

---

## Design Reference

### Policy struct

```elixir
%Exhub.MCP.Brain.Search.Policy{
  name: "balanced",
  description: "...",
  retrieval: [:content],        # [:content] | [:filename] | [:both]
  semantic: :auto,              # :off | :auto | :on
  semantic_limit: 10,
  fusion: "weighted_sum",       # nil = fall back to :brain_ranking default
  weights: nil,                 # nil = fall back to :brain_ranking weights
  min_score: nil,               # nil = fall back to :brain_ranking min_score
  scorers: nil,                 # nil = default scorer list
  top_n: nil                    # nil = no cap on returned files
}
```

### Built-in policies

| name       | retrieval   | semantic | notes                                       |
|------------|-------------|----------|---------------------------------------------|
| `balanced` | `[:content]`| `:auto`  | default; current weights from `:brain_ranking` |
| `keyword`  | `[:content]`| `:off`   | fast; weights default, `bm25` boosted       |
| `semantic` | `[:content]`| `:on`    | `semantic` weight boosted to 0.6            |
| `recency`  | `[:content]`| `:auto`  | `freshness` weight boosted to 0.6           |
| `filename` | `[:filename]`| `:off`  | filename-only channel                       |

### Precedence (lowest → highest)

1. Built-in policy defaults
2. Named policy config under `:exhub -> :brain_search -> "policies"` (deep-merged)
3. Per-call explicit params (`search_type`, `semantic`, `semantic_limit`, `fusion`, `weights`, `min_score`)
4. Inline `policy` map param (highest; only overrides keys it sets)

### Config shape (add to `config/config.exs`)

```elixir
config :exhub, :brain_search,
  %{
    "default_policy" => "auto",        # "auto" | any built-in/custom name
    "semantic_autodetect" => true,     # allow Selector to turn ON semantic in :auto mode
    "policies" => %{
      # optional custom policies; deep-merged over built-ins of the same name
      # "keyword" => %{"weights" => %{"bm25" => 0.6}}
    }
  }
```

---

### Task 1: Write Policy + Policies unit tests (failing first)

**Files:**
- Create: `test/exhub/mcp/brain/search/policy_test.exs`
- Create: `test/exhub/mcp/brain/search/policies_test.exs`

**Step 1: Write failing tests**

`policy_test.exs` — cover:
- `Policy.new/1` returns a struct with defaults (`retrieval: [:content]`, `semantic: :auto`, `fusion: nil`).
- String keys and atom keys both accepted: `Policy.new(%{"name" => "x", "semantic" => "on"})`.
- `semantic` normalizes `"on"`/`"off"`/`"auto"` to atoms.
- `Policy.merge(policy, overrides)` overrides only provided keys.

`policies_test.exs` — cover:
- `Policies.get("balanced")` returns the built-in, with default-policy fallback.
- Deep merge: config `"keyword" => %{"weights" => %{"bm25" => 0.6}}` keeps the other built-in keyword weights (`title_match` still present) and bumps `bm25`.
- Unknown name falls back to `Policies.default/0` (the `"default_policy"` config value, `"balanced"` if unset).
- `Policies.resolve(nil)` → default; `Policies.resolve("semantic")` → named; `Policies.resolve(%Policy{})` / map → inline (maps use `Policy.new/1`).

Use `Application.put_env(:exhub, :brain_search, ...)` in tests; clean up in `on_exit`.

**Step 2: Run tests to verify failure**

Run: `rtk mix test --no-start test/exhub/mcp/brain/search/policy_test.exs test/exhub/mcp/brain/search/policies_test.exs`

Expected: FAIL — modules `Exhub.MCP.Brain.Search.Policy` / `Policies` do not exist.

---

### Task 2: Implement Policy + Policies

**Files:**
- Create: `lib/exhub/mcp/brain/search/policy.ex`
- Create: `lib/exhub/mcp/brain/search/policies.ex`

**Step 1: Implement `Policy`**

- `@enforce_keys [:name]`, `defstruct` with the fields above (all optional).
- `new(map)` — accept string or atom keys; normalize `semantic` string→atom; ignore unknown keys; default `name` to `"balanced"` when absent (inline map case).
- `merge(policy, overrides)` — `Map.merge` semantics over the struct's non-nil behaviour (only keys present in overrides change; for `weights`, deep-merge maps).

**Step 2: Implement `Policies`**

- `builtins/0` — map of the five built-ins (encoding the table above; weights for `keyword`/`semantic`/`recency` are the `:brain_ranking` default weights with one signal boosted, resolved lazily via `default_weights/0`).
- `config/0` — reads `:exhub -> :brain_search` (default `%{}`), String-keyed.
- `all/0` — `builtins()` deep-merged over `config["policies"]`.
- `get(name)` / `default/0` — lookup with fallback to `"default_policy"` config value, then `"balanced"`.
- `resolve(nil | name | map | %Policy{})` — map → `Policy.new/1`; struct → as-is; name (string/atom) → `get/1`.

**Step 3: Run tests**

Run: `rtk mix test --no-start test/exhub/mcp/brain/search/policy_test.exs test/exhub/mcp/brain/search/policies_test.exs`

Expected: PASS.

**Step 4: Commit**

```bash
git add test/exhub/mcp/brain/search/ lib/exhub/mcp/brain/search/
git commit -m "feat(brain): add search Policy struct and Policies registry"
```

---

### Task 3: Write Selector heuristics tests (failing first)

**Files:**
- Create: `test/exhub/mcp/brain/search/selector_test.exs`

**Step 1: Write failing tests**

Cover `Selector.select(query)` → policy name (string):
- `"tag:project/active"` → `"keyword"` (semantic-off path; tag logic already query-driven).
- `"recent meeting notes"` → `"recency"` (contains `recent`/`latest`/`new`/`newest`).
- `"how do we handle authentication and login flows in the app"` (≥ 4 words) → `"semantic"`.
- `"groceries"` (single word, no stopwords) → `"keyword"`.
- `"meeting"` (single word) → `"keyword"` (fast path).
- anything else (e.g. `"meeting notes"`) → `"balanced"`.

**Step 2: Run tests to verify failure**

Run: `rtk mix test --no-start test/exhub/mcp/brain/search/selector_test.exs`

Expected: FAIL — `Exhub.MCP.Brain.Search.Selector` does not exist.

---

### Task 4: Implement Selector

**Files:**
- Create: `lib/exhub/mcp/brain/search/selector.ex`

**Step 1: Implement `select/1`**

Heuristic order:
1. `tag:` prefix → `"keyword"`.
2. Recency keywords (`recent`, `latest`, `newest`, `new`) → `"recency"`.
3. Tokenized query with ≥ 4 terms (natural-language phrasing) → `"semantic"`.
4. Single word with no spaces → `"keyword"`.
5. Otherwise → `"balanced"`.

Keep it pure (no config/I-O) so it is trivially testable.

**Step 2: Run tests**

Run: `rtk mix test --no-start test/exhub/mcp/brain/search/selector_test.exs`

Expected: PASS.

**Step 3: Commit**

```bash
git add test/exhub/mcp/brain/search/selector_test.exs lib/exhub/mcp/brain/search/selector.ex
git commit -m "feat(brain): add search policy Selector heuristics"
```

---

### Task 4b: Integrate policy into SearchVault (tests first)

**Files:**
- Modify: `lib/exhub/mcp/tools/brain/search_vault.ex`
- Create: `test/exhub/mcp/tools/brain/search_vault_policy_test.exs`

**Step 1: Write failing integration tests**

`search_vault_policy_test.exs` (mirror `search_vault_test.exs` setup — temp vault, `Application.put_env(:exhub, :obsidian_vault_path, v)`):

- `policy: "filename"` returns only filename hits (a note whose filename does not match the query is excluded even if its body matches).
- `policy: "keyword"` returns matches without a `semantic=` signal.
- Inline map: `policy: %{"semantic" => "on", "weights" => %{"semantic" => 0.9}}` runs the vector path; assert the output shows `semantic=`. (Reuse the `StubEmbedder`/unique-`VectorIndex` server pattern from `search_vault_semantic_test.exs`.)
- Explicit param overrides policy: `policy: "keyword", semantic: true` still enables semantic.
- `policy: "auto"` + a conversational query (`"how do we handle authentication"`) enables semantic when `"semantic_autodetect" => true`; with `"semantic_autodetect" => false` it does not.
- Unknown policy name falls back to default without raising.

**Step 2: Run tests to verify failure**

Run: `rtk mix test --no-start test/exhub/mcp/tools/brain/search_vault_policy_test.exs`

Expected: FAIL — `policy` param is currently rejected by the schema / ignored.

**Step 3: Implement policy resolution in `SearchVault`**

- Add `field(:policy, :string, ...)` and `field(:policy_map, :map, ...)` are **not** needed — accept only `policy` as string; inline maps come through `weights`-style loose typing only if desired. **Decision:** support `policy` string param only in the schema (inline maps are a `Policies.resolve/1` capability used by tests via direct module calls, and by `policy` returning a JSON map if the schema allows `:any`; keep schema `:string` for MCP simplicity and accept inline maps from `params` when `is_map(policy)`).
- In `execute/2`, after parsing params:
  - `policy = Policies.resolve(params.policy)` (nil → default/auto).
  - Auto-select: if `policy.name == "auto"` (the configured `"default_policy"` can be `"auto"`; built-in `balanced` is the concrete result of `Selector.select/1`), replace with `Policies.get(Selector.select(query))`.
  - Apply precedence: start from `policy`, override with explicit params (`search_type` → `retrieval`, `semantic`, `semantic_limit`, `fusion`, `weights`, `min_score`) when present (`Map.get(params, key) != nil`).
  - `semantic_enabled?`: `policy.semantic == :on` → true; `== :off` → false; `== :auto` → `Selector` word-count heuristic returns ≥ 4 words AND `"semantic_autodetect"` config is true AND not a tag search.
  - `search_type`/retrieval from `policy.retrieval` (mapped `[:filename] → "filename"`, `[:both] → "both"`, `[:content] → "content"`) unless explicit `search_type` given.
  - Build `rank_opts` from policy (`scorers`, `fusion`, `weights`, `min_score`, `context`) using existing `maybe_put_scorers`; apply `top_n` via `Enum.take(ranked, top_n)` after ranking (before `absolutize`).
- Keep tag searches semantic-off (existing guard in `maybe_semantic/5` covers this).

**Step 4: Run new + existing brain tests**

Run: `rtk mix test --no-start test/exhub/mcp/tools/brain/`

Expected: PASS — new policy tests plus all existing search/semantic/blocker tests (backward compatibility).

**Step 5: Commit**

```bash
git add lib/exhub/mcp/tools/brain/search_vault.ex test/exhub/mcp/tools/brain/search_vault_policy_test.exs
git commit -m "feat(brain): wire search policies into brain_search_vault"
```

---

### Task 5: Configure defaults + docs

**Files:**
- Modify: `config/config.exs`
- Modify: `docs/modules/brain.md`
- Modify: `lib/exhub/mcp/brain_server.ex` (moduledoc tool list mention)

**Step 1: Add `:brain_search` config**

Append the `config :exhub, :brain_search, %{"default_policy" => "auto", "semantic_autodetect" => true, "policies" => %{}}` block with comments, next to `:brain_ranking`.

**Step 2: Document the `policy` parameter**

In `docs/modules/brain.md` add a `policy` row to the `brain_search_vault` parameter table (`"auto"` default, values `balanced`/`keyword`/`semantic`/`recency`/`filename`/custom config name) and a short section describing auto-selection heuristics, precedence, and custom-policy config example.

**Step 3: Update `brain_server.ex` moduledoc**

Mention `policy` in the `brain_search_vault` tool summary line.

**Step 4: Verify**

Run: `rtk mix compile --warnings-as-errors` then `rtk mix test --no-start test/exhub/mcp/tools/brain/ test/exhub/mcp/brain/search/`

Expected: PASS, no warnings.

**Step 5: Commit**

```bash
git add config/config.exs docs/modules/brain.md lib/exhub/mcp/brain_server.ex
git commit -m "docs(brain): document search policies config and policy param"
```

---

### Task 7: Full verification

**Step 1: Run full test suite**

Run: `rtk mix test`

Expected: all pass (existing suite nothing broken).

**Step 2: Manual smoke (optional)**

Rebuild the running app (`mix deps.compile`/release flow as usual) and call `brain_search_vault` with `policy: "auto"`, a conversational query, and a short query; confirm semantic kicks in only for the conversational one.