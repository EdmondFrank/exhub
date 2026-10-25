# ToonFlow ← BigBanana Alignment — Implementation Plan

**Date**: 2026-10-25
**Status**: Implemented — milestones M1–M6 complete (2026-10-25), server/MCP only
**Scope**: Tier 1 + Tier 2 borrowings from `shuyu-labs/BigBanana-AI-Director`, **server / MCP only** (no canvas UI work)
**Design refs**: [2026-09-30-toonflow-design.md](2026-09-30-toonflow-design.md), [2026-09-30-toonflow-implementation.md](2026-09-30-toonflow-implementation.md)
**Module ref**: [docs/modules/toonflow.md](../modules/toonflow.md), [docs/toonflow-guide.md](../toonflow-guide.md)
**Reference (concepts only — CC BY-NC-SA 4.0, source unpublished)**: [github.com/shuyu-labs/BigBanana-AI-Director](https://github.com/shuyu-labs/BigBanana-AI-Director)

---

## 1. Context & goals

`exhub-toonflow` is the native Elixir/OTP re-implementation of the short-drama
pipeline. A gap analysis against BigBanana AI Director identified craft features
ToonFlow lacks. This plan adds the **Tier 1 and Tier 2** items as new
`Exhub.Toonflow.*` modules + MCP tools, without a custom UI in this pass.

Goals:

1. **Keyframe-driven shots** — first-class start/end frames per shot, frame
   continuity between shots, and real `fl2va` (first **and** last frame) video.
2. **Asset depth** — persist scenes/props (not just mirror them), generate their
   concept/reference images, and make generation **context-aware** (scene ref +
   character ref, not text alone).
3. **Wardrobe** — multiple looks ("variations") per character, per-shot
   selection, "change the outfit, keep the face" i2i mode.
4. **Shot tooling** — 9-grid candidate generation + ffmpeg cell crop, and
   AI shot-splitting into sub-shots.
5. **Prompt control** — per-project prompt overrides with version history and
   revert, plus worldview anchors injected into generation.
6. **Plan pre-review** — expose a per-project/per-shot readiness plan before a
   batch run.

Non-goals (this pass): canvas/web UI changes, cross-project asset library,
season/episode hierarchy, timeline/rough-cut editor, multi-provider registry.

---

## 2. Locked design decisions

| # | Decision | Rationale |
|---|----------|-----------|
| D1 | **Keyframes are `assets` rows** with `kind = keyframe_start \| keyframe_end` (image in `path`, prompt in `prompt`, extras in `meta`). A *pending* keyframe is represented by the shot's prompt in `shots.meta` with **no** asset row. | Reuses `Media.insert_asset/3`, `latest_asset/4`, `list_assets/3` and the snapshot's `{shot_id, kind}` map. No new table. |
| D2 | **9-grid crop via ffmpeg** (crop a 3×3 cell out of one grid image). | Matches BigBanana; `ffmpeg` already required for `assemble`. |
| D3 | **Prompt overrides are per-project** (`prompt_overrides` table layered over `priv/toonflow/prompts/*.md`). | Project-level control; no filesystem writes; versioned rows. |
| D4 | **Server / MCP only.** No `ToonflowView`/`router.ex` changes. | Smallest reviewable surface; UI is a follow-up. |
| D5 | **No column migrations.** New entities → new tables (`CREATE TABLE IF NOT EXISTS`, re-applied on every `DB.open/2`, so safe on existing DBs). New per-shot/per-character fields → `meta_json`. | No migration runner exists. |
| D6 | **Behaviour-injected clients** (`Media.Client`, `Video.Client`) and **pure helpers** for all new logic. | Unit-testable with `mix test --no-start`, no network. |
| D7 | **No new supervision children.** All code + config. | Hot-deploy per `AGENTS.md`. |

---

## 3. Data model changes

### 3.1 New tables (`Exhub.Toonflow.Schema` `@project_ddl`)

```sql
CREATE TABLE IF NOT EXISTS character_variations (
  id TEXT PRIMARY KEY,
  character_id TEXT NOT NULL,
  name TEXT NOT NULL,
  visual_prompt TEXT,
  refs_json TEXT,
  meta_json TEXT
);
CREATE INDEX IF NOT EXISTS idx_variations_character ON character_variations(character_id);

CREATE TABLE IF NOT EXISTS scenes (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  description TEXT,
  refs_json TEXT,
  meta_json TEXT
);
CREATE INDEX IF NOT EXISTS idx_scenes_name ON scenes(name);

CREATE TABLE IF NOT EXISTS props (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  description TEXT,
  refs_json TEXT,
  meta_json TEXT
);
CREATE INDEX IF NOT EXISTS idx_props_name ON props(name);

CREATE TABLE IF NOT EXISTS prompt_overrides (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  version INTEGER NOT NULL DEFAULT 1,
  content TEXT NOT NULL,
  note TEXT,
  created_at TEXT NOT NULL
);
CREATE INDEX IF NOT EXISTS idx_prompt_overrides_name ON prompt_overrides(name, version);
```

Plus matching `@*_columns` strings and `decode_variation/1`, `decode_scene/1`,
`decode_prop/1`, `decode_prompt_override/1` (pure, alongside the existing
decoders).

### 3.2 Reused `meta_json` keys (no schema change)

| Table | Key | Shape | Set by |
|-------|-----|-------|--------|
| `shots` | `keyframe_start_prompt` | string | storyboard |
| `shots` | `keyframe_end_prompt` | string | storyboard |
| `shots` | `character_variations` | `%{character_name => variation_id}` | `toonflow_set_shot_variations` |
| `shots` | `link` | `"keyframe" \| "9grid"` | storyboard / plan |
| `assets` | `status` | `"completed" \| "failed"` | keyframe/image generation |
| `assets` | `grid` | `%{cols, rows, cell_map}` | candidate generation |

### 3.3 Asset `kind` vocabulary (extended)

`image`, `video`, `audio` (existing) · **new**: `keyframe_start`,
`keyframe_end`, `candidates`, `scene_ref`†, `prop_ref`†, `variation_ref`†.

† Character/scene/prop/variation reference images also record `refs` on their
owning row; the `*_ref` asset kinds are optional bookkeeping for listing.

---

## 4. Phase 7 — Tier 1

### 7.1 Keyframes + continuity + first/last-frame video

**Create** `lib/exhub/toonflow/keyframe.ex` (`Exhub.Toonflow.Keyframe`)

| Function | Behaviour |
|----------|-----------|
| `generate(project, opts, server)` | opts: `:shot_id` (req), `:type` (`"start"\|"end"`, req), `:prompt`, `:refs`, `:model`, `:size`. Resolves prompt (explicit → shot `meta.keyframe_{type}_prompt` → shot `prompt`), builds refs via `Media.refs_for_shot/3`, calls `Media.generate_image/3` with `kind: "keyframe_#{type}"`. Returns `{:ok, asset}`. |
| `copy(project, opts, server)` | opts: `:shot_id`, `:direction` (`"prev_end" \| "next_start"`). Copies the sibling shot's `keyframe_end` (or `keyframe_start`) **path+prompt** into this shot's `keyframe_start` (or `keyframe_end`) as a new asset row. Returns `{:ok, asset}` / `{:error, :no_source_keyframe}`. |
| `list_for_shot(project, shot_id, server)` | `%{"start" => asset\|nil, "end" => asset\|nil}`. |
| `set_prompt(project, shot_id, type, prompt, server)` | Writes `shots.meta.keyframe_{type}_prompt`. |
| `next_shot/3`, `previous_shot/3` | Ordered by `(script_id, idx)`. |
| `continuity_copy/2` (pure) | `(from_asset, to_type)` → attrs for the copied row; unit-tested. |

**Modify**
- `media.ex` — `generate_image/3` gains `:kind` (default `"image"`) and
  `:lighting_meta`; factor the record step to accept `kind`.
- `video.ex` — plumb `:last_frame`: add to `client_opts`; resolution order
  `keyframe_start` → `image` for first frame, `keyframe_end` for last frame;
  `Video.Default` passes `last_frame` to `VideoGen` (`fl2va` already supports
  first+last, verified in `lib/exhub/mcp/tools/video_gen.ex`).
- `storyboard.ex` — `normalize_shot` captures `keyframe_start_prompt` /
  `keyframe_end_prompt`; `characters_digest` unchanged.
- `priv/toonflow/prompts/storyboard.user.md` — request per-shot
  `keyframe_start_prompt` / `keyframe_end_prompt`.
- `pipeline.ex` — insert stage `keyframes` between `storyboard` and `images`;
  `images` stage skips shots that already have a `keyframe_start`; `videos`
  passes both frames. Add stage to `@stages` and `stage_status/4`.

**Tools** (`lib/exhub/mcp/tools/toonflow/`)
- `toonflow_generate_keyframe` `{project, shot_id, type, prompt?, refs?, model?, size?}`
- `toonflow_copy_keyframe` `{project, shot_id, direction}`

**Tests** `test/exhub/toonflow/keyframe_test.exs` — `continuity_copy/2`,
prompt resolution order, `next_shot/previous_shot` (pure).

### 7.2 Scenes & props persistence + reference images

**Modify** `lib/exhub/toonflow/assets.ex`
- `extract_assets/3` persists `scenes`/`props` (upsert by name) — currently only
  mirrored to `characters/appearance.json`; keep the mirror.
- New: `list_scenes/3`, `list_props/3`, `get_scene/3`, `upsert_scene/2`,
  `upsert_prop/2`.
- New: `generate_reference/3` — opts `:kind` (`"character"|"scene"|"prop"`),
  `:name`; builds a reference prompt (regional/style prefix + description) and
  calls `Media.generate_image/3`, storing the path in the row's `refs_json`.
- `parse_assets/1` unchanged (already returns `scenes`/`props`).

**Tools**
- `toonflow_list_scenes` `{project, limit?}`
- `toonflow_list_props` `{project, limit?}`
- `toonflow_generate_reference` `{project, kind, name, model?, size?}`

**Tests** — extend `assets_test.exs`: scene/prop normalization + decode.

### 7.3 Context-aware reference assembly

**Modify** `lib/exhub/toonflow/media.ex`
- New pure `refs_for_shot(shot, ctx, opts)` where `ctx =
  %{characters, scenes, variations}` → ordered, de-duplicated, existence-filtered
  refs: **explicit → scene ref → per-character ref** (variation-aware in 8.4).
- `resolve_refs/3` delegates to it.
- `generate_image/3` loads scenes + (Phase 8) variations for the shot's scene.

**Modify** `lib/exhub/toonflow/storyboard.ex` — `shot_prompt/2` appends the
scene description alongside character appearances.

**Tests** — `media_test.exs` `refs_for_shot/3` ordering, dedup, missing-file
filtering.

**Phase 7 exit criteria**
- `toonflow_extract_assets` → `toonflow_generate_reference` →
  `toonflow_generate_keyframe` (start/end) → `toonflow_copy_keyframe` →
  `toonflow_generate_video` (`fl2va`, first+last) runs end-to-end; the clip is
  `ffprobe`-verifiable between the two frames.
- `toonflow_pipeline_run` with `stages: ["assets","storyboard","keyframes","videos"]`
  works and is resumable.
- `mix compile --force --warnings-as-errors` clean; unit tests green.

---

## 5. Phase 8 — Tier 2

### 8.4 Wardrobe / character variations

**Modify** `assets.ex`
- `add_variation/4` (character_id, name, visual_prompt) + optional generation;
  `list_variations/3`; `variations_by_name/3` → `%{name => variation}` for a
  character.
- `generate_variation/3` — variation-mode i2i: conditions on the character's
  base `refs`, prompt instructs "keep face/hair/hair-colour/skin/face-structure
  identical; change only the outfit per `<visual_prompt>`"; stores result in
  that variation's `refs_json` (asset kind `variation_ref`).

**Modify** `media.ex`
- `generate_image/3` gains `:variation` (boolean). When true, wrap the prompt
  with the "keep-face / new-outfit" instruction and prefer the base character
  ref.
- `refs_for_shot/3` consults `shot.meta["character_variations"]` and uses the
  variation ref when set, else the base ref (BigBanana's
  `getRefImagesForShot` semantics).

**Tools**
- `toonflow_generate_variation` `{project, character, name, visual_prompt, model?, size?}`
- `toonflow_list_variations` `{project, character?}`
- `toonflow_set_shot_variations` `{project, shot_id, selections}` (map)

### 8.5 9-grid candidates + shot splitting

**Modify** `media.ex`
- `generate_candidates/3` — opts `:shot_id`, `:count` (default 9), `:model`,
  `:size`; builds a "3×3 storyboard grid of candidate compositions" prompt,
  saves under `assets/images/`, records `kind="candidates"` with
  `meta.grid = %{cols: 3, rows: 3}`.
- `crop_grid/4` (new pure arg builder + ffmpeg runner) — `(path, cell, cols,
  rows)` → `crop=w/cols:h/rows:x:y` via the existing `Exile`/ffmpeg pattern used
  by `Assemble`; `cell` 0..8, or `"whole"`.

**Modify** `storyboard.ex`
- `split_shot/3` — LLM (`storyboard.split.*`) returns sub-shots with
  `size`/`motion`/`visual_focus` + start/end prompts; `replace_shot_with_subshots`
  reuses `parse_storyboard` + `insert_shot` within a transaction; `idx`
  resequenced.

**Tools**
- `toonflow_generate_shot_candidates` `{project, shot_id, count?, model?, size?}`
- `toonflow_select_candidate` `{project, shot_id, type, cell?, candidates_id?}`
  (`cell` omitted / `"whole"` → whole image; else ffmpeg crop → `keyframe_{type}`)
- `toonflow_split_shot` `{project, shot_id, instructions?}`

**Create** `priv/toonflow/prompts/storyboard.split.system.md`, `.user.md`.

**Tests** — `crop_grid/4` arg builder (pure), candidate/split parsing.

### 8.6 Prompt management + worldview

**Modify** `lib/exhub/toonflow/prompts.ex`
- `resolve/2` — per-project override (latest version) → shipped file.
- `set/4` (name, content, note), `list/1`, `history/2`, `revert/3` (insert a new
  version copying a prior one).
- Call sites (`events.ex`, `script.ex`, `storyboard.ex`, `assets.ex`) switch
  `Prompts.render/2` → `Prompts.resolve/2` (project-passed).

**Create** `lib/exhub/toonflow/worldview.ex` (`Exhub.Toonflow.Worldview`)
- Notes stored as `memory_notes` kind `worldview`; `set/3`, `get/2`,
  `digest/2` (pure: group map/region/location/music-style anchors).
- Always-injected digest appended to `script`, `assets`, `storyboard` (not
  semantic recall).

**Tools**
- `toonflow_prompt_list`, `toonflow_prompt_get`, `toonflow_prompt_set`,
  `toonflow_prompt_history`, `toonflow_prompt_revert`
- `toonflow_worldview_set`, `toonflow_worldview_get`

### 8.7 Plan pre-review

**Modify** `pipeline.ex`
- `plan/3` enriched: per-stage status (existing) **plus** per-shot readiness →
  `%{"stages" => [...], "shots" => [%{"idx", "ready", "link", "missing"}],
  "next" => ...}`; `link` from `shots.meta["link"]` (default `"keyframe"`).

**Tools**
- `toonflow_pipeline_plan` `{project, stages?, from?}` → the plan payload.

**Tests** — `pipeline_test.exs` plan shape.

---

## 6. MCP tool surface (26 → 45)

| # | Tool | Phase |
|---|------|-------|
| 1-26 | *(existing — unchanged)* | — |
| 27 | `toonflow_generate_keyframe` | 7.1 |
| 28 | `toonflow_copy_keyframe` | 7.1 |
| 29 | `toonflow_list_scenes` | 7.2 |
| 30 | `toonflow_list_props` | 7.2 |
| 31 | `toonflow_generate_reference` | 7.2 |
| 32 | `toonflow_generate_variation` | 8.4 |
| 33 | `toonflow_list_variations` | 8.4 |
| 34 | `toonflow_set_shot_variations` | 8.4 |
| 35 | `toonflow_generate_shot_candidates` | 8.5 |
| 36 | `toonflow_select_candidate` | 8.5 |
| 37 | `toonflow_split_shot` | 8.5 |
| 38 | `toonflow_prompt_list` | 8.6 |
| 39 | `toonflow_prompt_get` | 8.6 |
| 40 | `toonflow_prompt_set` | 8.6 |
| 41 | `toonflow_prompt_history` | 8.6 |
| 42 | `toonflow_prompt_revert` | 8.6 |
| 43 | `toonflow_worldview_set` | 8.6 |
| 44 | `toonflow_worldview_get` | 8.6 |
| 45 | `toonflow_pipeline_plan` | 8.7 |

Register all in `Exhub.MCP.ToonflowServer` (`component(...)` per tool).
Add error messages to `Helpers.message/1` as needed.

---

## 7. Config changes

`config/config.exs` + `Exhub.Toonflow.Config` defaults:

```elixir
config :exhub, :toonflow, %{
  # ... existing ...
  "media" => %{
    "image_model" => "qwen-image-2.0",
    "video_model" => "MiniMax-H3",
    "tts_model" => "CosyVoice2",
    "tts_voice" => "alloy",
    "keyframes" => true,        # pipeline may generate start/end frames
    "last_frame" => true,       # fl2va uses the end frame when present
    "candidates" => 9           # 9-grid candidate count default
  },
  "prompts" => %{"overrides" => true}
}
```

No UI/supervision changes.

---

## 8. File manifest

### Create
```
lib/exhub/toonflow/keyframe.ex
lib/exhub/toonflow/worldview.ex
lib/exhub/mcp/tools/toonflow/generate_keyframe.ex
lib/exhub/mcp/tools/toonflow/copy_keyframe.ex
lib/exhub/mcp/tools/toonflow/list_scenes.ex
lib/exhub/mcp/tools/toonflow/list_props.ex
lib/exhub/mcp/tools/toonflow/generate_reference.ex
lib/exhub/mcp/tools/toonflow/generate_variation.ex
lib/exhub/mcp/tools/toonflow/list_variations.ex
lib/exhub/mcp/tools/toonflow/set_shot_variations.ex
lib/exhub/mcp/tools/toonflow/generate_shot_candidates.ex
lib/exhub/mcp/tools/toonflow/select_candidate.ex
lib/exhub/mcp/tools/toonflow/split_shot.ex
lib/exhub/mcp/tools/toonflow/prompt_list.ex
lib/exhub/mcp/tools/toonflow/prompt_get.ex
lib/exhub/mcp/tools/toonflow/prompt_set.ex
lib/exhub/mcp/tools/toonflow/prompt_history.ex
lib/exhub/mcp/tools/toonflow/prompt_revert.ex
lib/exhub/mcp/tools/toonflow/worldview_set.ex
lib/exhub/mcp/tools/toonflow/worldview_get.ex
lib/exhub/mcp/tools/toonflow/pipeline_plan.ex
priv/toonflow/prompts/storyboard.split.system.md
priv/toonflow/prompts/storyboard.split.user.md
test/exhub/toonflow/keyframe_test.exs
test/exhub/toonflow/worldview_test.exs
```

### Modify
```
lib/exhub/toonflow/schema.ex          # 4 tables + columns + decoders
lib/exhub/toonflow/media.ex           # :kind, :variation, refs_for_shot, candidates, crop_grid
lib/exhub/toonflow/video.ex           # last_frame plumbing
lib/exhub/toonflow/assets.ex          # scenes/props persistence, variations, references
lib/exhub/toonflow/storyboard.ex      # keyframe prompts, scene prompt, split_shot
lib/exhub/toonflow/pipeline.ex        # keyframes stage, plan enrichment
lib/exhub/toonflow/prompts.ex         # resolve/set/list/history/revert
lib/exhub/toonflow/script.ex          # resolve/2, worldview digest
lib/exhub/toonflow/events.ex          # resolve/2
lib/exhub/mcp/toonflow_server.ex      # register new components
lib/exhub/mcp/tools/toonflow/helpers.ex  # new error messages
config/config.exs                     # media/candidates/prompts keys
priv/toonflow/prompts/storyboard.user.md  # keyframe prompts
test/exhub/toonflow/{media,assets,storyboard,pipeline,prompts}_test.exs
docs/modules/toonflow.md, docs/toonflow-guide.md  # docs
```

---

## 9. Testing plan

- Pure logic unit tests (no app boot, no network):
  `continuity_copy/2`, keyframe prompt resolution, `next_shot/previous_shot`,
  `refs_for_shot/3`, variation prompt wrapper, `crop_grid/4` args,
  `parse_storyboard` (keyframe prompts, sub-shots), `Prompts.resolve/2`,
  `Worldview.digest/2`, `Pipeline.plan/3` shape, new decoders.
- Media/LLM/video clients stay behaviour-injected; stubbed in tests.
- Run: `MIX_ENV=test mix test --no-start test/exhub/toonflow/`.
- `mix compile --force --warnings-as-errors`; `mix format` on touched files only.
- Live smoke (optional, after deploy): a throwaway project through
  `extract_assets → generate_reference → generate_keyframe → generate_video`.

---

## 10. Zero-downtime activation

New tables auto-create on the next `DB.open/2` (no migration). No new
supervision children. Per slice:

```sh
MIX_ENV=prod mix release --overwrite
_build/prod/rel/exhub/bin/exhub rpc "Exhub.HotReload.reload_and_summarize()"
curl -s localhost:9069/toonflow/mcp -H 'content-type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'   # expect 45 tools
```

⚠️ Never restart/stop the VM (proxied LLM traffic — see `AGENTS.md`).

---

## 11. Risks & mitigations

| Risk | Mitigation |
|------|------------|
| `fl2va` last-frame quality / provider limits | `last_frame` optional; fall back to `fl2va` first-only, then `t2va` |
| ffmpeg crop portability | configurable `ffmpeg_path`; pure `crop_grid` args unit-tested |
| Existing DBs miss new tables | `CREATE TABLE IF NOT EXISTS` re-applied on every `DB.open` |
| Scene/prop upsert name collisions | upsert by name (idempotent), mirror kept for inspection |
| Prompt overrides shadow shipped fixes | version history + `revert`; overrides opt-out via config |
| Cost/latency of candidates + variations | per-tool (not default pipeline); `count` configurable |
| Scope creep | UI explicitly deferred; no library/episodes/timeline this pass |

---

## 12. Sequencing & milestones

| Milestone | Contents | Deliverable |
|-----------|----------|-------------|
| **M1** | 7.1 keyframes | start/end frames, continuity, `fl2va` first+last |
| **M2** | 7.2 + 7.3 | scenes/props persisted + refs; context-aware generation |
| **M3** | 8.4 | wardrobe variations + per-shot selection |
| **M4** | 8.6 | prompt overrides + worldview |
| **M5** | 8.5 | 9-grid candidates + crop; shot splitting |
| **M6** | 8.7 + docs | plan pre-review; docs updated |

Each milestone is an independently compiling, warning-clean, unit-tested,
hot-deployable slice.

---

## 13. Open items

1. `toonflow_prompt_get` vs `prompt_list` only — keep both? (proposed: yes)
2. Candidate grid default `3×3` only, or allow `cols×rows`? (proposed: 3×3)
3. Variation reference asset kind: record `variation_ref` rows, or store paths
   only in `refs_json`? (proposed: paths only; kind optional)
4. Should the `keyframes` pipeline stage be on by default (`media.keyframes`
   true) or opt-in? (proposed: on, `resume`-skippable)