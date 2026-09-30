# Toonflow on ExHub — Implementation Plan

**Date**: 2026-09-30
**Status**: Phases 0-5 complete; Phase 6 (polish) remaining
**Design**: [docs/plans/2026-09-30-toonflow-design.md](2026-09-30-toonflow-design.md)
**Scope**: MCP-tool-driven pipeline core; no custom UI in v1

---

## 1. Approach

Build `Exhub.Toonflow.*` incrementally inside the existing `exhub` app, reusing
`sagents`, the Anubis MCP server pattern, the Brain RAG vector stack, the Gitee
AI media tools, and `Exile`/FFmpeg. Each phase ends with a compiling,
warning-clean, unit-tested slice that can be hot-deployed to the running release.

**Guardrails**

- `mix compile --force --warnings-as-errors` after every phase.
- `mix test --no-start test/exhub/toonflow/` for pure logic; no live API calls in
  unit tests (inject media/LLM behind behaviours, stub in tests).
- `mix format` only the files touched.
- **Never restart the running VM.** Hot-reload code; attach new supervision
  children via the `AGENTS.md` RPC recipe.
- Do not stage/commit without asking.

## 2. Phase Overview

| Phase | Theme | Key deliverable | Exit criteria |
|-------|-------|-----------------|---------------|
| 0 | Scaffolding | Config, Workspace, Schema, Store, empty MCP server, docs/module ref | `/toonflow/mcp` lists tools; project CRUD works |
| 1 | Novel & script | `add_novel`, chapter split, `extract_events`, `generate_script`, memory v0 | End-to-end: novel file → script in DB |
| 2 | Assets & storyboard | character/scene extraction, director agent, `generate_storyboard`, images | Script → shots → frame images |
| 3 | Video, voice, export ✅ | `generate_video`, `generate_voice`, `assemble`, `export` (FFmpeg) | Shots → mp4 episode + srt |
| 4 | Memory & pipeline ✅ | sqlite-vec semantic recall, `Pipeline` orchestration, agent chat | `toonflow_chat` drives a full run |
| 5 | UI ✅ | Canvas web view + websocket progress | Canvas + live progress serve at `/toonflow` |
| 6 | Polish | Skills/tool-cards, provider notes, docs, activation recipe | Docs complete, tests green |

## 3. Phase Detail

### Phase 0 — Scaffolding & design freeze

**Files to create**

| File | Purpose |
|------|---------|
| `lib/exhub/toonflow/config.ex` | `:exhub, :toonflow` config resolution + defaults |
| `lib/exhub/toonflow/workspace.ex` | Project dir layout + path helpers (pure) |
| `lib/exhub/toonflow/schema.ex` | DDL + row encode/decode (pure) |
| `lib/exhub/toonflow/store.ex` | GenServer over `exqlite` (projects/tables CRUD) |
| `lib/exhub/mcp/toonflow_server.ex` | Anubis server, `capabilities: [:tools]` |
| `lib/exhub/mcp/tools/toonflow/{list_projects,create_project,project_info}.ex` | v0 tools |
| `test/exhub/toonflow/{workspace_test,schema_test,store_test}.exs` | Unit tests |
| `docs/modules/toonflow.md` | User-facing module reference |

**Files to modify**

- `lib/exhub/application.ex` — add `{Exhub.Toonflow.Store, name: …}` and the
  `ToonflowServer` child.
- `lib/exhub/mcp/hub/built_in_registry.ex` — `"toonflow" => Exhub.MCP.ToonflowServer`.
- `lib/exhub/router.ex` — `forward("/toonflow/mcp", to: Exhub.MCP.LazyPlug, …)`.
- `config/config.exs` — default `:toonflow` config block.

**Verify**

```sh
mix compile --force --warnings-as-errors
mix test --no-start test/exhub/toonflow/
# runtime (after hot deploy):
curl -s localhost:9069/toonflow/mcp -H 'content-type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
```

### Phase 1 — Novel & script

**Create**

- `lib/exhub/toonflow/novel.ex` — ingest (path/text/docx via `DocExtract`),
  chapter splitting (pure + heuristics), chunking.
- `lib/exhub/toonflow/events.ex` — LLM chapter event-graph extraction (LangChain
  via `Exhub.Genclaw.LLMHelper`-style helper) + JSON parsing/validation (pure).
- `lib/exhub/toonflow/script.ex` — ScriptAgent prompt → structured script; version
  storage.
- `lib/exhub/toonflow/prompts.ex` + `priv/toonflow/prompts/{events,script}.md`.
- Tools: `toonflow_add_novel`, `toonflow_list_chapters`, `toonflow_extract_events`,
  `toonflow_list_events`, `toonflow_generate_script`, `toonflow_get_script`,
  `toonflow_update_script`.
- `lib/exhub/toonflow/memory.ex` (v0 — schema + note CRUD, index deferred to P4).

**Reuse**: `Exhub.MCP.DocExtract` (novel files), `LlmConfigServer`/LangChain.

**Exit**: given a `.txt` novel, produce chapters, an event graph, and a script
version — all persisted and queryable via MCP tools.

### Phase 2 — Assets & storyboard

**Create**

- `lib/exhub/toonflow/assets.ex` — character/scene/prop extraction → appearance DB.
- `lib/exhub/toonflow/storyboard.ex` — DirectorAgent: script → shots
  (景别/构图/光线/运镜); prompt assembly per shot.
- `lib/exhub/toonflow/media.ex` — wrappers over `image_gen` / `i2i` / `look`.
- `lib/exhub/toonflow/jobs.ex` — async job lifecycle (DynamicSupervisor + Task).
- `lib/exhub/toonflow/factory.ex` — register `toonflow_director`,
  `toonflow_consistency` agents (sagents) + `priv/toonflow/tool_cards/*.yaml`.
- Tools: `toonflow_extract_assets`, `toonflow_list_characters`,
  `toonflow_generate_storyboard`, `toonflow_list_shots`, `toonflow_generate_image`.

**Reuse**: `ImageGen`, `i2i`, `Look`, `ImageSource`, `sagents`.

**Exit**: a script yields a shot list and one generated frame per shot, with
character references applied via `i2i`.

### Phase 3 — Video, voice, export — ✅ complete

Implemented: `Exhub.Toonflow.{Video, Voice, Assemble, Jobs, HTTP}` with the
`Video.Client` / `Voice.Client` behaviours (MoArk async submit+poll reusing the
`Exhub.MCP.Tools.VideoGen` / `Exhub.MCP.Tools.Speak` pure helpers), FFmpeg /
FFprobe episode assembly with SRT generation, and the `toonflow_generate_video`,
`toonflow_generate_voice`, `toonflow_assemble` and `toonflow_export` tools
(19 tools total). Assembly is synchronous; asynchronous job submission is
deferred to the Phase 4 pipeline.

**Create**

- `lib/exhub/toonflow/assemble.ex` — FFmpeg concat/mux/subtitle builders (pure) +
  execution via `Exile`.
- Tools: `toonflow_generate_video` (wraps `VideoGen`, async submit+poll via `Jobs`),
  `toonflow_generate_voice` (wraps `Speak`), `toonflow_assemble`,
  `toonflow_export`.

**Reuse**: `VideoGen`, `Speak`, `Exile`, system FFmpeg.

**Exit**: shots → clips → per-scene assembly → final `output/<episode>.mp4` +
`.srt`.

### Phase 4 — Memory & pipeline — ✅ complete

Implemented:

- `lib/exhub/toonflow/memory/index.ex` — `Exhub.Toonflow.Memory.Index`, a
  GenServer over a **global** `<root>/toonflow_index.db` (`sqlite-vec` `vec0`,
  signature-based incremental rebuild) holding every project's note chunks;
  each chunk carries its `project`, so `search/2` scopes to one project over a
  shared connection. Reuses `Exhub.MCP.Brain.RAG.{Embedder, Chunker}`; the
  embedder (`:toonflow_embedder`) and server name (`:toonflow_index_server`)
  are injectable. Short notes that the chunker would drop fall back to a single
  whole-note chunk. Dimension changes drop + rebuild.
- `lib/exhub/toonflow/memory.ex` (extended) — `export_notes/3` (markdown to
  `<project>/memory/notes/<id>.md`), `index/3` (export + rebuild), `search/3`
  and `recall/3` (best-effort prompt fragment; never fatal).
- `lib/exhub/toonflow/pipeline.ex` — `stages/0`, `run/3`, `resume/3` (skip
  existing outputs/assets), `plan/3`; runs under a `Jobs` row, threads the
  script id through the stages, and returns a per-stage report (stop-on-error by
  default, `continue_on_error` to press on).
- `:recall` opts on `Script.generate_script/3` and
  `Storyboard.generate_storyboard/3` (append recalled memory to instructions).
- 7 new tools (19 → 26): `toonflow_pipeline_run`, `toonflow_memory_add`,
  `toonflow_memory_list`, `toonflow_memory_search`, `toonflow_memory_index`,
  `toonflow_chat`, `toonflow_list_agents`.
- `Exhub.Toonflow.Memory.Index` added to the supervision tree; a `"toonflow"`
  director agent (holding the `toonflow` tool set) added to
  `Exhub.Sagents.Factory.agents/0`.

**Exit**: `toonflow_pipeline_run` (or `toonflow_chat`) drives
novel→script→storyboard→media→assemble in one call, resumable and with semantic
recall over prior notes.

### Phase 5 — Canvas UI & live progress — ✅ complete

Read-mostly canvas + websocket progress; mutating endpoints are loopback-only.

| File | Purpose |
|------|---------|
| `lib/exhub/toonflow/snapshot.ex` | `Exhub.Toonflow.Snapshot`: `build/1` view-model (`project`, `plan`, `shots`, `outputs`, `counts`, `jobs`) + `project_list/0`; `asset_url/3`/`media_url/2`; `resolve_media/3` serves **only** `assets/`, `output/`, `storyboards/` and rejects traversal, symlinks and non-regular files |
| `lib/exhub/toonflow/progress.ex` | `Exhub.Toonflow.Progress`: best-effort fan-out over a **duplicate** `Registry` (`Exhub.Toonflow.Progress.Registry`); `stage_event/4`, `shot_event/5`, `job_event/4`; normalises non-JSON terms / non-atom keys |
| `lib/exhub/toonflow/socket_handler.ex` | `Exhub.Toonflow.SocketHandler` (`:cowboy_websocket`) for `WS /toonflow/ws`; frames `snapshot`/`stage`/`shot`/`job`/`jobs`/`heartbeat`/`pong`/`error`; actions `subscribe`/`unsubscribe`/`refresh`/`ping`; `ui.tick_ms` coarse refresh |
| `lib/exhub/router/toonflow_view.ex` | `Exhub.Router.ToonflowView`: server-rendered HTML (inline CSS/JS, dark theme) for the index and the canvas (stage rail, shot grid, inspector, live log, run controls) |

**Routes** (`lib/exhub/router.ex`): `get /toonflow`,
`get /toonflow/projects/:name`, `get|post /toonflow/api/projects`,
`get /toonflow/api/projects/:name`, `post /toonflow/api/projects/:name/run`,
`get /toonflow/media/:name/*path`, and `socket "/toonflow/ws"`. Mutating `POST`s
are loopback-only (`ui.require_local` → `403` otherwise).

**Wiring**: `Progress` calls threaded through `Pipeline` (`stage_event` per
stage, `shot_event` per shot); the duplicate `Registry` added to
`lib/exhub/application.ex`; `ui` config block in `config/config.exs` +
`Exhub.Toonflow.Config`.

**Tests**: `test/exhub/toonflow/{progress_test,snapshot_test,toonflow_view_test}.exs`.

**Verify (live)**: `GET /toonflow` 200; create a project via
`POST /toonflow/api/projects`; `GET /toonflow/api/projects/:name` returns the
snapshot with all 9 stages; media guards 404; and the raw-socket probe
(`/tmp/tf_ws_probe.py`) → `101 Switching Protocols`, a `snapshot` frame, `pong`,
then live `stage` events during a `{"stages":["novel"]}` run.

**Gotchas learned**

- **Cowboy frames**: `cowlib`'s `cow_ws:frame/2` has no clause for a bare
  binary, so replies must be frames (`{:text, iodata}`). Returning `[json]`
  closes the socket with `{:function_clause, [{:cow_ws, :frame, …}]}`.
- **Dispatch is boot-frozen**: a hot-reloaded `socket/3` route is only served
  after the VM (or the `Exhub.Router.HTTP` listener child) restarts — do **not**
  reinstall it with `:cowboy.set_env/3` on a live listener (that wedged the HTTP
  listener in testing). Ordinary `get`/`post` routes hot-reload fine.

### Phase 6 — Polish (remaining)

- ✅ `docs/modules/toonflow.md` — Phase 5 (UI + websocket) section added.
- ✅ `docs/plans/2026-09-30-toonflow-implementation.md` — Phase 5 marked complete.
- README docs table row.
- Tool cards / skills in `priv/toonflow/`.
- Provider notes (routing image/video/tts through ExHub proxy).
- `docs/plans/2026-09-30-toonflow-activation.md` — exact zero-downtime steps.
- Remove the stray `lib/llm/erl_crash.dump` artifact.

## 4. File Manifest (create)

```
lib/exhub/toonflow/config.ex
lib/exhub/toonflow/workspace.ex
lib/exhub/toonflow/schema.ex
lib/exhub/toonflow/store.ex
lib/exhub/toonflow/novel.ex
lib/exhub/toonflow/events.ex
lib/exhub/toonflow/script.ex
lib/exhub/toonflow/assets.ex
lib/exhub/toonflow/storyboard.ex
lib/exhub/toonflow/media.ex
lib/exhub/toonflow/jobs.ex
lib/exhub/toonflow/assemble.ex
lib/exhub/toonflow/memory.ex
lib/exhub/toonflow/memory/index.ex
lib/exhub/toonflow/pipeline.ex
lib/exhub/toonflow/factory.ex
lib/exhub/toonflow/prompts.ex
lib/exhub/mcp/toonflow_server.ex
lib/exhub/mcp/tools/toonflow/*.ex
priv/toonflow/prompts/*.md
priv/toonflow/agents/*.md
priv/toonflow/tool_cards/*.yaml
test/exhub/toonflow/*_test.exs
docs/modules/toonflow.md
```

## 5. File Manifest (modify)

```
lib/exhub/application.ex              # children
lib/exhub/router.ex                   # forward "/toonflow/mcp"
lib/exhub/mcp/hub/built_in_registry.ex# register "toonflow"
config/config.exs                     # :toonflow defaults
README.md                             # docs table row (Phase 6)
```

## 6. Zero-Downtime Activation (per phase)

```sh
# 1) build fresh beams into the running release
MIX_ENV=prod mix release --overwrite
# 2) hot-reload :exhub modules
_build/prod/rel/exhub/bin/exhub rpc "Exhub.HotReload.reload_and_summarize()"
# 3) attach NEW supervision children once (first phase that adds them)
_build/prod/rel/exhub/bin/exhub rpc 'Supervisor.start_child(Exhub.Supervisor, Exhub.Toonflow.Store)'
_build/prod/rel/exhub/bin/exhub rpc 'Supervisor.start_child(Exhub.Supervisor, Exhub.MCP.ToonflowServer)'
# Phase 4 — memory index (needs the sqlite-vec extension available):
_build/prod/rel/exhub/bin/exhub rpc 'Supervisor.start_child(Exhub.Supervisor, Exhub.Toonflow.Memory.Index)'
# Phase 5 — progress registry (duplicate keys; browser sockets subscribe to it):
_build/prod/rel/exhub/bin/exhub rpc 'Supervisor.start_child(Exhub.Supervisor, {Registry, keys: :duplicate, name: Exhub.Toonflow.Progress.Registry})'
# 4) verify
curl -s localhost:9069/toonflow/mcp -H 'content-type: application/json' \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
curl -s -o /dev/null -w '%{http_code}\n' localhost:9069/toonflow
```

> ⚠️ Do **not** restart/stop the VM — it carries all proxied LLM traffic,
> including this session's. See `AGENTS.md`.
>
> ⚠️ The `WS /toonflow/ws` socket route and the `ui` config are **not** picked up
> by a hot reload: the Cowboy dispatch is compiled when the listener starts, and
> `sys.config` is only read at VM boot. They take effect on the next real restart
> (or by bouncing the `Exhub.Router.HTTP` listener child). Plain `get`/`post`
> routes and `Exhub.Toonflow.SocketHandler` *code* do hot-reload.
>
> ⚠️ Never reinstall the dispatch on a live listener with `:cowboy.set_env/3` —
> it wedged the HTTP listener (all MCP traffic) in testing.

## 7. Milestones & Sequencing

- **M1 (Phases 0–1):** novel → script core, MCP-exposed. *First demoable slice.*
- **M2 (Phase 2):** script → storyboard → frames.
- **M3 (Phase 3):** shots → mp4 + subtitles.
- **M4 (Phase 4):** one-call pipeline via agent chat.
- **M5 (Phase 6):** docs/polish; Phase 5 optional.

## 8. Open Items (resolve during build)

1. Exact default `root_dir` naming (`~/.config/exhub/toonflow` vs a visible
   `~/ExhubToonflow`) — currently proposed `~/.config/exhub/toonflow`.
2. Whether `Toonflow.Memory` shares a process with `Store` or is separate.
3. Embedding provider for project memory (shared Brain RAG config vs. dedicated).
4. TTS voice selection per character (map in appearance DB).
5. Subtitle timing source (TTS duration vs. estimated per-line).
6. Whether to add a `toonflow_pipeline_run` tool (Phase 4) or keep `toonflow_chat`
   as the only entry point.