# Toonflow on ExHub — Design

**Date**: 2026-09-30
**Status**: Approved (design), implementation pending
**Scope**: MCP-tool-driven pipeline core (no custom UI in v1)
**Namespace**: `Exhub.Toonflow.*` inside the existing `exhub` OTP app

---

## 1. Overview

[Toonflow](https://github.com/HBAI-Ltd/Toonflow-app) (MIT, ~11.3k★) is an
open-source **AI short-drama (短剧) / anime-drama (漫剧) creation platform**. Its
pipeline is *小说 → 剧本 → 分镜 → 图像 → 视频* (novel → script → storyboard →
image → video), organized on an "infinite canvas" with a 3-tier agent system.

An earlier investigation (this repo, 2026-09-30) concluded that ExHub already
provides ~70% of Toonflow's primitives, and that the right move is **not** to
port the Node/TypeScript app but to **re-implement Toonflow as a first-class
`Exhub.Toonflow.*` subsystem inside the `exhub` OTP app** — the same way
`Exhub.GenClaw` and `Exhub.Hercules` were built on top of ExHub's sagents +
Anubis MCP + LangChain + media tools.

This document is the design reference. The phased build plan lives in
[docs/plans/2026-09-30-toonflow-implementation.md](2026-09-30-toonflow-implementation.md).
The user-facing module reference (`docs/modules/toonflow.md`) is written when
Phase 1 lands.

## 2. Goals & Non-Goals

### Goals

1. **G1 — Pipeline core.** Expose the full novel→script→storyboard→image→video→
   export pipeline as a coherent set of MCP tools (`toonflow_*`).
2. **G2 — MCP-native.** All functionality is reachable from any MCP client
   (AiderDesk, Claude Code, Emacs) through ExHub's MCP hub; no bespoke UI.
3. **G3 — Reuse, don't rebuild.** Agents ride on `sagents`; media on
   `ImageGen` / `i2i` / `VideoGen` / `Speak` / `Look`; memory on the Brain RAG
   stack; LLM on `LlmConfigServer`; assembly on FFmpeg via Exile.
4. **G4 — Local workspaces.** Projects are plain directories under a configurable
   root (`workspaces/<project>/`), mirroring Toonflow's workspace model, so
   assets are inspectable and portable.
5. **G5 — Zero-downtime.** New code hot-reloads; new supervision children are
   attached without restarting the running VM (per `AGENTS.md`).
6. **G6 — Testable.** Core logic is pure and unit-testable with
   `mix test --no-start`.

### Non-Goals (v1)

- The infinite-canvas web UI (deferred — drive from existing MCP clients).
- Electron/web/desktop packaging.
- The plugin market, Skill Hub, Provider Hub storefronts.
- A2A / multi-node agent collaboration.
- Multi-user auth and tenancy.
- Local ONNX inference (ExHub uses the Brain RAG `/embeddings` stack instead).
- Full parity with every Toonflow node type (3D director stage, timeline, etc.).

## 3. Key Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Placement | `Exhub.Toonflow.*` inside the `exhub` app | Hot-reload, release pipeline, SecretVault, MCP hub, `Exile`, `exqlite` all shared; precedent: `Exhub.GenClaw`, `Exhub.Hercules` |
| v1 surface | MCP tools only (no UI) | Cheapest path to value; drivable from AiderDesk/Emacs today |
| Project storage | Local dir `workspaces/<project>/` | Inspectable assets, portable/backup-friendly, matches Toonflow |
| Metadata store | SQLite via `exqlite` (+ `sqlite_vec`) | Already a dependency; used by Brain RAG; no Ecto needed |
| Vector memory | Reuse Brain RAG (`VectorIndex` + `Embedder` + `Chunker`) | Avoids a second vector stack; OpenAI-compatible embeddings |
| Agents | `sagents` via `Exhub.Toonflow.Factory` | Same framework as Agent Hub / GenClaw; middleware, tool-cards, prompts |
| LLM | `Exhub.Llm.LlmConfigServer` + internal proxy | Central keys (SecretVault), token pooling, usage metrics |
| Image | Gitee AI `image_gen` + `i2i` | Already configured (`giteeai_api_key`) |
| Video | Gitee AI / moark `VideoGen` (MiniMax-H3) | Already configured; async submit+poll |
| Voice | `Speak` (Qwen3-TTS) | Already configured |
| Vision/consistency | `Look` | Already configured |
| Assembly | FFmpeg via `Exile` | FFmpeg confirmed available locally; Exile already used |
| Provider system | ExHub proxy `/openai/v1` | "Programmable providers" become ExHub model routing |

## 4. Reuse Map

| Toonflow capability | ExHub asset reused | Location |
|---------------------|--------------------|----------|
| 3-tier agents (script/production/consistency/QA) | `sagents` + `Exhub.Sagents.Factory`/`Hub` | `lib/exhub/sagents/*` |
| Domain agent with middleware + tool cards | **`Exhub.GenClaw`** (template) | `lib/exhub/genclaw/*` |
| Persistent semantic memory | Brain RAG (`VectorIndex`, `Embedder`, `Chunker`, rankers) | `lib/exhub/mcp/brain/rag/*` |
| Image generation / storyboard frames | `Exhub.MCP.Tools.ImageGen` (`image_gen`, `i2i`) | `lib/exhub/mcp/tools/image_gen.ex`, `i2i.ex` |
| Local file → image data URI | `Exhub.MCP.ImageSource` | `lib/exhub/mcp/image_source.ex` |
| Video generation | `Exhub.MCP.Tools.VideoGen` (MiniMax-H3 `t2va`/`fl2va`) | `lib/exhub/mcp/tools/video_gen.ex` |
| Voice / subtitles | `Exhub.MCP.Tools.Speak` | `lib/exhub/mcp/tools/speak.ex` |
| Vision consistency checks | `Exhub.MCP.Tools.Look` | `lib/exhub/mcp/tools/look.ex` |
| Document/novel text extraction | `Exhub.MCP.Tools.DocExtract` | `lib/exhub/mcp/tools/doc_extract.ex` |
| Programmable providers + secrets | LLM proxy (`/openai/v1`) + `LlmConfigServer` + SecretVault | `lib/exhub/proxy_plug.ex`, `lib/exhub/llm*` |
| MCP extensibility | ExHub **is** the MCP hub | `lib/exhub/mcp/hub/*` |
| Async long jobs | `DynamicSupervisor` + `Task` (pattern from `Desktop.ProcessStore`) | `lib/exhub/mcp/desktop/*` |
| Process execution (FFmpeg) | `Exile` | `mix.exs` |
| Hot reload / deploy | `Exhub.HotReload` + `AGENTS.md` recipe | `lib/exhub/hot_reload.ex` |

## 5. Architecture

```
        MCP clients (AiderDesk / Claude Code / Emacs / curl)
             │  POST /toonflow/mcp
             ▼
   ┌───────────────────────────────┐
   │  Exhub.MCP.ToonflowServer     │   use Anubis.Server, capabilities: [:tools]
   │  component(Exhub.MCP.Tools.   │
   │            Toonflow.*)        │
   └───────────────┬───────────────┘
                   │  in-process calls (BuiltInRegistry)
                   ▼
   ┌───────────────────────────────────────────────────────────┐
   │                    Exhub.Toonflow.*                       │
   │                                                           │
   │  Pipeline ──▶ {Script, Storyboard, Assets, Assemble}      │
   │  Store (SQLite)      Jobs (DynamicSupervisor + Task)      │
   │  Memory (sqlite-vec) Factory (sagents agent defs)         │
   │  Workspace (paths)   Prompts (priv/toonflow/*)            │
   └──────┬─────────────────────────┬──────────────────┬───────┘
          │                         │                  │
          ▼                         ▼                  ▼
   Exhub.Llm.*              MCP media tools      Exile / FFmpeg
   (LlmConfigServer,        (ImageGen, i2i,       (concat, mux,
    proxy /openai/v1)        VideoGen, Speak,     subtitles)
                             Look, DocExtract)
```

**Pattern**: `ToonflowServer` + `Tools.Toonflow.*` follow the existing
`LookServer`/`Tools.Look` and `VideoGenServer`/`Tools.VideoGen` structure
(Anubis server + tool components). Agents follow `GenClaw` (sagents + custom
middleware + YAML tool cards + `priv/` prompt templates).

## 6. Modules

| Module | Purpose |
|--------|---------|
| `Exhub.Toonflow.Config` | Resolve config (`:exhub, :toonflow`), defaults, root dir |
| `Exhub.Toonflow.Workspace` | Project dir layout, path helpers (pure) |
| `Exhub.Toonflow.Store` | GenServer over SQLite: CRUD for all tables |
| `Exhub.Toonflow.Schema` | DDL + row encoders/decoders (pure) |
| `Exhub.Toonflow.Novel` | Ingest novel, chaptering, chunking |
| `Exhub.Toonflow.Events` | Chapter event-graph extraction & query (LLM) |
| `Exhub.Toonflow.Script` | Script generation/update (LLM) |
| `Exhub.Toonflow.Assets` | Character/scene/prop extraction + appearance DB |
| `Exhub.Toonflow.Storyboard` | Shot planning (景别/构图/光线/运镜) |
| `Exhub.Toonflow.Media` | Thin wrappers over image/video/voice/look tools |
| `Exhub.Toonflow.Jobs` | Async job lifecycle + polling for async media |
| `Exhub.Toonflow.Assemble` | FFmpeg concat/mux/subtitles/voice-over (pure arg builders) |
| `Exhub.Toonflow.Memory` | sqlite-vec index over project notes/assets |
| `Exhub.Toonflow.Pipeline` | End-to-end orchestration across stages |
| `Exhub.Toonflow.Factory` | sagents agent definitions |
| `Exhub.Toonflow.Prompts` | Render `priv/toonflow/**` templates |
| `Exhub.MCP.ToonflowServer` | Anubis MCP server (`/toonflow/mcp`) |
| `Exhub.MCP.Tools.Toonflow.*` | One component per tool |

## 7. Data Model

### 7.1 Workspace layout

```
<root_dir>/workspaces/<project>/
  project.json          # portable project metadata mirror
  novels/               # source novels (uploaded txt/md/docx)
  chapters/             # per-chapter text + event graph JSON
  scripts/              # script versions (markdown/json)
  characters/           # appearance DB (json) + reference images
  storyboards/          # shot lists (json/markdown)
  assets/
    images/             # generated frames
    videos/             # generated clips
    audio/              # TTS voice tracks
  output/               # assembled episodes (mp4) + subtitles (srt)
  index.db              # per-project sqlite (metadata + vectors)
```

`root_dir` default: `<config :exhub, :toonflow, root_dir>` →
`Path.join(System.user_home!(), ".config/exhub/toonflow")`. Overridable.

### 7.2 SQLite tables (per project `index.db`)

| Table | Columns (core) |
|-------|----------------|
| `projects` | `id, name, root_dir, meta_json, created_at, updated_at` |
| `novels` | `id, project_id, title, source_path, text, meta_json, created_at` |
| `chapters` | `id, novel_id, idx, title, text, summary` |
| `events` | `id, project_id, chapter_id, idx, kind, summary, payload_json` |
| `scripts` | `id, project_id, chapter_id, version, format, content, meta_json, created_at` |
| `characters` | `id, project_id, name, appearance, refs_json, meta_json` |
| `shots` | `id, project_id, script_id, idx, scene, shot_desc, size, camera, lighting, motion, prompt, meta_json` |
| `assets` | `id, project_id, shot_id, character_id, kind, path, url, prompt, meta_json, created_at` |
| `jobs` | `id, project_id, type, status, params_json, result_json, error, created_at, updated_at` |
| `memory_notes` | `id, project_id, kind, title, text, meta_json, created_at` |
| `vec_memory` | virtual (`sqlite-vec`): `id, embedding float[N]` |

Chunk tracking mirrors `Exhub.MCP.Brain.RAG.VectorIndex` so re-indexing is
incremental (signature-based).

## 8. MCP Tool Surface (v1)

Server: `Exhub.MCP.ToonflowServer`, route `/toonflow/mcp`.

| Tool | Stage | Description |
|------|-------|-------------|
| `toonflow_list_projects` | project | List workspace projects |
| `toonflow_create_project` | project | Create `workspaces/<name>` + `index.db` |
| `toonflow_project_info` | project | Project stats (counts, paths) |
| `toonflow_add_novel` | novel | Ingest novel (path or text), chapter split |
| `toonflow_list_chapters` | novel | List/summarize chapters |
| `toonflow_extract_events` | events | LLM chapter event-graph extraction |
| `toonflow_list_events` | events | Query the event graph |
| `toonflow_generate_script` | script | ScriptAgent: novel/events → script |
| `toonflow_get_script` | script | Fetch a script version |
| `toonflow_update_script` | script | Apply an edit (new version) |
| `toonflow_extract_assets` | assets | Characters/scenes/props + appearance DB |
| `toonflow_list_characters` | assets | List characters + refs |
| `toonflow_generate_storyboard` | storyboard | DirectorAgent: script → shots |
| `toonflow_list_shots` | storyboard | List shots for a script |
| `toonflow_generate_image` | media | Frame gen (wraps `image_gen`/`i2i`) |
| `toonflow_generate_video` | media | Clip gen (wraps `video_gen`) |
| `toonflow_generate_voice` | media | Line TTS (wraps `speak`) |
| `toonflow_assemble` | export | Per-scene FFmpeg concat + mux |
| `toonflow_export` | export | Final episode (mp4) + srt |
| `toonflow_memory_search` | memory | Semantic recall over project |
| `toonflow_memory_add` | memory | Add a memory note (indexed) |
| `toonflow_chat` | agents | Drive a toonflow agent (script/director/qa/…) |
| `toonflow_list_agents` | agents | List registered toonflow agents |
| `toonflow_job_status` | jobs | Poll an async job |

## 9. Agents & Pipeline

Defined in `Exhub.Toonflow.Factory` (sagents), prompts in
`priv/toonflow/agents/*.md`, tool cards in `priv/toonflow/tool_cards/*.yaml`
(GenClaw style). Tool access is selective per agent (smaller context).

| Agent | Input | Output | Tools |
|-------|-------|--------|-------|
| `toonflow_script` | novel + events | structured script | project read, events, script write |
| `toonflow_director` | script | shot list (景别/构图/光线/运镜) | script read, storyboard write, `look` |
| `toonflow_consistency` | shots + character DB | consistency report | character read, `look` |
| `toonflow_qa` | script + shots | logic/continuity report | project read |
| `toonflow_production` | shot list | media nodes → assembled scene | image/video/voice, assemble |

Pipeline (`Exhub.Toonflow.Pipeline`) chains stages and records jobs; every stage
is also individually callable as a tool, so agents can drive it step-by-step.

## 10. Configuration

```elixir
config :exhub, :toonflow, %{
  "root_dir" => nil,                 # nil => ~/.config/exhub/toonflow
  "agents" => %{
    "script" => "kimi-k2.6",
    "director" => "kimi-k2.6",
    "qa" => "kimi-k2.6"
  },
  "media" => %{
    "image_model" => "qwen-image-2.0",
    "video_model" => "MiniMax-H3",
    "tts_voice" => nil
  },
  "memory" => %{
    "enabled" => true,
    "index_path" => nil,             # nil => <project>/index.db
    "embedding_model" => "text-embedding-3-small",
    "dim" => 1536
  },
  "assembly" => %{
    "ffmpeg_path" => "ffmpeg",
    "subtitles" => true
  }
}
```

Shared keys: `:giteeai_api_key` (image/video/tts/vision) and the LLM keys via
`LlmConfigServer` — the same ones already used elsewhere.

## 11. Supervision & Zero-Downtime

New children added to `Exhub.Application`:

```
{Exhub.Toonflow.Store, name: Exhub.Toonflow.Store}
{Exhub.Toonflow.Jobs, name: Exhub.Toonflow.Jobs}
Exhub.Toonflow.Memory                      # if index lives process-side
{Exhub.MCP.ToonflowServer, transport: :streamable_http,
 request_timeout: 600_000, session_idle_timeout: 86_400_000 * 365}
```

Plus `Exhub.MCP.Hub.BuiltInRegistry`: `"toonflow" => Exhub.MCP.ToonflowServer`;
and `Exhub.Router`: `forward("/toonflow/mcp", …)`.

Because a **new** child is not started in the already-running VM, activation
follows the `AGENTS.md` recipe:

```sh
bin/exhub rpc 'Supervisor.start_child(Exhub.Supervisor, Exhub.Toonflow.Store)'
bin/exhub rpc 'Supervisor.start_child(Exhub.Supervisor, Exhub.MCP.ToonflowServer)'
bin/exhub rpc 'IO.inspect(Process.whereis(Exhub.Toonflow.Store))'
```

Code-only changes need only `MIX_ENV=prod mix release --overwrite` +
`Exhub.HotReload.reload/0`. **Never restart the VM** (proxied LLM traffic).

## 12. Testing

- Pure modules (`Workspace`, `Schema`, `Assemble` arg builders, `Prompts`,
  event parsing/validation) get unit tests under `test/exhub/toonflow/`.
- Run with `mix test --no-start test/exhub/toonflow/` (app boot is unreliable
  locally — see `AGENTS.md`).
- Media/LLM calls are injected behind behaviour boundaries and stubbed in tests;
  no live API calls in unit tests.
- `mix compile --force --warnings-as-errors` must stay clean.

## 13. Risks & Mitigations

| Risk | Mitigation |
|------|------------|
| LLM/video cost & latency | Async `Jobs`; per-stage tools allow partial runs; small models for QA |
| Character consistency across shots | Dedicated appearance DB + `i2i` conditioning + `toonflow_consistency` |
| SQLite concurrency | Single serializing GenServer (`Store`) like `VectorIndex` |
| FFmpeg portability | Configurable `ffmpeg_path`; pure arg builders unit-tested |
| Scope creep (UI/plugins) | Explicit non-goals; UI deferred to a later phase |
| Long async video jobs | Submit `wait: false` + `task_id` polling via `Jobs` |
| Secrets in logs | Never print keys; use SecretVault; treat `*exhub*` buffer as sensitive |