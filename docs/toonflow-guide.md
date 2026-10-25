# Toonflow Pipeline — Usage Guide

`exhub-toonflow` is ExHub's **native** AI short-drama pipeline. You give it a
novel; it produces an assembled episode (**mp4 + srt**). Everything is driven by
MCP tools, so you can run it from any MCP client (AiderDesk, Claude Code, Emacs)
or from the built-in web canvas.

```
novel ─▶ events ─▶ script ─▶ assets ─▶ storyboard ─▶ keyframes ─▶ images ─▶ videos ─▶ voices ─▶ assemble
        (LLM)     (LLM)     (LLM)      (LLM)         (moark)      (moark)   (moark)   (moark)   (FFmpeg)
```

- **Authoritative reference**: [docs/modules/toonflow.md](modules/toonflow.md) — every tool & parameter.
- **Design**: [docs/plans/2026-09-30-toonflow-design.md](plans/2026-09-30-toonflow-design.md).
- **Build plan / status**: [docs/plans/2026-09-30-toonflow-implementation.md](plans/2026-09-30-toonflow-implementation.md) — **Phases 0-8 complete**.
- **BigBanana alignment** (keyframes, scenes/props, variations, candidates, prompt overrides, worldview, plan pre-review): [docs/plans/2026-10-25-toonflow-bigbanana-alignment.md](plans/2026-10-25-toonflow-bigbanana-alignment.md).

---

## 1. Prerequisites

| Need | Detail |
|------|--------|
| ExHub running | prod release on **`localhost:9069`**, or `mix run --no-halt` in dev |
| LLM keys | already used by ExHub (`Exhub.Llm.LlmConfigServer`) — script/director use `kimi-k2.6` by default |
| Media key | shared `:giteeai_api_key` (image / video / TTS via moark) |
| FFmpeg | `ffmpeg` + `ffprobe` on `PATH` (only needed for `assemble` / `export`) |
| Workspace root | defaults to `~/.config/exhub/toonflow` |

---

## 2. Two ways to drive it

| | MCP tools | Web canvas |
|--|-----------|-----------|
| Best for | scripting, agents, automation | watching a run, nudging it |
| Entry | `POST /toonflow/mcp` (or hub `toonflow__<tool>`) | `http://localhost:9069/toonflow` |
| Progress | tool return values | live websocket frames |

Both share the same project store and workspace — you can mix them freely.

### 2.1 Connect an MCP client

```
POST http://localhost:9069/toonflow/mcp          # the toonflow server itself
# or, through the unified hub (any server, one endpoint):
POST http://localhost:9069/mcp-hub/mcp           # tools are  toonflow__<tool>
```

Your MCP client performs the `initialize` handshake; a tool call is then:

```json
{"jsonrpc":"2.0","id":1,"method":"tools/call",
 "params":{"name":"toonflow_create_project","arguments":{"name":"my-drama"}}}
```

### 2.2 Open the canvas

```sh
curl -s -o /dev/null -w '%{http_code}\
' localhost:9069/toonflow      # 200
# then in a browser:  http://localhost:9069/toonflow
# and a project:      http://localhost:9069/toonflow/projects/my-drama
```

The canvas shows a **stage rail**, **shot grid**, **inspector**, and a **live log**
fed by websocket frames (`snapshot` / `stage` / `shot` / `jobs`).

> **Mutating routes are loopback-only** unless `ui.require_local` is `false`
> (the app is also reachable over the VPN). A non-loopback `POST` returns
> `403 forbidden (mutating endpoints are loopback-only)`. Reads are unrestricted.

---

## 3. Quickstart — your first episode

```sh
# 1. create the project (workspace dir + registry row)
toonflow_create_project   {name: "my-drama", description: "first short drama"}

# 2. ingest the novel and split it into chapters
toonflow_add_novel        {project: "my-drama", path: "~/novels/story.txt",
                           title: "风雨"}
toonflow_list_chapters    {project: "my-drama"}     # -> chapter ids (chp_…)

# 3. run the WHOLE pipeline in one call (resumable, idempotent)
toonflow_pipeline_run     {project: "my-drama", path: "~/novels/story.txt",
                           title: "风雨", recall: true, mix_audio: true}
```

`toonflow_pipeline_run` returns a per-stage report:

```json
{"stages":[{"stage":"novel","status":"ok"}, {"stage":"script","status":"ok"}, …],
 "completed":10, "errors":[], "skip_existing":false}
```

Then package the episode:

```sh
toonflow_export {project: "my-drama", filename: "ep01", mix_audio: true}
# -> output/ep01.mp4 (+ output/ep01.srt)
```

Prefer the canvas? Open `http://localhost:9069/toonflow`, create the project,
and use the **Run** controls (they hit `POST /toonflow/api/projects/:name/run`).

---

## 4. Step-by-step (when you want control)

Run one stage at a time — each stage is also a standalone tool, so an agent can
drive it incrementally.

### 4.1 Novel → chapters

```sh
toonflow_add_novel     {project:"my-drama", path:"~/novels/story.txt"}   # or text: "…"
toonflow_list_chapters {project:"my-drama", novel_id:"nov_…", limit: 5}
```
`.txt`/`.md` are read directly; `.pdf`/`.docx`/images go through Gitee AI OCR
(`doc_extract`). Chapters are detected from `第N章/回/节/篇/卷`, `Chapter N`, or
Markdown `#`..`####`; heading-less text becomes one `全文` chapter.

### 4.2 Events

```sh
toonflow_extract_events {project:"my-drama", chapter_id:"chp_…"}
toonflow_list_events    {project:"my-drama", chapter_id:"chp_…", kind:"conflict"}
```
Typed events (`plot`/`conflict`/`reveal`/`emotion`/`action`/`dialogue`/`setup`).
Re-running replaces that chapter's events; per-chapter failures land in `errors`.

### 4.3 Script

```sh
toonflow_generate_script {project:"my-drama", chapter_id:"chp_…",
                          instructions:"更紧张，强化反转"}
toonflow_get_script      {project:"my-drama", chapter_id:"chp_…"}   # latest version
toonflow_update_script   {project:"my-drama", script_id:"scr_…",
                          content:"…edited…", note:"tighten act 2"}
```
Every call stores a **new immutable version** (omit `chapter_id` to adapt the
whole novel).

### 4.4 Assets (cast / scenes / props)

```sh
toonflow_extract_assets     {project:"my-drama", script_id:"scr_…"}
toonflow_list_characters    {project:"my-drama"}
toonflow_list_scenes        {project:"my-drama"}
toonflow_list_props         {project:"my-drama"}
```
Characters, scenes and props are upserted by name (idempotent) into the
appearance DB, which grounds storyboard + image prompts for visual consistency.

Render a **reference image** for any of them so later frames are conditioned on
it (`i2i`):

```sh
toonflow_generate_reference {project:"my-drama", kind:"scene", name:"祖宅"}
toonflow_generate_reference {project:"my-drama", kind:"character", name:"林昭"}
```

**Wardrobe variations** — the same face, a different outfit — and pinning one to
a shot:

```sh
toonflow_generate_variation  {project:"my-drama", character:"林昭", name:"夜行衣",
                              visual_prompt:"墨色夜行衣"}
toonflow_list_variations     {project:"my-drama", character:"林昭"}
toonflow_set_shot_variations {project:"my-drama", shot_id:"sht_…",
                              selections:{林昭:"夜行衣"}}     # {} clears
```

### 4.5 Storyboard (shots)

```sh
toonflow_generate_storyboard {project:"my-drama", script_id:"scr_…",
                              instructions:"青蓝冷光，手持运镜"}
toonflow_list_shots          {project:"my-drama", script_id:"scr_…"}
```
Each shot carries scene, 景别 (`size`), lighting, 运镜 (`motion`), characters, a
text-to-image `prompt` **and** optional first/last-frame prompts
(`keyframe_start_prompt` / `keyframe_end_prompt`). Re-running replaces that
script's shots. One shot carrying two beats? Split it in place:

```sh
toonflow_split_shot {project:"my-drama", shot_id:"sht_…", instructions:"拆成三个递进的镜头"}
```

### 4.6 Media (per shot)

```sh
# start/end frames first: they drive fl2va interpolated clips
toonflow_generate_keyframe {project:"my-drama", shot_id:"sht_…", type:"start"}
toonflow_generate_keyframe {project:"my-drama", shot_id:"sht_…", type:"end"}
# (or copy the neighbour's frame so a cut doesn't jump)
toonflow_copy_keyframe     {project:"my-drama", shot_id:"sht_…", direction:"prev_end"}

toonflow_generate_image {project:"my-drama", shot_id:"sht_…", size:"1024x1024"}
toonflow_generate_video {project:"my-drama", shot_id:"sht_…", duration_seconds:6,
                         aspect_ratio:"16:9"}   # task defaults: fl2va with first+last frame
toonflow_generate_voice {project:"my-drama", shot_id:"sht_…"}   # uses meta.dialogue / meta.台词
```

Unsure what a composition should be? Ask for a **candidate grid** (9
alternatives in one image) and crop the one you like into the keyframe:

```sh
toonflow_generate_shot_candidates {project:"my-drama", shot_id:"sht_…", count:9}
toonflow_select_candidate         {project:"my-drama", shot_id:"sht_…", type:"start", cell:"4"}
```

Image gen appends character appearances, the scene description and any reference
images (`i2i`). `fl2va` uses the shot's `keyframe_start`/`keyframe_end` when
present, else its latest frame as the first frame.

### 4.7 Assemble / export

```sh
toonflow_assemble {project:"my-drama", script_id:"scr_…", mix_audio:true}
toonflow_export   {project:"my-drama", filename:"ep01", subtitles:true, mix_audio:true}
```
FFmpeg concatenates the clips in order, writes `output/<name>.srt` from each
shot's dialogue (timings from `ffprobe`), and soft-muxes it. `export` returns a
manifest (`video`, `subtitle`, `output_dir`, asset count).

---

## 5. Memory (directorial control)

Record your taste once; recall it into later generations.

```sh
toonflow_memory_add    {project:"my-drama", kind:"style", title:"基调",
                        text:"悬疑冷峻，青蓝冷光，手持运镜"}
toonflow_memory_index  {project:"my-drama"}                 # embed into sqlite-vec
toonflow_memory_search {project:"my-drama", query:"运镜风格", top_k:5}   # -> chunks + similarity
```

Pass `recall: true` to `toonflow_pipeline_run`, or `:recall` to
`generate_script`/`generate_storyboard`, to append recalled notes to the
instructions. Requires the embedding key configured under `:brain_rag`.

### 5.1 Worldview anchors (always injected)

Memory notes are recalled by *similarity*; **worldview** anchors are always
injected into the script, assets and storyboard instructions — use them for what
must never drift (map, region, period, art style, music style):

```sh
toonflow_worldview_set {project:"my-drama", text:"架空唐代，岭南水乡，水墨动画风格"}
toonflow_worldview_set {project:"my-drama", title:"美术风格", text:"青蓝冷调，写实厚涂"}
toonflow_worldview_get {project:"my-drama"}       # -> notes + the injected digest
```

### 5.2 Prompt overrides (per project, versioned)

Every generation stage renders a template from `priv/toonflow/prompts/*`. Override
one for this project — the override wins until a newer version (or a revert)
replaces it:

```sh
toonflow_prompt_list    {project:"my-drama"}
toonflow_prompt_get     {project:"my-drama", name:"storyboard.system"}   # shipped or override
toonflow_prompt_set     {project:"my-drama", name:"storyboard.system",
                         content:"…full template text (keep the {{placeholders}})…",
                         note:"more 特写"}
toonflow_prompt_history {project:"my-drama", name:"storyboard.system"}
toonflow_prompt_revert  {project:"my-drama", name:"storyboard.system", version:1}
```

Overrides are opt-out via `prompts.overrides => false`.

---

## 6. Agent chat

```sh
toonflow_chat       {message:"为 my-drama 生成全片并导出 ep01"}
toonflow_list_agents {}
```
The `toonflow` director agent (sagents) holds the whole `toonflow` tool set and
can drive projects itself.

---

## 7. Resuming & idempotency

- `toonflow_pipeline_run` defaults to **`resume: true`**: a stage whose output
  exists is `skipped`, and per-shot media stages skip shots that already have the
  matching asset. Re-runs are cheap.
- Run a subset with `stages: ["script","storyboard"]`, or everything from a point
  with `from: "storyboard"`.
- `continue_on_error: true` presses on past a failing stage; otherwise the run
  stops at the first failure and returns `success: false` + `stage` + `reason`
  (e.g. `missing_source` when a novel stage has no input).
- **Pre-review before spending**: `toonflow_pipeline_plan {project:"my-drama"}`
  returns each stage's status plus, per shot, its `link` (how the clip is made)
  and what is still `missing` — with no generation at all.
- The opt-in extras (`generate_shot_candidates`, `generate_variation`,
  `generate_reference`, `copy_keyframe`, `split_shot`) are **not** pipeline
  stages; they are director tools you call yourself, and `pipeline_plan` never
  counts them as required.

---

## 8. Tool cheat-sheet

| Stage | Tools |
|-------|-------|
| project | `list_projects`, `create_project`, `project_info` |
| novel | `add_novel`, `list_chapters` |
| events | `extract_events`, `list_events` |
| script | `generate_script`, `get_script`, `update_script` |
| assets | `extract_assets`, `list_characters`, `list_scenes`, `list_props`, `generate_reference` |
| variations | `generate_variation`, `list_variations`, `set_shot_variations` |
| storyboard | `generate_storyboard`, `list_shots`, `split_shot` |
| keyframes | `generate_keyframe`, `copy_keyframe`, `generate_shot_candidates`, `select_candidate` |
| media | `generate_image`, `generate_video`, `generate_voice` |
| export | `assemble`, `export` |
| memory | `memory_add`, `memory_list`, `memory_index`, `memory_search` |
| prompts | `prompt_list`, `prompt_get`, `prompt_set`, `prompt_history`, `prompt_revert`, `worldview_set`, `worldview_get` |
| orchestration | `pipeline_run`, `pipeline_plan`, `chat`, `list_agents` |

45 tools total. Full parameter tables: [docs/modules/toonflow.md](modules/toonflow.md).

---

## 9. Configuration & workspace

```elixir
config :exhub, :toonflow, %{
  "root_dir" => nil,                              # nil => ~/.config/exhub/toonflow
  "agents"   => %{"script" => "kimi-k2.6", "director" => "kimi-k2.6", "qa" => "kimi-k2.6"},
  "media"    => %{"image_model" => "qwen-image-2.0", "video_model" => "MiniMax-H3", "tts_model" => "Step-Audio-TTS-3B", "tts_voice" => "中文女",
                  "keyframes" => true, "last_frame" => true, "candidates" => 9},
  "prompts"  => %{"overrides" => true},
  "memory"   => %{"enabled" => true, "top_k" => 5, "scope" => "project", "dim" => 1024},
  "assembly" => %{"ffmpeg_path" => "ffmpeg", "ffprobe_path" => "ffprobe", "subtitles" => true},
  "ui"       => %{"enabled" => true, "tick_ms" => 5000, "require_local" => true}
}
```

```
~/.config/exhub/toonflow/
  toonflow.db                 # project registry (SQLite)
  toonflow_index.db           # memory index (SQLite + sqlite-vec), all projects
  workspaces/<project>/
    project.json              # portable metadata mirror
    index.db                  # project data (novels, chapters, events, …)
    novels/ chapters/ scripts/ characters/ storyboards/
    memory/notes/             # exported notes (the indexed sources)
    assets/{images,videos,audio}/
    output/                   # assembled mp4 + srt
```

---

## 10. Troubleshooting / gotchas

| Symptom | Cause / fix |
|---------|-------------|
| `missing_source` on the `novel` stage | no `path`/`text` supplied (or resume with nothing to do). Pass a novel source. |
| `403 forbidden (mutating endpoints are loopback-only)` | you POSTed to the canvas API from a non-loopback address. Set `ui.require_local => false`, or call from `localhost`. |
| media/storyboard tool 404 on `/toonflow/media/...` | only `assets/`, `output/`, `storyboards/` are served; traversal/symlinks are rejected by design. |
| WebSocket closes immediately after `101` | a socket handler replied with a bare binary instead of a Cowboy frame (`{:text, …}`). See the frame note in [docs/modules/toonflow.md](modules/toonflow.md). |
| New `socket/3` route returns HTTP 200 instead of `101` | Cowboy's dispatch table is compiled at **listener start** — a hot-reloaded socket route only serves after the VM (or the `Exhub.Router.HTTP` listener child) restarts. Never reinstall the dispatch with `:cowboy.set_env/3` on a live listener. |
| `assemble`/`export` fails | `ffmpeg`/`ffprobe` not on `PATH` (or a shot is missing its clip — every shot needs `generate_video` first). |
| `select_candidate` fails with `ffmpeg` error | cell cropping shells out to `assembly.ffmpeg_path`; ensure it is installed, or pass `cell:"whole"` to use the whole grid. |
| `no_candidates` on `select_candidate` | no candidate grid for that shot yet — run `toonflow_generate_shot_candidates` first. |
| a prompt override seems ignored | `prompts.overrides` is `false`, or the stage you expect does not use that template (check with `toonflow_prompt_get`). |
| memory search returns nothing | run `toonflow_memory_index` first, and ensure the `:brain_rag` embedding key is set. |

---

## 11. Where things live

| Concern | Module |
|---------|--------|
| orchestration | `Exhub.Toonflow.Pipeline` |
| MCP surface | `Exhub.MCP.ToonflowServer` + `Exhub.MCP.Tools.Toonflow.*` |
| keyframes / continuity | `Exhub.Toonflow.Keyframe` |
| references / variations | `Exhub.Toonflow.Assets`, `Exhub.Toonflow.Media` |
| prompts / worldview | `Exhub.Toonflow.Prompts`, `Exhub.Toonflow.Worldview` |
| progress fan-out | `Exhub.Toonflow.Progress` (duplicate `Registry`) |
| canvas | `Exhub.Router.ToonflowView`, `Exhub.Toonflow.SocketHandler`, `Exhub.Toonflow.Snapshot` |
| store / DB | `Exhub.Toonflow.Store`, `Schema`, `DB` |

See [docs/modules/toonflow.md](modules/toonflow.md) for the module table and the
live-verification commands.
