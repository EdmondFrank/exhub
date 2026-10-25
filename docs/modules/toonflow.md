# exhub-toonflow

`exhub-toonflow` is ExHub's **native** AI short-drama pipeline — an Elixir/OTP
re-implementation of [Toonflow](https://github.com/HBAI-Ltd/Toonflow-app) built on
ExHub's own primitives, rather than a port of the Node/TypeScript app.

Pipeline: **novel → script → storyboard → image → video → export**, driven
entirely through MCP tools so any MCP client (AiderDesk, Claude Code, Emacs) can
run it.

> **Status: Phases 5–8.** Project management,
> novel ingest, chaptering, event-graph extraction, script generation/versioning,
> cast/scene/prop extraction, character **wardrobe variations**, shot planning
> (storyboard) and **shot splitting**, frame image generation, **start/end
> keyframes** with cross-shot continuity, **candidate grids** with ffmpeg cell
> cropping, clip/voice generation, FFmpeg episode assembly, semantic memory
> recall (`sqlite-vec`), **per-project prompt overrides** and always-injected
> **worldview anchors**, the one-call `toonflow_pipeline_run` plus the read-only
> `toonflow_pipeline_plan` pre-review, director-agent chat, and the browser
> **canvas UI** with websocket progress are all live. See
> [docs/plans/2026-09-30-toonflow-design.md](../plans/2026-09-30-toonflow-design.md),
> [docs/plans/2026-09-30-toonflow-implementation.md](../plans/2026-09-30-toonflow-implementation.md)
> and [docs/plans/2026-10-25-toonflow-bigbanana-alignment.md](../plans/2026-10-25-toonflow-bigbanana-alignment.md).

## MCP Endpoint

```
POST /toonflow/mcp
```

Registered as the built-in hub server `toonflow`; also reachable through the
unified hub endpoint (`/mcp-hub/mcp`) as `toonflow__<tool>`.

## Web UI & live progress

A read-mostly canvas for watching (and nudging) a project, rendered straight from
`Exhub.Router.ToonflowView` — no build step, no `Plug.Static`.

| Route | Kind | Purpose |
|-------|------|---------|
| `GET /toonflow` | HTML | Project index + create form |
| `GET /toonflow/projects/:name` | HTML | Canvas: stage rail, shot grid, inspector, live log, run controls |
| `GET /toonflow/api/projects` | JSON | `{projects: [...]}` (id, counts, timestamps) |
| `POST /toonflow/api/projects` | JSON | Create a project (`{name, description}`) |
| `GET /toonflow/api/projects/:name` | JSON | Full `Snapshot.build/2` payload |
| `POST /toonflow/api/projects/:name/run` | JSON | Run/resume the pipeline (`{stages, from, resume, recall, …}`) |
| `GET /toonflow/media/:name/*path` | file | Serve a project file (see sandbox below) |
| `WS /toonflow/ws?project=<name>` | websocket | Live snapshot + progress frames |

**Mutating routes are loopback-only** unless `ui.require_local` is `false`; a
non-loopback `POST` gets `403 forbidden (mutating endpoints are loopback-only)`
(the app is also reachable over the VPN). Reads are unrestricted.

**Media sandbox** — `/toonflow/media/...` only serves paths under `assets/`,
`output/` and `storyboards/`. `Exhub.Toonflow.Snapshot.resolve_media/3` rejects
traversal, symlinks and non-regular files, returning `404` otherwise.

### Websocket frames

`Exhub.Toonflow.SocketHandler` sends JSON text frames:

| `type` | When | Payload |
|--------|------|---------|
| `snapshot` | on connect / `subscribe` / `refresh` | the `Snapshot.build/2` view-model |
| `stage` | pipeline stage starts / ends / errors | `{stage, status, detail|reason}` |
| `shot` | per-shot media stage | `{stage, shot_id, status, detail|reason}` |
| `jobs` | every `ui.tick_ms` | last 5 job rows (coarse refresh) |
| `heartbeat` | tick when the project is unknown | — |
| `pong` / `error` | after a client `{"action":"ping"}` / bad frame | — |

Client messages: `{"action":"subscribe","project":"…"}`, `"unsubscribe"`,
`"refresh"`, `"ping"`.

> **Cowboy frame gotcha.** `cowlib`'s `cow_ws:frame/2` has **no clause for a bare
> binary**, so a reply must be a frame tuple. `Exhub.Toonflow.SocketHandler`
> wraps every payload via `frames/1 → [{:text, json}]`; returning `[json]`
> crashes the connection with `{:function_clause, [{:cow_ws, :frame, …}]}`.
> Progress is fanned out by `Exhub.Toonflow.Progress` over a **duplicate**
> `Registry` (`Exhub.Toonflow.Progress.Registry`), so many sockets may watch the
> same project; every call is best-effort (`try/rescue`) and a no-op when the
> registry is absent.

> **Adding a `socket/3` route needs a listener (re)start.** Cowboy compiles the
> dispatch table when the listener starts, so a hot-reloaded `socket/3` route is
> **not** picked up by `Exhub.HotReload.reload/0` — it only serves after the VM
> (or the `Exhub.Router.HTTP` listener child) restarts. Ordinary `get`/`post`
> routes live in `Exhub.Router` module code and hot-reload fine. Do **not** try
> to reinstall the dispatch on a live listener with `:cowboy.set_env/3`.

## Tools

### `toonflow_list_projects`

List all projects (newest first) with id, name, workspace path, metadata and
timestamps.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| —         | —    | —        | No parameters. |

### `toonflow_create_project`

Create a project: a local workspace directory plus a registry entry.

| Parameter     | Type   | Required | Default | Description |
|---------------|--------|----------|---------|-------------|
| `name`        | string | ✓        | —       | Lowercase slug used as the directory name (letters, digits, dot, dash, underscore; starts alphanumeric; max 64 chars). |
| `description` | string |          | `""`    | Optional short description stored in `meta`. |

### `toonflow_project_info`

Project metadata plus workspace statistics.

| Parameter | Type   | Required | Description |
|-----------|--------|----------|-------------|
| `name`    | string | ✓        | Project name (or id) to inspect. |

### `toonflow_add_novel`

Ingest a novel and split it into chapters.

| Parameter | Type   | Required | Description |
|-----------|--------|----------|-------------|
| `project` | string | ✓        | Target project name. |
| `path`    | string |          | Local novel file — `.txt`/`.md` read directly, `.pdf`/`.docx`/images via Gitee AI OCR (`doc_extract`). |
| `text`    | string |          | Inline novel text (alternative to `path`). |
| `title`   | string |          | Novel title (defaults to the file name). |

Chapters are detected from `第N章/回/节/篇/卷`, `Chapter N`, or Markdown `#`..`####`
headings; heading-less text becomes a single `全文` chapter. Each chapter is also
mirrored to `chapters/<idx>-<slug>.txt`.

### `toonflow_list_chapters`

Lightweight chapter summaries (id, index, title, text length, preview).

| Parameter  | Type    | Required | Description |
|------------|---------|----------|-------------|
| `project`  | string  | ✓        | Project name. |
| `novel_id` | string  |          | Restrict to one novel id. |
| `limit`    | integer |          | Maximum number of chapters. |

### `toonflow_extract_events`

Extract a structured event graph from chapter(s) with the LLM.

| Parameter    | Type    | Required | Description |
|--------------|---------|----------|-------------|
| `project`    | string  | ✓        | Project name. |
| `chapter_id` | string  |          | Extract only this chapter (default: all). |
| `limit`      | integer |          | Maximum chapters to process. |

Events are typed (`plot`/`conflict`/`reveal`/`emotion`/`action`/`dialogue`/`setup`)
with a summary, characters, location and importance. Re-running replaces the
chapter's events. Per-chapter failures are collected in `errors` instead of
aborting the whole run.

### `toonflow_list_events`

| Parameter    | Type   | Required | Description |
|--------------|--------|----------|-------------|
| `project`    | string | ✓        | Project name. |
| `chapter_id` | string |          | Filter by chapter id. |
| `kind`       | string |          | Filter by event kind. |

### `toonflow_generate_script`

Adapt a chapter (or, with no `chapter_id`, the whole novel) into a short-drama
script, using the chapter's extracted event graph. Every call stores a new
immutable version.

| Parameter      | Type   | Required | Description |
|----------------|--------|----------|-------------|
| `project`      | string | ✓        | Project name. |
| `chapter_id`   | string |          | Adapt a single chapter (default: whole novel). |
| `instructions` | string |          | Extra writing/directing guidance. |

### `toonflow_get_script`

Fetch a script by `script_id`, or by `chapter_id` (+ optional `version`); with
neither, the most recent script in the project.

| Parameter    | Type    | Required | Description |
|--------------|---------|----------|-------------|
| `project`    | string  | ✓        | Project name. |
| `script_id`  | string  |          | Exact script version by id. |
| `chapter_id` | string  |          | Latest script for a chapter (or `version`). |
| `version`    | integer |          | Specific version number with `chapter_id`. |

### `toonflow_update_script`

Store an edited script as a new version, keeping the previous one intact.

| Parameter   | Type   | Required | Description |
|-------------|--------|----------|-------------|
| `project`   | string | ✓        | Project name. |
| `script_id` | string | ✓        | Script id the edit derives from. |
| `content`   | string | ✓        | Full edited script content. |
| `note`      | string |          | Optional note describing the edit. |

### `toonflow_extract_assets`

Extract the cast (characters with stable appearances), scenes and props from a
script, building the project's appearance database.

| Parameter      | Type   | Required | Description |
|----------------|--------|----------|-------------|
| `project`      | string | ✓        | Project name. |
| `script_id`    | string |          | Script to read (default: the latest). |
| `instructions` | string |          | Extra extraction guidance. |

Characters are upserted by name (idempotent) into the `characters` table; the
full extraction is mirrored to `characters/appearance.json`.

### `toonflow_list_characters`

| Parameter | Type    | Required | Description |
|-----------|---------|----------|-------------|
| `project` | string  | ✓        | Project name. |
| `name`    | string  |          | Filter to a single character name. |
| `limit`   | integer |          | Maximum number of characters. |

### `toonflow_list_scenes` / `toonflow_list_props`

The scene and prop libraries extracted by `toonflow_extract_assets` (upserted by
name, so re-extraction is idempotent).

| Parameter | Type    | Required | Description |
|-----------|---------|----------|-------------|
| `project` | string  | ✓        | Project name. |
| `name`    | string  |          | Filter to a single name. |
| `limit`   | integer |          | Maximum number of rows. |

### `toonflow_generate_reference`

Render a *reference image* for a character, scene or prop and store it on that
subject's `refs_json`, so later frames are conditioned on it (`i2i`).

| Parameter | Type   | Required | Description |
|-----------|--------|----------|-------------|
| `project` | string | ✓        | Project name. |
| `kind`    | string | ✓        | `character`, `scene` or `prop`. |
| `name`    | string | ✓        | The subject's name (from the library). |
| `prompt`  | string |          | Override the prompt built from its appearance/description. |
| `model`   | string |          | Image model (default: `media.image_model`). |
| `size`    | string |          | Output size (default `1024x1024`). |

### `toonflow_generate_variation`

Render a **wardrobe variation** of a character — the same face, a different
outfit — conditioned on the character's base reference images.

| Parameter       | Type   | Required | Description |
|-----------------|--------|----------|-------------|
| `project`       | string | ✓        | Project name. |
| `character`     | string | ✓        | Character name. |
| `name`          | string | ✓        | Variation name, e.g. `夜行衣`. |
| `visual_prompt` | string |          | The new look (default: the name). |
| `model` / `size`| string |          | Overrides. |

Each variation is a `character_variations` row holding its own `refs_json`;
re-generating the same name updates it rather than duplicating it.

### `toonflow_list_variations`

| Parameter   | Type   | Required | Description |
|-------------|--------|----------|-------------|
| `project`   | string | ✓        | Project name. |
| `character` | string |          | Filter to one character's variations. |

### `toonflow_set_shot_variations`

Pin which wardrobe variation a shot uses, per character. This is what makes
`refs_for_shot/3` condition the shot's frames on the variation instead of the
character's base look.

| Parameter    | Type   | Required | Description |
|--------------|--------|----------|-------------|
| `project`    | string | ✓        | Project name. |
| `shot_id`    | string | ✓        | Target shot. |
| `selections` | object | ✓        | Map of character name → variation name (or id). `{}` clears all. |

### `toonflow_generate_storyboard`

Turn a script into an ordered shot list (分镜) with the DirectorAgent LLM call,
grounded in the extracted character appearances.

| Parameter      | Type   | Required | Description |
|----------------|--------|----------|-------------|
| `project`      | string | ✓        | Project name. |
| `script_id`    | string |          | Script to adapt (default: the latest). |
| `instructions` | string |          | Extra directing guidance (style, pacing). |

Each shot carries a scene, description, 景别 (`size`), lighting, 运镜 (`motion`),
the characters it features, and a text-to-image `prompt`. Re-running replaces
that script's shots (idempotent) and mirrors them to
`storyboards/<script_id>.json`.

### `toonflow_list_shots`

| Parameter   | Type    | Required | Description |
|-------------|---------|----------|-------------|
| `project`   | string  | ✓        | Project name. |
| `script_id` | string  |          | Restrict to one script's shots. |
| `scene`     | string  |          | Restrict to one scene. |
| `limit`     | integer |          | Maximum number of shots. |

### `toonflow_split_shot`

Split one over-packed shot into finer sub-shots with the director LLM
(`storyboard.split.*`).

| Parameter      | Type   | Required | Description |
|----------------|--------|----------|-------------|
| `project`      | string | ✓        | Project name. |
| `shot_id`      | string | ✓        | The shot to split. |
| `instructions` | string |          | How to split (free-form guidance). |

The sub-shots **replace** the original **in place** — same order position, the
script's indices resequenced — and inherit its scene, cast and outer keyframe
prompts (the first sub-shot keeps the original's start frame prompt, the last
keeps its end frame prompt), so start/end continuity survives the split. The
replaced shot's generated assets are removed, since they described the coarser
shot. At most 8 sub-shots are kept. Returns `sub_shots`, `created` (new ids) and
the new shot summaries.

### `toonflow_generate_image`

Generate a frame for a shot (or a free prompt) via the shared Gitee AI / moark
image API, saving it under `assets/images/` and recording an asset row.

| Parameter | Type   | Required | Description |
|-----------|--------|----------|-------------|
| `project` | string | ✓        | Project name. |
| `shot_id` | string |          | Render this shot (builds the prompt/references). |
| `prompt`  | string |          | Free-form prompt (used as-is; alternative to `shot_id`). |
| `model`   | string |          | Image model (default: the configured `media.image_model`). |
| `size`    | string |          | Output size, e.g. `1024x1024` (default). |

When `shot_id` is given the prompt is assembled from the shot and character
appearances are appended for consistency; character reference images (when
present) are conditioned through `i2i`. The returned `path` is the local image.

### `toonflow_generate_keyframe`

Render a shot's **start** or **end** keyframe (an asset of kind `keyframe_start`
/ `keyframe_end`). Two frames per shot make `fl2va` (first-last-frame-to-video)
possible: the `keyframes` pipeline stage, and `toonflow_generate_video` with
`task: "fl2va"`, use them.

| Parameter | Type   | Required | Description |
|-----------|--------|----------|-------------|
| `project` | string | ✓        | Project name. |
| `shot_id` | string | ✓        | Target shot. |
| `type`    | string | ✓        | `start` or `end`. |
| `prompt`  | string |          | Override the shot's `meta.keyframe_<type>_prompt`. |
| `refs`    | string[] |        | Extra reference images. |
| `model` / `size` | string |   | Overrides. |

The prompt resolves explicit → shot `meta.keyframe_<type>_prompt` → the shot's
own prompt/description. The storyboard LLM writes those prompts
(`keyframe_start_prompt` / `keyframe_end_prompt`).

### `toonflow_copy_keyframe`

Duplicate an **adjacent** shot's frame onto this shot — the continuity trick that
keeps a cut from jumping. `prev_end` copies the previous shot's end frame into
this shot's **start** frame; `next_start` copies the next shot's start frame into
this shot's **end** frame. The image is duplicated under this shot's own key, so
regenerating the neighbour cannot mutate the copy.

| Parameter   | Type   | Required | Description |
|-------------|--------|----------|-------------|
| `project`   | string | ✓        | Project name. |
| `shot_id`   | string | ✓        | Target shot. |
| `direction` | string | ✓        | `prev_end` or `next_start`. |

### `toonflow_generate_shot_candidates`

Render a **candidate grid** for a shot: one image holding several alternative
compositions, so a frame can be picked instead of regenerated blindly.

| Parameter | Type    | Required | Description |
|-----------|---------|----------|-------------|
| `project` | string  | ✓        | Project name. |
| `shot_id` | string  | ✓        | Shot to propose candidates for. |
| `count`   | integer |          | Cells (default `media.candidates`, 9 → a 3×3 grid). |
| `prompt`  | string  |          | Override the shot's prompt. |
| `model` / `size` | string |   | Overrides. |

The grid shape is derived from `count` (`Media.grid_shape/1`) and recorded in the
asset's `meta.grid`. No candidate image is used by the pipeline automatically —
this is an opt-in director tool.

### `toonflow_select_candidate`

Promote one cell of a candidate grid into the shot's start or end keyframe.

| Parameter       | Type   | Required | Description |
|-----------------|--------|----------|-------------|
| `project`       | string | ✓        | Project name. |
| `shot_id`       | string | ✓        | Shot the keyframe belongs to. |
| `type`          | string | ✓        | `start` or `end`. |
| `cell`          | string |          | 0-based grid cell (`0`..`8`), or `whole` (omit) for the whole image. |
| `candidates_id` | string |          | A specific grid (default: the shot's latest). |

A numeric cell is cropped out with ffmpeg (`Media.crop_filter/3` +
`Media.crop_image/5`); anything else copies the whole image. The result is a
normal `keyframe_<type>` asset, so the `videos` stage picks it up automatically.

### `toonflow_generate_video`

Generate a clip for a shot (or a free prompt) via the shared MoArk async video
API, saving it under `assets/videos/` and recording an asset row. The call
submits the task and polls until it finishes.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project` | string | ✓ | Project name. |
| `shot_id` | string | | Render this shot (builds the prompt/frame). |
| `prompt` | string | | Free-form prompt (used as-is; alternative to `shot_id`). |
| `task` | string | | `t2va` (text-to-video) or `fl2va` (first/last-frame-to-video). Default: `fl2va` when the shot already has a frame image, else `t2va`. |
| `model` | string | | Video model (default: the configured `media.video_model`). |
| `duration_seconds` | integer | | Clip length, 4–15 (default 6). |
| `aspect_ratio` | string | | e.g. `16:9` (default), `9:16`, `1:1`. |
| `seed` | integer | | Optional random seed. |
| `first_frame` | string | | Image URL, data URI or local path; required for `fl2va` when the shot has no frame. |
| `last_frame` | string | | Interpolation target for `fl2va`; defaults to the shot's `keyframe_end` when `media.last_frame` is on. |

For `fl2va`, the shot's `keyframe_start` (else its latest frame image) is used as
the first frame and its `keyframe_end` as the last, unless `first_frame` /
`last_frame` are given. The returned `path` is the local clip.

### `toonflow_generate_voice`

Synthesize speech via the Gitee AI synchronous TTS API
(`Step-Audio-TTS-3B` by default), saving it under `assets/audio/` and recording
an asset row. The audio bytes are returned directly (no task/poll); the saved
file's extension is corrected to the detected container (e.g. `.wav` for
`ChatTTS`).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project` | string | ✓ | Project name. |
| `shot_id` | string | | Use this shot's dialogue (`meta.dialogue` / `meta.台词`, else the shot description). |
| `text` | string | | Explicit text (alternative to `shot_id`). |
| `voice` | string | | Voice name (default: the configured `media.tts_voice`). |
| `model` | string | | TTS model (default: the configured `media.tts_model`, `Step-Audio-TTS-3B`). |
| `prompt_audio_url` | string | | Reference audio URL for the clone models (IndexTTS-2, GLM-TTS). |
| `prompt_text` | string | | Transcript of `prompt_audio_url` (optional). |
| `speaker` | string | | Alias for `voice` (legacy). |
| `output_format` | string | | Nominal format; corrected to the detected one. |
| `language` | string | | Optional language hint. |
| `instruction` | string | | Optional style instruction. |

### `toonflow_assemble`

Assemble a script's storyboard clips (in order) into a single video under the
project's `output/`, optionally mixing per-shot voiceover and writing an `.srt`
subtitle track derived from each shot's dialogue.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project` | string | ✓ | Project name. |
| `script_id` | string | | Script to assemble (default: the latest). |
| `scene` | string | | Only assemble shots from this scene. |
| `subtitles` | boolean | | Soft-mux the generated `.srt` into the video (default: `assembly.subtitles`). |
| `mix_audio` | boolean | | Mix the shots' voice clips (default false). |
| `name` | string | | Output file stem (default: the script id). |

Every shot in the script must already have a generated clip
(`toonflow_generate_video`). The `.srt` timings come from `ffprobe` on each
clip; subtitle text is the shot's `meta.dialogue` / `meta.台词`, else its
description.

### `toonflow_export`

Assemble a script's clips and package the result as a named episode under
`output/`, returning a manifest (video, subtitle and output-dir paths plus an
asset count).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project` | string | ✓ | Project name. |
| `script_id` | string | | Script to export (default: the latest). |
| `scene` | string | | Only export shots from this scene. |
| `filename` | string | | Output video name (default: the script id); a `.mp4` extension is added if missing. |
| `subtitles` | boolean | | Soft-mux the generated `.srt` (default: `assembly.subtitles`). |
| `mix_audio` | boolean | | Mix the shots' voice clips (default false). |

### `toonflow_pipeline_run`

Run the whole pipeline (or a subset) in one call — **novel → events → script →
assets → storyboard → images → videos → voices → assemble** — recording the run
as a `jobs` row and returning a per-stage report.

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project` | string | ✓ | Project name. |
| `stages` | string[] | | Subset to run (default: all), e.g. `["script","storyboard"]`. |
| `from` | string | | Run all stages starting at this one. |
| `resume` | boolean | | Skip stages/assets that already exist (default **true**). |
| `path` / `text` / `title` | | | Novel source for the `novel` stage. |
| `chapter_id` | string | | Scope events/script to a single chapter. |
| `instructions` | string | | Extra writing/directing guidance. |
| `recall` | boolean | | Append semantically-recalled memory to script/storyboard. |
| `shots_limit` | integer | | Cap the shots processed by the media stages. |
| `mix_audio` / `subtitles` | boolean | | Assembly options. |
| `continue_on_error` | boolean | | Keep going after a stage fails (default false → stop at first failure). |

With `resume: true` (the default) a stage whose output already exists is
`skipped`, and per-shot media stages skip shots that already have the
corresponding asset, so re-runs are cheap and idempotent. Returns
`%{"stages" => [%{"stage","status","detail"|"reason"}], "completed", "errors"}`;
a stop-on-error run returns `success: false` with `stage` + `reason`.

### `toonflow_pipeline_plan`

Read-only pre-review — the cheap way to see the cost of a run before committing
to it. No parameters beyond `project`.

Returns `%{"stages" => [%{"stage", "status"}], "next" => name | nil, "shots" =>
[%{"idx", "link", "ready", "missing"}]}`. A stage status is `done` / `partial` /
`ready`; a shot's `link` is how its clip is produced — `keyframe` (first/last
frame interpolation, the default), `image` or `text`, from `shots.meta.link`
(`Pipeline.normalize_link/1` also accepts the task names `fl2va` / `i2va` /
`t2va`) — and `missing` lists what is still absent for it. Opt-in extras
(candidate grids, variations, keyframe copies) are never counted as required.

### `toonflow_prompt_list`

List the prompt templates this project overrides: name, latest version, version
count, the latest note, and whether a shipped template of that name exists.
Templates without an override are absent (the shipped file is used).

### `toonflow_prompt_get`

Read the effective text of a prompt template — the project's override when one
exists, else the shipped `priv/toonflow/prompts/<name>.md` — plus its `source`
(`"override"` / `"shipped"`) and `version`. Template names: `events.system`,
`events.user`, `script.system`, `script.user`, `assets.system`, `assets.user`,
`storyboard.system`, `storyboard.user`, `storyboard.split.system`,
`storyboard.split.user`. `{{key}}` placeholders stay unsubstituted.

### `toonflow_prompt_set`

Save a new version of a project's prompt override — the *effective* template for
that project from now on. Versions are immutable and numbered, so nothing is
lost. Keep the `{{key}}` placeholders the shipped template uses or the stage will
render with empty values. Parameters: `project` (required), `name` (required),
`content` (required), `note`.

### `toonflow_prompt_history` / `toonflow_prompt_revert`

`toonflow_prompt_history` lists every saved version of an override (newest first)
with its note, timestamp and content. `toonflow_prompt_revert` (`project`,
`name`, `version`) re-saves an earlier version as a **new** one (noted
`revert to vN`), so the history stays linear and the revert itself is auditable.

Overrides are opt-out: set `prompts.overrides` to `false` and `Prompts.resolve/4`
falls back to the shipped file (`current/3` still reports the stored override).

### `toonflow_worldview_set`

Record (or replace) a worldview anchor for the project: the map, region, period,
art style, music style — whatever must stay fixed. Unlike `toonflow_memory_add`
(recalled by similarity), the worldview is **always** injected into the script,
assets and storyboard instructions, so every stage generates inside the same
world.

| Parameter | Type   | Required | Description |
|-----------|--------|----------|-------------|
| `project` | string | ✓        | Project name. |
| `text`    | string | ✓        | The anchors, e.g. `架空唐代，岭南水乡，水墨动画风格`. |
| `title`   | string |          | Anchor topic (default `世界观`), e.g. `美术风格`. |

Notes are `memory_notes` rows of kind `worldview`, one per title (re-writing a
title replaces it).

### `toonflow_worldview_get`

Read the project's worldview notes and the exact digest injected into the
instructions. Optional `title` filter. Returns `count`, `notes` and `digest`
(empty when nothing is set).

### `toonflow_memory_add`

Record a director's memory note for a project (tone, style, casting,
continuity …).

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `project` | string | ✓ | Project name. |
| `text` | string | ✓ | The note body. |
| `kind` | string | | Note kind, e.g. `style` / `tone` / `casting` (default `note`). |
| `title` | string | | Short title (default `未命名`). |
| `meta` | string | | Optional JSON object stored with the note. |

### `toonflow_memory_list`

List a project's notes, newest first. Optional `kind` filter and `limit`.

### `toonflow_memory_index`

Export a project's notes to `<project>/memory/notes/*.md` and (re)build them into
the shared `sqlite-vec` index. Only notes whose content changed since the last
build are re-embedded. Returns `scanned`/`changed`/`indexed`/`failed`/`chunks`.
Requires the embedding API key configured under `:brain_rag`.

### `toonflow_memory_search`

Semantic (vector) search over the indexed notes. Parameters: `project`
(required), `query` (required), `top_k` (default 5, or `memory.top_k`). Returns
the closest chunks with a `similarity` score.

### `toonflow_chat`

Send a message to a Toonflow agent (default `"toonflow"`) and get its reply. The
agent is started lazily and holds the full `toonflow` MCP tool set, so it can
drive projects itself. Parameters: `message` (required), `agent`.

### `toonflow_list_agents`

List the registered agent profiles (including `"toonflow"`) and whether each is
running. No parameters.

## Pipeline

### novel → script

```
toonflow_create_project  {name: "my-drama"}
toonflow_add_novel       {project: "my-drama", path: "~/novels/story.txt"}
toonflow_list_chapters   {project: "my-drama"}
toonflow_extract_events  {project: "my-drama", chapter_id: "chp_..."}
toonflow_generate_script {project: "my-drama", chapter_id: "chp_...",
                          instructions: "更紧张，强化反转"}
toonflow_get_script      {project: "my-drama", chapter_id: "chp_..."}
```

### script → storyboard → frames

```
toonflow_extract_assets     {project: "my-drama"}
toonflow_list_characters    {project: "my-drama"}
toonflow_generate_storyboard{project: "my-drama", script_id: "scr_..."}
toonflow_list_shots         {project: "my-drama", script_id: "scr_..."}
toonflow_generate_image     {project: "my-drama", shot_id: "sht_..."}
```

### keyframes, candidates & continuity

```sh
# scene/prop references + character wardrobe variations
toonflow_list_scenes        {project: "my-drama"}
toonflow_generate_reference {project: "my-drama", kind: "scene", name: "祖宅"}
toonflow_generate_variation {project: "my-drama", character: "林昭", name: "夜行衣",
                             visual_prompt: "墨色夜行衣"}
toonflow_set_shot_variations{project: "my-drama", shot_id: "sht_...",
                             selections: {林昭: "夜行衣"}}

# start/end frames → fl2va interpolation (and copy the neighbour's frame for a clean cut)
toonflow_generate_keyframe  {project: "my-drama", shot_id: "sht_...", type: "start"}
toonflow_generate_keyframe  {project: "my-drama", shot_id: "sht_...", type: "end"}
toonflow_copy_keyframe      {project: "my-drama", shot_id: "sht_...", direction: "prev_end"}
toonflow_generate_video     {project: "my-drama", shot_id: "sht_...", task: "fl2va"}

# or: try 9 compositions in one image, then crop the best one into the keyframe
toonflow_generate_shot_candidates {project: "my-drama", shot_id: "sht_...", count: 9}
toonflow_select_candidate         {project: "my-drama", shot_id: "sht_...",
                                   type: "start", cell: "4"}

# refine a shot that carries two beats, then pre-review the cost of a re-run
toonflow_split_shot         {project: "my-drama", shot_id: "sht_...",
                             instructions: "拆成三个递进的镜头"}
toonflow_pipeline_plan      {project: "my-drama"}
```

### clips, voice → episode

```sh
toonflow_generate_video  {project: "my-drama", shot_id: "sht_..."}
toonflow_generate_voice  {project: "my-drama", shot_id: "sht_..."}
toonflow_assemble        {project: "my-drama", script_id: "scr_...", mix_audio: true}
toonflow_export          {project: "my-drama", script_id: "scr_...", filename: "ep01"}
```

`toonflow_assemble` concatenates the clips, writes `output/<name>.srt`, and
(by default) soft-muxes the subtitles; `toonflow_export` does the same and
packages the result under `output/<filename>.mp4`.

### one call, with memory recall

```sh
# record directorial notes, index them, then recall them into the run
toonflow_memory_add   {project: "my-drama", kind: "style", title: "基调",
                       text: "悬疑冷峻，青蓝冷光，手持运镜"}
toonflow_memory_index {project: "my-drama"}

toonflow_pipeline_run {project: "my-drama", path: "~/novels/story.txt",
                       title: "风雨", recall: true, mix_audio: true}
```

`toonflow_pipeline_run` runs every stage and returns a per-stage report; the
script and storyboard stages append the recalled notes to their instructions.
Re-running resumes (skips existing outputs). You can also hand the whole thing
to the director agent:

```sh
toonflow_chat {message: "为 my-drama 生成全片并导出 ep01"}
```

## Workspace layout

Projects live under the workspace root (default `~/.config/exhub/toonflow`):

```
<root>/
  toonflow.db                 # project registry (SQLite)
  toonflow_index.db           # memory index (SQLite + sqlite-vec), all projects
  workspaces/
    <project>/
      project.json            # portable metadata mirror
      index.db                # project data (novels, chapters, events, ...)
      novels/ chapters/ scripts/ characters/ storyboards/
      memory/notes/           # exported memory notes (indexed sources)
      assets/ images/ videos/ audio/
      output/
```

## Configuration

```elixir
config :exhub, :toonflow, %{
  # nil => ~/.config/exhub/toonflow
  "root_dir" => nil,
  "agents" => %{
    "script" => "deepseek-v4.1-flash",
    "director" => "deepseek-v4.1-flash",
    "qa" => "deepseek-v4.1-flash"
  },
  "media" => %{
    "image_model" => "qwen-image-2.0",
    "video_model" => "MiniMax-H3",
    "tts_model" => "Step-Audio-TTS-3B",
    "tts_voice" => "中文女",
    # Start/end keyframes: the `keyframes` stage renders per-shot first/last
    # frames, and `videos` feeds the end frame to `fl2va`.
    "keyframes" => true,
    "last_frame" => true,
    # Default `toonflow_generate_shot_candidates` cells (9 → a 3×3 grid).
    "candidates" => 9
  },
  "prompts" => %{
    # Per-project prompt overrides win over priv/toonflow/prompts.
    "overrides" => true
  },
  "memory" => %{
    # Semantic recall (Phase 4). The index reuses the Brain RAG embedding
    # stack, so provider/model/dim come from :brain_rag; these mirror its
    # defaults for reference.
    "enabled" => true,
    "index_path" => nil,
    "top_k" => 5,
    "scope" => "project",
    "batch_size" => 16,
    "rebuild_timeout" => 600_000,
    "embedding_model" => "Qwen3-Embedding-4B",
    "dim" => 1024
  },
  "assembly" => %{
    "ffmpeg_path" => "ffmpeg",
    "ffprobe_path" => "ffprobe",
    "subtitles" => true
  },
  "ui" => %{
    # Canvas web view + live progress websocket (Phase 5).
    # `require_local` restricts the mutating REST endpoints (create/run) to
    # loopback clients, since the app is also reachable over the VPN.
    "enabled" => true,
    "tick_ms" => 5000,
    "require_local" => true
  }
}
```

Media generation reuses the shared `:giteeai_api_key` (image/video/TTS/vision);
LLM calls reuse `Exhub.Llm.LlmConfigServer`. Each media backend is injectable,
so tests never hit the network: `exhub, :toonflow_media_client`
(`Exhub.Toonflow.Media.Default`), `exhub, :toonflow_video_client`
(`Exhub.Toonflow.Video.Default`) and `exhub, :toonflow_voice_client`
(`Exhub.Toonflow.Voice.Default`). Episode assembly shells out to the
`assembly.ffmpeg_path` / `assembly.ffprobe_path` binaries via `Exile`.

The memory index (`Exhub.Toonflow.Memory.Index`) reuses
`Exhub.MCP.Brain.RAG.{Embedder, Chunker}`; override the embedder with
`exhub, :toonflow_embedder` and the registered server with
`exhub, :toonflow_index_server` (both used by the tests).

## Modules

| Module | Purpose |
|--------|---------|
| `Exhub.Toonflow` | Shared helpers (ids, timestamps, previews) |
| `Exhub.Toonflow.Config` | Config resolution + defaults |
| `Exhub.Toonflow.Workspace` | Pure project path helpers |
| `Exhub.Toonflow.Schema` | SQLite DDL + row codecs (pure) |
| `Exhub.Toonflow.DB` | Low-level `Exqlite` helpers (shared) |
| `Exhub.Toonflow.Store` | GenServer over the registry; `run_project/3` runs project-DB work serialized |
| `Exhub.Toonflow.Novel` | Novel ingest + chapter splitting (pure `split_chapters/1`) |
| `Exhub.Toonflow.Events` | Event-graph extraction (pure `parse_events/1`) |
| `Exhub.Toonflow.Script` | Script generation/versioning (pure `parse_script/1`) |
| `Exhub.Toonflow.Assets` | Character/scene/prop extraction (pure `parse_assets/1`), reference images and wardrobe variations |
| `Exhub.Toonflow.Storyboard` | Shot planning (pure `parse_storyboard/1`, `characters_of/1`, `shot_prompt/3`) + `split_shot/3` |
| `Exhub.Toonflow.Keyframe` | Start/end keyframe generation, cross-shot continuity copies, candidate-cell selection (pure `normalize_type/1`, `adjacent/2`, `normalize_cell/1`, `continuity_attrs/3`) |
| `Exhub.Toonflow.Media` | Frame generation + candidate grids; `Media.Client` behaviour + `Media.Default` (moark t2i/i2i); shared asset insert/query helpers; pure `crop_filter/3`/`crop_argv/6` + `crop_image/5` |
| `Exhub.Toonflow.Video` | Clip generation; `Video.Client` behaviour + `Video.Default` (moark async video, `t2va`/`i2va`/`fl2va`) |
| `Exhub.Toonflow.Voice` | TTS generation; `Voice.Client` behaviour + `Voice.Default` (moark async speech) |
| `Exhub.Toonflow.Assemble` | FFmpeg episode assembly + SRT (pure argv/builders, `Exile` execution) |
| `Exhub.Toonflow.Jobs` | Job ledger over the `jobs` table |
| `Exhub.Toonflow.HTTP` | Shared MoArk async submit/poll/download helpers |
| `Exhub.Toonflow.Json` | Lenient LLM JSON extraction (fences/prose, CJK-safe) |
| `Exhub.Toonflow.Memory` | Memory-note CRUD + export/index/search/recall |
| `Exhub.Toonflow.Worldview` | Always-injected worldview anchors (pure `digest/1`) |
| `Exhub.Toonflow.Prompts` | `priv/toonflow/prompts/*` rendering + per-project versioned overrides (`resolve/4`, `set`/`history`/`revert`) |
| `Exhub.Toonflow.Memory.Index` | `sqlite-vec` semantic index GenServer (global DB, per-project scoping) |
| `Exhub.Toonflow.Pipeline` | Staged orchestration (`run`/`resume`/`plan` with per-shot pre-review) over `Jobs` |
| `Exhub.Toonflow.Snapshot` | Canvas view-model (`build/2`) + sandboxed media-path resolution |
| `Exhub.Toonflow.Progress` | Best-effort progress fan-out over a duplicate `Registry` |
| `Exhub.Toonflow.SocketHandler` | Cowboy websocket for the canvas (`/toonflow/ws`) |
| `Exhub.Router.ToonflowView` | Server-rendered canvas HTML (inline CSS/JS) |
| `Exhub.Toonflow.LLM` | LLM behaviour + default (wraps `Exhub.Genclaw.LLMHelper`) |
| `Exhub.MCP.ToonflowServer` | Anubis MCP server (`/toonflow/mcp`) |
| `Exhub.MCP.Tools.Toonflow.*` | Tool components |

## Tests

```sh
mix test --no-start test/exhub/toonflow/
```

## See Also

- `docs/plans/2026-09-30-toonflow-design.md` — full design
- `docs/plans/2026-09-30-toonflow-implementation.md` — phased build plan
- `docs/modules/mcp-hub.md` — how built-in MCP servers are wired
- `docs/modules/agent-hub.md` — the sagents framework reused for Toonflow agents