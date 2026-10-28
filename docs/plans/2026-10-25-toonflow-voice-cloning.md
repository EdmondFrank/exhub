# ToonFlow Voice — Multi-Model TTS + Per-Character Voice Cloning

**Date**: 2026-10-25
**Status**: Proposed — awaiting review
**Scope**: server / MCP only (no canvas / `ToonflowView` changes)
**Design refs**: [2026-09-30-toonflow-design.md](2026-09-30-toonflow-design.md), [2026-10-25-toonflow-bigbanana-alignment.md](2026-10-25-toonflow-bigbanana-alignment.md)
**Module ref**: [docs/modules/toonflow.md](../modules/toonflow.md), [docs/modules/speak.md](../modules/speak.md)
**Evidence**: live MoArk/Gitee AI probes, 2026-10-25 (see §2)

---

## 1. Context & goals

`Exhub.Toonflow.Voice` already does per-shot TTS with a per-character `media.voice_map`,
but its default backend (`TeleTTS-Bidirectional-Mandarin`, a WebSocket preset TTS)
has only 10 preset voices of which **exactly one is male** and most are dialect-accented
female voices. That makes multi-character dialogue hard to cast, and there is no
voice cloning and no audio-vs-shot-duration check.

This plan (A1–A4, one PR) upgrades the voice layer to:

1. **A1** — a new `Exhub.TTS.Async` client (MoArk async submit→poll) covering the
   async-only TTS models.
2. **A2** — route by model across `Realtime` / `Sync` / `Async`.
3. **A3** — clean baseline default: `Step-Audio-TTS-3B` (standard-Mandarin
   `中文男` / `中文女`), replacing the dialect-skewed TeleTTS default.
4. **A4** — **per-character voice cloning** (`IndexTTS-2` / `GLM-TTS`), seeded from a
   MoArk async clip, plus a dialogue-vs-duration budget check.

Non-goals (this pass): lip-sync / talking-head (MoArk `InfiniteTalk` / `Duix-Avatar`
are listed but **not served** — see §2), BGM (`ACE-Step`, not on the speech
endpoints), translation-driven multi-language dubbing, and any emotion-tag TTS
(MoArk has no equivalent of 豆包 "多情感").

---

## 2. Live probe results (2026-10-25, `ai.gitee.com`)

| Model | Endpoint | Result |
|-------|----------|--------|
| `Step-Audio-TTS-3B` | sync `POST /v1/audio/speech` | ✅ 200 MP3, honours `voice` (`中文男` / `中文女`) |
| `ChatTTS` | sync | ✅ 200 WAV (fixed speaker; `voice_url` timbre) |
| `IndexTTS-2` | sync (clone) | ✅ 200 WAV with `prompt_audio_url` |
| `GLM-TTS` | sync (clone) | ✅ 200 WAV with `prompt_audio_url` |
| `CosyVoice3` | async `inputs:"文本"` | ✅ task success, `.mp3` URL |
| `Spark-TTS-0.5B` | async `inputs:"文本"` + `gender/pitch/speed` | ✅ task success, `.mpeg` URL (**free**) |
| `Qwen3-TTS` | async | ⚠️ task accepted; requires an `inputs.prompt` shape still to be pinned |
| `VoxCPM2` / `VoxCPM2-Pro`, `ACE-Step-v1-3.5B` | sync + async | ❌ `暂不支持该接口` (listed, not served) |
| `TeleTTS-*` | wss `/v1/audio/speech/ws` | ✅ (already wired via `Exhub.TTS.Realtime`) |
| ASR: `SenseVoiceSmall`, `whisper-large-v3-turbo`, `GLM-ASR`, `Fun-ASR-Nano-2512` | `POST /v1/audio/transcriptions` | ✅ |
| `TeleASR-MultiDialect` | transcriptions | ❌ `暂不支持该接口` |
| Translation: `HY-MT2-7B`, `Hy-MT2-30B-A3B`, `qwen-mt-flash` | `/v1/chat/completions` | ✅ |
| `InfiniteTalk`, `Duix-Avatar` | videos/images async | ❌ `暂不支持该接口` |

**Cloning constraint (decisive):** `prompt_audio_url` must be a **publicly fetchable
URL** ending in an audio extension. A `data:` URI is rejected (`Unsupported file
type: g==`), a local path is rejected, and MoArk exposes **no upload endpoint**
(`/v1/files`, `/v1/voiceprint`, … → 404). But an **async** TTS output is itself a
public presigned URL (`https://gitee-ai.su.bcebos.com/…`, ~7-day expiry), so the
chain *seed (async) → clone (sync)* is fully self-contained on MoArk.

---

## 3. Locked design decisions

| # | Decision | Rationale |
|---|----------|-----------|
| D1 | New `Exhub.TTS.Async` mirrors the `Sync`/`Realtime` return shape (`{:ok, %{audio, format, …}}`). | Callers stay backend-agnostic; `Voice.Default` just routes. |
| D2 | `Voice.Default` routes by an explicit `@async_models` list, else `Realtime.model?/1`, else `Sync`. | One place to add models; matches the existing `Realtime.model?` pattern. |
| D3 | Default `media.tts_model` → `Step-Audio-TTS-3B`, `media.tts_voice` → `中文女`. | Standard-Mandarin male+female baseline; TeleTTS stays selectable. |
| D4 | Per-character voice spec lives in `characters.meta_json["voice"]` (**no schema migration**). `characters.refs_json` is left for i2i image refs. | `meta_json` is free-form and already returned by `toonflow_list_characters`. |
| D5 | Cloning uses the **seed(dir)→clone** pattern: an async TTS clip's `file_url` is the `prompt_audio_url`. | Only public URL source that is fully on MoArk; no external hosting. |
| D6 | The local reference WAV (`assets/audio/refs/<character>.wav`) is the authoritative copy; the URL is cached with an expiry and re-seeded when stale. | Presigned URLs expire (~7d). |
| D7 | Resolution order: explicit `opts` > character voice spec > `media.voice_map` > `media.tts_voice`. | Explicit always wins; matches the existing `voice_map` precedence. |
| D8 | Audio-vs-duration budget is a **pure** function (`Voice.speech_budget/2`), surfaced as a warning + suggested `speech_rate`, never a hard failure. | Industry heuristic ~4 chars/sec; must not break existing runs. |

---

## 4. A1 — `Exhub.TTS.Async` (new module)

`lib/exhub/tts/async.ex`

- Submit `POST https://api.moark.com/v1/async/audio/speech` → `%{"task_id" => id}`,
  poll `GET /v1/task/{id}` until `status` ∈ success/failure, then download
  `output.file_url`. **Reuse `Exhub.Toonflow.HTTP`** (async submit/poll/download
  helpers already used by the video client) rather than reimplementing polling.
- API key from `:exhub, :giteai_api_key` (same as `Sync`).
- Per-model payload builders (from §2):
  - `CosyVoice3` → `%{"inputs" => text}`
  - `Spark-TTS-0.5B` → `%{"inputs" => text, "gender" => g, "pitch" => p, "speed" => s}`
  - `Qwen3-TTS` → best-effort; pin the `inputs.prompt` shape before enabling
    (ship behind the `@async_models` list so it is opt-in until verified).
- `synthesize(text, opts)` returns the same shape as `Sync.synthesize/2`
  (`audio` binary + detected `format`), so downstream save/asset code is shared.
- Serialise concurrent submissions behind a `:global` lock (same rationale as
  `Realtime`), and cap/poll with the existing timeouts.
- Detection of real container from magic bytes (reuse `Sync.detect_format/1`).

## 5. A2 — `Exhub.Toonflow.Voice.Default` routing

`lib/exhub/toonflow/voice.ex` (all three modules `Voice` / `Voice.Client` /
`Voice.Default` live in this one file).

```elixir
@async_models ~w(CosyVoice3 Spark-TTS-0.5B Qwen3-TTS)

def generate_voice(text, opts) do
  model = Keyword.get(opts, :model) || Sync.default_model()
  cond do
    model in @async_models -> async(text, model, opts)
    Realtime.model?(model) -> realtime(text, model, opts)
    true                   -> sync(text, model, opts)
  end
end
```

- Add the `async/3` branch (mirrors `sync/3`: build opts, call, `save/2`).
- Keep `reject_nil/1` and the format-correction behaviour.

## 6. A3 — config defaults

`config/config.exs` → `:exhub, :toonflow, media` (~L140):

```elixir
"tts_model" => "Step-Audio-TTS-3B",   # was TeleTTS-Bidirectional-Mandarin
"tts_voice" => "中文女",               # was guangzhounv
```

- Update the surrounding comment block (TeleTTS is now a *selectable* dialect
  option, not the default).
- `voice_map` values gain optional clone fields (see A4).
- **Runtime caveat**: hot reload does NOT re-read `sys.config`, so a live VM must
  be updated with `bin/exhub rpc 'Application.put_env(:exhub, :toonflow, …)'`
  (same gotcha as `media.tts_model` in the existing memory note).

## 7. A4 — per-character voice cloning chain

### 7.1 Data model (no migration)

- Reference clip: `assets/audio/refs/<character>.wav` (authoritative, permanent).
- Registry: `characters.meta_json["voice"]`:

```json
{
  "model": "IndexTTS-2",
  "source": "seeded",
  "seed_model": "Spark-TTS-0.5B",
  "seed_opts": {"gender": "male", "pitch": 3, "speed": 2},
  "prompt_text": "我是林昭，岭南水乡的剑客，这是我声纹的样本。",
  "ref_path": "assets/audio/refs/林昭.wav",
  "ref_url": "https://gitee-ai.su.bcebos.com/…",
  "ref_url_expires_at": 1793000000,
  "speech_rate": 1.0
}
```

- Mirror an `assets` row (`kind: "voice_ref"`) so it shows on the canvas and is
  regenerable.

### 7.2 New module `Exhub.Toonflow.VoiceRef`

`lib/exhub/toonflow/voice_ref.ex`

| Function | Purpose |
|----------|---------|
| `seed(project, character, opts)` | Async TTS a short 声纹样本 → download `file_url` to `ref_path` → write `meta_json["voice"]` |
| `set(project, character, opts)` | Manual: `ref_audio_url` + `prompt_text` (+ optional `model`) |
| `get(project, character)` / `list(project)` | Read voice spec(s) |
| `refresh(project, character)` | Re-seed / re-host when `ref_url` is stale |
| `expired?(spec, now)` *(pure)* | URL-expiry check |
| `merge_spec(voice_map_spec, char_spec)` *(pure)* | Apply D7 precedence |

Seed uses A1's async client (Spark/CosyVoice3), so **no external hosting** is needed
for the happy path; `set/3` covers a user-supplied target timbre.

### 7.3 Wire into `Exhub.Toonflow.Voice`

Today `prompt_audio_url` / `prompt_text` are read **only** from explicit opts
(voice.ex ~L85). Change to also read the character voice spec:

```elixir
spec = Config.voice_spec(Config.voice_map(), speaker)      # voice_map part
      |> VoiceRef.merge_spec(VoiceRef.get(project, speaker))
model = opts[:model] || spec["model"] || media_config()["tts_model"]

|> put_opt(:prompt_audio_url, opts[:prompt_audio_url] || spec["ref_url"])
|> put_opt(:prompt_text,      opts[:prompt_text]      || spec["prompt_text"])
```

- When a character has a voice spec, the clone model is chosen automatically.
- Per-shot flow: `shot.meta.speaker` → character → `ref_url` → `IndexTTS-2`/`GLM-TTS`.
- Fallback unchanged for un-seeded characters (voice_map / TeleTTS presets).

### 7.4 Speech budget (audio-vs-duration)

Pure `Voice.speech_budget(duration_s, dialogue)` → `:ok | {:tight, rate} | {:over, chars}`,
heuristic ≈ `duration_s * 4` chars. Used to (a) warn on oversize dialogue in
`toonflow_generate_voice`, (b) auto-suggest `speech_rate`, (c) list offenders in
`toonflow_pipeline_plan`.

## 8. MCP tools

| Tool | Change |
|------|--------|
| `toonflow_generate_voice` | add `provider`, `speech_rate`, `ref_audio_url`, `instruction` params; return a `budget` field |
| `toonflow_seed_voice` *(new)* | `{project, character, seed_model?, gender?/pitch?/speed?}` → seed + clone-ready spec |
| `toonflow_set_character_voice` *(new)* | `{project, character, ref_audio_url, prompt_text, model?}` |
| `toonflow_list_character_voices` *(new)* | `{project}` → all character voice specs |
| `speak` (optional parity) | expose `provider: async` models via `Exhub.TTS.Async` |

Pipeline: an optional `voices_seed` stage (before `voices`); `resume: true` skips
characters that already have a voice spec.

## 9. Tests

- `test/exhub/tts/async_test.exs` — payload builders + submit/poll with a mox/stub
  HTTP (no network).
- `test/exhub/toonflow/voice_test.exs` — extend: `voice_spec` precedence (D7),
  client routing (A2), `speech_budget/2`, clone `prompt_audio_url` pass-through.
- `test/exhub/toonflow/voice_ref_test.exs` — `expired?/2`, `merge_spec/2` (pure).
- Run: `MIX_ENV=test mix test --no-start test/exhub/tts/ test/exhub/toonflow/`.

## 10. Rollout

1. `MIX_ENV=prod mix release --overwrite`
2. `exhub_hot_reload` (code only).
3. `bin/exhub rpc 'Application.put_env(:exhub, :toonflow, …)'` for the new
   `media.tts_model` / `voice_map` defaults (hot reload does not re-read sys.config).
4. Verify: `toonflow_generate_voice` on a seeded character; re-check
   `toonflow_pipeline_plan`.

## 11. Risks & open items

- **Qwen3-TTS async schema** (`inputs.prompt`) not yet pinned → ship disabled.
- **URL expiry (~7d)** → local `ref_path` is authoritative; `refresh/3` re-seeds.
  A permanent host (Gitee raw URL via the existing `GITEE_TOKEN`) is an optional
  path C for long-running series.
- **Clone consistency** depends on reference quality: 5–15 s, clean, standard
  Mandarin, and `prompt_text` **verbatim** matching the clip.
- **One model per character** — do not mix `IndexTTS-2` / `GLM-TTS` on one voice.
- **No emotion control on MoArk** — approximate with `speech_rate` / Spark
  `pitch`/`speed`.
- **Lip-sync blocked** — `InfiniteTalk` / `Duix-Avatar` are listed but unserved;
  revisit if MoArk opens them.
