# exhub-speak

The `exhub-speak` module provides MCP-based text-to-speech synthesis via
[Gitee AI](https://ai.gitee.com) / [moark.com](https://moark.com).

Two backends are available, selected with the `provider` parameter:

| Provider | Endpoint | Default model | Returns |
|----------|----------|---------------|---------|
| `sync` (default) | `POST https://ai.gitee.com/v1/audio/speech` | `CosyVoice2` | audio bytes directly |
| `async` | `POST /v1/async/audio/speech` + poll | `Qwen3-TTS` | audio URL after polling |

## Setup

### Configuration

Set your Gitee AI API key (shared with the `look` / `listen` / `image-gen` modules):

```bash
mix scr.insert dev giteeai_api_key "your-api-key"
```

Get your API key from [ai.gitee.com](https://ai.gitee.com) (工作台 → 访问令牌).

## Tool: `speak`

### Providers

#### `sync` (default) — `CosyVoice2`

Posts to the OpenAI-compatible `POST /v1/audio/speech` endpoint, which streams
the audio bytes straight back (no task/poll). Implemented by `Exhub.TTS.Sync`.

Verified models (2026-09):

| Model | Output | Notes |
|-------|--------|-------|
| `CosyVoice2` (default) | WAV | Chinese/English |
| `ChatTTS` | WAV | |
| `Step-Audio-TTS-3B` | MP3 | |
| `IndexTTS-2` | WAV | **clone only** — requires `prompt_audio_url` |
| `GLM-TTS` | WAV | **clone only** — requires `prompt_audio_url` |
| `TeleTTS-Mandarin` | — | requires a valid `voice` enum |

`CosyVoice3` and `Qwen3-TTS` are **not** served by this endpoint (the API answers
`暂不支持该接口`); use `provider: "async"` for those.

The response `content-type` is unreliable — `CosyVoice2` advertises `audio/mp3`
while emitting a RIFF/WAVE stream — so the real container is detected from the
payload's magic bytes and the saved file's extension is corrected accordingly
(`Exhub.TTS.Sync.detect_format/1` / `with_format/2`).

#### `async` (legacy) — `Qwen3-TTS`

The MoArk async flow: submit to `POST /v1/async/audio/speech`, poll
`GET /v1/task/{task_id}` until the audio is ready, then return its URL (and
optionally save it locally).

The model caps each request at **150 characters**, so longer text is split into
sentence-aligned segments automatically; when `output` is set the segments are
concatenated into a single WAV file. Two modes are supported per segment:
**voice design** (`speaker` + optional `instruction`) and **zero-shot clone**
(`ref_audio` + `ref_text`).

### Parameters

| Parameter | Type | Required | Default | Provider | Description |
|-----------|------|----------|---------|----------|-------------|
| `text` | string | ✓ | — | both | Text to synthesize |
| `provider` | string | | `sync` | both | `sync` or `async` |
| `model` | string | | `CosyVoice2` / `Qwen3-TTS` | both | Speech synthesis model |
| `voice` | string | | `alloy` | sync | Preset voice |
| `prompt_audio_url` | string | | — | sync | Reference audio URL (clone models) |
| `prompt_text` | string | | — | sync | Transcript of `prompt_audio_url` |
| `output` | string | | — | both | Absolute / `~` path to save the audio |
| `speaker` | string | | `Vivian` | async | Preset voice (voice-design mode) |
| `language` | string | | — | async | Language hint, e.g. `Chinese`, `English` |
| `instruction` | string | | — | async | Natural-language voice description |
| `ref_audio` | string | | — | async | Reference audio **URL** for zero-shot cloning |
| `ref_text` | string | | — | async | Transcript of `ref_audio`; required with `ref_audio` |
| `output_format` | string | | `mp3` | async | Audio format (only `mp3` is accepted by the API) |
| `wait` | boolean | | `true` | async | `false` submits and returns the `task_id`(s) immediately |
| `task_id` | string | | — | async | Poll an existing task instead of submitting a new one |

## Usage Examples

### Basic (sync, CosyVoice2, saved locally)

```json
{
  "text": "你好，我是模力方舟的语音合成。",
  "voice": "alloy",
  "output": "~/Downloads/speech.mp3"
}
```

The file is written as `~/Downloads/speech.wav` (the extension is corrected to
the detected container).

### Sync voice cloning (IndexTTS-2 / GLM-TTS)

```json
{
  "text": "这是用参考音色合成的句子。",
  "model": "IndexTTS-2",
  "prompt_audio_url": "https://example.com/reference.wav",
  "prompt_text": "这是参考音频对应的文字内容。"
}
```

### Other sync models

```json
{ "text": "晚上好。", "model": "ChatTTS" }
```

### Async (legacy Qwen3-TTS)

```json
{
  "text": "你好，我是模力方舟的语音合成。",
  "provider": "async",
  "speaker": "Vivian",
  "language": "Chinese",
  "output": "~/Downloads/speech.mp3"
}
```

Submit without waiting and poll the returned `task_id`:

```json
{ "text": "一段较长的文本……", "provider": "async", "wait": false }
```

```json
{ "task_id": "1b0c1f9e6b0e4f62ab89f9f8e92f2c2a" }
```

## Response

**sync** — JSON with `status`, `provider`, `model`, `voice`, `format`,
`content_type`, `bytes` and `saved_path` (when `output` was set).

**async** — JSON with `audio_url` (first result), `audio_urls` (all results),
`saved_path` (when `output` was set), `segments`, `task_id` / `task_ids`,
`status`, `model`, `speaker`, `output_format` and `usage_info`.

## Endpoint

MCP endpoint: `/speak/mcp`

## Notes

- **sync** uses Gitee AI's OpenAI-compatible endpoint:
  `POST /v1/audio/speech` with `{"model", "input", "voice", "prompt_audio_url",
  "prompt_text"}`.
- **async** uses MoArk's async Serverless API:
  `POST /v1/async/audio/speech` → `{"task_id": …}`, then
  `GET /v1/task/{task_id}` → `{"status": …, "output": {"result":
  [{"audio_urls": [{"url": …}]}]}}` (the generic `output.file_url` shape is
  also handled).
- Result download links are typically valid for 24 hours — save the file with
  `output` if you need it longer.
- Async input is limited to 150 characters per request; longer text is auto-split
  at sentence boundaries and the per-segment audio is concatenated into one WAV.
- CosyVoice2 pads the clip with leading/trailing silence; trim it
  (`silenceremove`) if you need tight timing.
- Transient async upstream errors (`An unexpected error has occurred`, `Upstream
  returned an unparseable error response`) are retried automatically.
- Reuses the existing `giteeai_api_key` SecretVault entry.