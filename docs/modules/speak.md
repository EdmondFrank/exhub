# exhub-speak

The `exhub-speak` module provides MCP-based text-to-speech synthesis via
[Gitee AI](https://ai.gitee.com) / [moark.com](https://moark.com).

Three backends are available, selected with the `provider` parameter:

| Provider | Endpoint | Default model | Returns |
|----------|----------|---------------|---------|
| `realtime` (default) | `wss://api.moark.com/v1/audio/speech/ws` | `TeleTTS-Bidirectional-Mandarin` | WAV (PCM streamed, wrapped) |
| `sync` | `POST https://ai.gitee.com/v1/audio/speech` | `Step-Audio-TTS-3B` | audio bytes directly |
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

#### `realtime` (default) — `TeleTTS-Bidirectional-Mandarin`

Streams over MoArk's WebSocket speech endpoint
(`wss://api.moark.com/v1/audio/speech/ws`), implemented by
`Exhub.TTS.Realtime`. The server pushes raw PCM (24 kHz mono `s16le`) as base64
`output` frames; the tool accumulates them and wraps the result in a RIFF/WAVE
header, so the clip is saved as `.wav`.

Verified models (2026-10-25):

| Model | Voices | Notes |
|-------|--------|-------|
| `TeleTTS-Bidirectional-Mandarin` (default) | all 10 | |
| `TeleTTS-MultiDialect` | all 10 | answered `TELEAI_WS_CLOSED` (protocol error 1002) throughout the verification window |
| `TeleTTS-Mandarin` | none | rejects `voice` (`Parameter error`) — voiceless only |

Preset voices: `guangzhounv`, `shanghainv`, `chengdunv`, `zhengzhounv`,
`dongbeinv`, `changshanv`, `xiannv`, `xiamennv`, `joyce`, `lucas`. `volume`
(default 50) and `speech_rate` (default 1) tune the delivery; `sample_rate`
(default 24000) sets the output rate.

#### `sync` — `Step-Audio-TTS-3B`

Posts to the OpenAI-compatible `POST /v1/audio/speech` endpoint, which streams
the audio bytes straight back (no task/poll). Implemented by `Exhub.TTS.Sync`.

Verified models (2026-10-25):

| Model | Output | Notes |
|-------|--------|-------|
| `Step-Audio-TTS-3B` | MP3 | honours `voice` (`中文男` / `中文女`) |
| `ChatTTS` | WAV | fixed speaker — ignores `voice` |
| `CosyVoice2` | — | **no longer served** (answers `暂不支持该接口`) |
| `IndexTTS-2` | WAV | **clone only** — requires `prompt_audio_url` |
| `GLM-TTS` | WAV | **clone only** — requires `prompt_audio_url` |
| `TeleTTS-Mandarin` | — | rejects the OpenAI `voice` field (`参数无效 'voice'`) |

`CosyVoice2` was the default until 2026-10-25, when the endpoint stopped serving
it; `CosyVoice3` and `Qwen3-TTS` are also **not** served here (the API answers
`暂不支持该接口`) — use `provider: "async"` for the latter. The default moved to
`Step-Audio-TTS-3B` because it is the only non-cloning model that honours
`voice`, which multi-character scripts need.

The response `content-type` is unreliable — `CosyVoice2` advertised `audio/mp3`
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
| `text` | string | ✓ | — | all | Text to synthesize |
| `provider` | string | | `realtime` | all | `realtime`, `sync` or `async` |
| `model` | string | | `TeleTTS-Bidirectional-Mandarin` / `Step-Audio-TTS-3B` / `Qwen3-TTS` | all | Speech synthesis model |
| `voice` | string | | `guangzhounv` (realtime) / `alloy` (sync) | realtime, sync | Preset voice |
| `volume` | integer | | `50` | realtime | Output volume |
| `speech_rate` | number | | `1` | realtime | Speech rate |
| `sample_rate` | integer | | `24000` | realtime | Output sample rate (Hz) |
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

### Basic (realtime, TeleTTS, saved locally)

```json
{
  "text": "你好，我是模力方舟的语音合成。",
  "voice": "guangzhounv",
  "output": "~/Downloads/speech.wav"
}
```

### Sync (Step-Audio-TTS-3B)

```json
{
  "text": "你好，我是模力方舟的语音合成。",
  "provider": "sync",
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
  "provider": "sync",
  "model": "IndexTTS-2",
  "prompt_audio_url": "https://example.com/reference.wav",
  "prompt_text": "这是参考音频对应的文字内容。"
}
```

### Other sync models

```json
{ "text": "晚上好。", "provider": "sync", "model": "ChatTTS" }
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

**realtime** — JSON with `status`, `provider`, `model`, `voice`, `format`
(`wav`), `sample_rate`, `bytes` and `saved_path` (when `output` was set).

**sync** — JSON with `status`, `provider`, `model`, `voice`, `format`,
`content_type`, `bytes` and `saved_path` (when `output` was set).

**async** — JSON with `audio_url` (first result), `audio_urls` (all results),
`saved_path` (when `output` was set), `segments`, `task_id` / `task_ids`,
`status`, `model`, `speaker`, `output_format` and `usage_info`.

## Endpoint

MCP endpoint: `/speak/mcp`

## Notes

- **realtime** uses MoArk's streaming WebSocket endpoint
  (`wss://api.moark.com/v1/audio/speech/ws?model=…`); the access token travels in
  the `openai-insecure-api-key.<TOKEN>` subprotocol. The server streams raw PCM,
  which is wrapped as WAV.
- **realtime** serialises sessions: MoArk's TeleTTS backend serves one session
  at a time (a concurrent connection answers `TELEAI_WS_CLOSED`, protocol error
  1002), so `Exhub.TTS.Realtime` holds a global lock and parallel `speak` calls
  queue instead of failing.
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