# exhub-speak

The `exhub-speak` module provides MCP-based text-to-speech synthesis using
[Gitee AI](https://moark.com) / [moark.com](https://moark.com) **Qwen3-TTS**.

## Setup

### Configuration

Set your Gitee AI API key (shared with the `look` / `listen` / `image-gen` modules):

```bash
mix scr.insert dev giteeai_api_key "your-api-key"
```

Get your API key from [moark.com](https://moark.com) (工作台 → 设置 → 访问令牌).

## Tool: `speak`

Synthesize speech from text using Qwen3-TTS. Qwen3-TTS is **asynchronous** on
MoArk: the tool submits the text to `POST /v1/async/audio/speech`, polls
`GET /v1/task/{task_id}` until the audio is ready, then returns its URL (and
optionally saves the file locally).

The model caps each request at **150 characters**, so longer text is split into
sentence-aligned segments automatically; when `output` is set the segments are
concatenated into a single WAV file.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `text` | string | ✓ | — | Text to synthesize |
| `speaker` | string | | `Vivian` | Preset voice (voice-design mode) |
| `language` | string | | — | Language hint, e.g. `Chinese`, `English` |
| `instruction` | string | | — | Natural-language voice description (voice-design mode) |
| `ref_audio` | string | | — | Reference audio **URL** for zero-shot voice cloning |
| `ref_text` | string | | — | Transcript of `ref_audio`; required with `ref_audio` |
| `output_format` | string | | `mp3` | Audio format (only `mp3` is accepted by the API) |
| `output` | string | | — | Absolute / `~` path to save the audio file |
| `model` | string | | `Qwen3-TTS` | Speech synthesis model |
| `wait` | boolean | | `true` | Set `false` to submit and return the `task_id`(s) immediately |
| `task_id` | string | | — | Poll an existing task instead of submitting a new one |

### Modes

- **Voice design** — set `speaker` (and optionally `instruction`) to pick/design
  a voice, e.g. `speaker: "Vivian"` with
  `instruction: "体现撒娇稚嫩的萝莉女声"`.
- **Zero-shot voice clone** — set `ref_audio` (a reference audio URL) together
  with `ref_text` (its transcript). Only URLs are supported for `ref_audio`.

## Usage Examples

### Basic (voice design, saved locally)

```json
{
  "text": "你好，我是模力方舟的语音合成。",
  "speaker": "Vivian",
  "language": "Chinese",
  "output": "~/Downloads/speech.mp3"
}
```

### Custom voice instruction

```json
{
  "text": "各位听众，晚上好。",
  "instruction": "低沉磁性的男声，语速偏慢，富有故事感。"
}
```

### Zero-shot voice cloning

```json
{
  "text": "这是用参考音色合成的句子。",
  "ref_audio": "https://example.com/reference.wav",
  "ref_text": "这是参考音频对应的文字内容。"
}
```

### Submit without waiting

```json
{
  "text": "一段较长的文本……",
  "wait": false
}
```

Then poll the returned `task_id`:

```json
{
  "task_id": "1b0c1f9e6b0e4f62ab89f9f8e92f2c2a"
}
```

## Response

JSON with `audio_url` (first result), `audio_urls` (all results), `saved_path`
(when `output` was set), `segments` (number of synthesized chunks), `task_id` /
`task_ids`, `status`, `model`, `speaker`, `output_format` and `usage_info`.

## Endpoint

MCP endpoint: `/speak/mcp`

## Notes

- Uses MoArk's async Serverless API:
  `POST /v1/async/audio/speech` → `{"task_id": …}`, then
  `GET /v1/task/{task_id}` → `{"status": …, "output": {"result":
  [{"audio_urls": [{"url": …}]}]}}` (the generic `output.file_url` shape is
  also handled).
- Result download links are typically valid for 24 hours — save the file with
  `output` if you need it longer.
- Input is limited to 150 characters per request; longer text is auto-split at
  sentence boundaries and the per-segment audio is concatenated into one WAV.
- The model emits **WAV/PCM** audio (16-bit mono, 24 kHz) regardless of the
  `.mp3` URLs / `content_type`; save with a `.wav` extension.
- Transient upstream errors (`An unexpected error has occurred`, `Upstream
  returned an unparseable error response`) are retried automatically.
- Reuses the existing `giteeai_api_key` SecretVault entry.