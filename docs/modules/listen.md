# exhub-listen

The `exhub-listen` module provides MCP-based audio transcription using
[Gitee AI](https://ai.gitee.com) / [moark.com](https://moark.com) speech-to-text API.

## Setup

### Configuration

Set your Gitee AI API key (shared with `look` module):

```bash
mix scr.insert dev giteeai_api_key "your-api-key"
```

Get your API key from [moark.com](https://moark.com) (工作台 → 设置 → 访问令牌).

## Tool: `listen`

Transcribe audio files to text using AI speech recognition.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `file` | string | ✓ | — | Absolute path to audio file or `~` shorthand |
| `model` | string | | `whisper-large-v3-turbo` | Speech recognition model |
| `language` | string | | — | Language hint (e.g. `zh`, `en`) |

### Supported Models

| Model | Description |
|-------|-------------|
| `whisper-large-v3-turbo` | OpenAI Whisper multilingual real-time recognition (default) |
| `whisper-large-v3` | Whisper standard |

### Supported Audio Formats

MP3, WAV, M4A, FLAC, OGG, OPUS, WebM, MP4, AAC, MPEG, OGA

## Usage Examples

### Basic transcription

```json
{
  "file": "/path/to/audio.mp3"
}
```

### With language hint

```json
{
  "file": "~/Downloads/recording.wav",
  "language": "zh"
}
```

### Use a specific model

```json
{
  "file": "/path/to/meeting.mp3",
  "model": "whisper-large-v3"
}
```

## Endpoint

MCP endpoint: `/listen/mcp`

## Notes

- Uses OpenAI-compatible `/v1/audio/transcriptions` endpoint
- Audio files are uploaded as multipart/form-data
- 1-hour audio takes approximately 20 seconds to transcribe
- The `TeleASR-MultiDialect` model (Chinese + English + 60 dialects) is listed in the API docs but currently returns "暂不支持该接口" at this endpoint
