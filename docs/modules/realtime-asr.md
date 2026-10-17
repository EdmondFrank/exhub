# exhub-realtime-asr

OpenAI-compatible audio transcription for MoArk (Gitee AI), including MoArk's
**WebSocket-only** real-time ASR models such as `TeleASR-MultiDialect`.

## Endpoint

```
POST /openai/v1/audio/transcriptions
```

`multipart/form-data`, OpenAI Whisper-compatible:

| Field | Type | Required | Default | Description |
|-------|------|----------|---------|-------------|
| `file` | file | ✓ | — | Audio file to transcribe |
| `model` | string | | `whisper-large-v3-turbo` | See models below |
| `language` | string | | — | Language hint (HTTP models only) |
| `response_format` | string | | `json` | `json`, `text` or `verbose_json` |
| `prompt` | string | | — | Accepted for compatibility (ignored) |
| `temperature` | number | | — | Accepted for compatibility (ignored) |

**Example**

```sh
curl http://localhost:9069/openai/v1/audio/transcriptions \
  -H "Authorization: Bearer anything" \
  -F file=@recording.m4a \
  -F model=TeleASR-MultiDialect
```

```json
{"text": "你好，这是一段测试音频。"}
```

The MoArk access token is injected by ExHub (the pooled `:giteeai_api_key`); the
client never needs a MoArk key.

## Models

| Model | Transport | Notes |
|-------|-----------|-------|
| `TeleASR-MultiDialect` | WebSocket | Chinese + English + 60 dialects; **only** reachable over the WS |
| `whisper-large-v3-turbo` | HTTP (proxied) | Default |
| `whisper-large-v3` | HTTP (proxied) | |
| `SenseVoiceSmall` | HTTP (proxied) | |
| `whisper-1` | HTTP (proxied) | Alias for `whisper-large-v3-turbo` |

## How it works

`Exhub.RealtimeASR.Handler` routes by model:

* **WebSocket models** — `Exhub.RealtimeASR.Transcriber` decodes the upload to
  raw `s16le` / 16 kHz / mono PCM with `ffmpeg` (`System.find_executable/1`), then
  `Exhub.RealtimeASR.Client` streams it to
  `wss://api.moark.com/v1/audio/transcriptions/ws` and returns the final
  transcript. ffmpeg is required for this path.
* **HTTP models** — the multipart body is forwarded to MoArk's
  `https://api.moark.com/v1/audio/transcriptions` with the pooled token.

The WebSocket protocol (MoArk borrows OpenAI Realtime *subprotocol names* but
uses its own message shape):

```
client → {"type":"start","model":…,"payload":{input,options}}
server → {"type":"started"}
client → raw binary PCM frames (s16le / 16 kHz / mono)
client → {"type":"finish"}
server → {"type":"output","payload":{"text":…,"is_final":bool}}   (repeated)
server → {"type":"completed"} | {"type":"error","payload":{"message":…}}
```

The connection is opened with `Mint` + `Mint.WebSocket` in passive mode, over
TLS options supplied by `Exhub.TLSCompat` (`api.moark.com` needs the
`key_usage_mismatch` override).

## Error responses

Errors use the OpenAI shape:

```json
{"error": {"message": "…", "type": "invalid_request_error"}}
```

| Status | Cause |
|--------|-------|
| `400` | Missing `file`, or an unsupported/unknown model |
| `500` | MoArk key not configured, unreadable upload, or ffmpeg missing |
| `502` | Upstream (MoArk WS/HTTP) failure |

## Tests

```sh
mix test --no-start test/exhub/realtime_asr/
```