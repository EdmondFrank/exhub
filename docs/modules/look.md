# exhub-look

The `exhub-look` module provides MCP-based image understanding using
[Gitee AI](https://moark.com) vision models.

## Setup

### Configuration

Set your Gitee AI API key:

```bash
mix scr.insert dev giteeai_api_key "your-api-key"
```

Get your API key from [Gitee AI](https://moark.com).

## Tool: `look`

Analyze images using AI vision models.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `image` | string | ✓ | — | Local file path or remote URL |
| `prompt` | string | ✓ | — | What to extract or analyze (optional in `text` mode) |
| `mode` | string | | `analyze` | `analyze` or `text` (aliases: `ocr`, `transcribe`) |
| `model` | string | | `deepseek-v4.1-flash` | Vision model to use |
| `response_format` | string | | `text` | `text` or `json` |
| `thinking` | boolean | | `true` (`analyze`) / `false` (`text`) | Enable the model's reasoning pass |
| `max_lines` | integer | | — | `text` mode: transcribe at most the last N visible lines |
| `only_new` | string | | — | `text` mode: previous transcript; unchanged lines are dropped |
| `expect` | string | | — | `text` mode: newline-separated substrings that must appear; appends `[expect: ok]` / `[expect: MISSING …]` |

## Mode: `text` (terminal / code / log screenshots)

`mode: "text"` is a transcription preset:

- prepends a strict transcription prompt (exact characters, no typo fixing, no
  Markdown, no code fences, no invented box borders) — a caller-supplied
  `prompt` is appended as an extra instruction instead of replacing it;
- disables the model's reasoning pass (`thinking: {type: "disabled"}`), which
  cut measured latency from 6.5 s to 3.5 s on a 1568×670 terminal screenshot;
- raises `max_tokens` to 8192 and pins `temperature: 0`;
- strips a wrapping ` ``` ` fence from the response, and appends a
  `[truncated: …]` marker if the model hits the token limit
  (`finish_reason: "length"`).

```json
{ "image": "~/Pictures/TerminalScreenshots/n/n1.jpg", "mode": "text" }
```

## Why not `doc_extract` for terminal screenshots

Measured on a set of real (watermarked, full-line) terminal screenshots:

| Method | Latency | Result |
|--------|---------|--------|
| `look` + `deepseek-v4.1-flash`, `mode: "text"` | 3.5–12 s | clean text grid, complete |
| `look` + `deepseek-v4.1-flash`, free-form prompt | 8 s | adds fake `┌─│┐` frames, 2.5× longer, can hit the token cap |
| `look` + `qwen3.5-122b-a10b` / `qwen3.8-omni-flash` / `kimi-k2.6` | 9–35 s | correct but slower |
| `look` + `minimax-m3.1-flash` | 40 s | empty response — avoid for transcription |
| `doc_extract` `Unlimited-OCR-2` | 140 s | fails: `OCR did not complete: length` |
| `doc_extract` `MinerU2.5-Pro` | slow | `\_`/`\$` escapes, ```` ```txt ```` fences, truncated |
| macOS Vision OCR (local, experiment) | 0.3 s | fast but garbled lines and digit errors (`9000`→`9008`) |

Document OCR reconstructs page layout and emits Markdown, which is the wrong
shape for monospaced full-width terminal lines; the vision model additionally
uses language context to repair characters the OCR pipeline misreads. Image
pre-processing (grayscale threshold, 2× upscale) did not improve results and is
not applied.

### Supported Models

| Model | Description |
|-------|-------------|
| `deepseek-v4.1-flash` | DeepSeek V4.1 Flash vision model (default) |
| `kimi-k2.6` | Moonshot AI vision model |
| `kimi-k2.5` | Moonshot AI vision model |
| `qwen3.5-122b-a10b` | Alibaba Qwen3.5 vision model |
| `qwen3.8-omni-flash` | Alibaba Qwen3.8 native omni-modal model (1M context) |
| `minimax-m3.1-flash` | MiniMax M3.1 Flash multimodal model (1M context); slow and can return an empty response — avoid for transcription |

### Supported Image Formats

PNG, JPG, JPEG, GIF, WebP, BMP

## Usage Examples

### Extract text from an image

```json
{
  "image": "/path/to/document.png",
  "prompt": "Extract all text from this image"
}
```

### Analyze image contents

```json
{
  "image": "https://example.com/photo.jpg",
  "prompt": "Describe what you see in this image in detail"
}
```

### Get structured JSON output

```json
{
  "image": "/path/to/form.png",
  "prompt": "Extract all fields and values as JSON",
  "response_format": "json"
}
```

### Use a specific model

```json
{
  "image": "/path/to/chart.png",
  "prompt": "Analyze this chart and summarize the data trends",
  "model": "qwen3.8-omni-flash"
}
```

## Model evaluation

All six models were benchmarked on a 16-case, ground-truth image suite
(OCR, chart/table reading, counting, spatial, structured JSON, arithmetic,
rotated/low-contrast text, hallucination resistance). Results and method:
`PKB/knowledge-base/exhub/exhub-look-model-evaluation.md` (assets in
`PKB/knowledge-base/exhub/look-eval/`).

`deepseek-v4.1-flash` remains the default — it scored the highest task
accuracy (16/16, including every hard case) and has the lowest image-token
cost. `qwen3.5-122b-a10b` is ~2× faster at 15/16 if latency matters more.

## Endpoint

MCP endpoint: `/look/mcp`
