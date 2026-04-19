# exhub-look

The `exhub-look` module provides MCP-based image understanding using
[Gitee AI](https://ai.gitee.com) vision models.

## Setup

### Configuration

Set your Gitee AI API key:

```bash
mix scr.insert dev giteeai_api_key "your-api-key"
```

Get your API key from [Gitee AI](https://ai.gitee.com).

## Tool: `look`

Analyze images using AI vision models.

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `image` | string | ✓ | — | Local file path or remote URL |
| `prompt` | string | ✓ | — | What to extract or analyze |
| `model` | string | | `glm-5v-turbo` | Vision model to use |
| `response_format` | string | | `text` | `text` or `json` |

### Supported Models

| Model | Description |
|-------|-------------|
| `glm-5v-turbo` | Zhipu AI efficient vision model (default) |
| `kimi-k2.5` | Moonshot AI vision model |
| `qwen3.5-122b-a10b` | Alibaba Qwen3.5 vision model |
| `qwen2.5-vl-32b-instruct` | Alibaba Qwen2.5-VL 32B |
| `glm-4.6v` | Zhipu AI GLM-4.6V vision model |
| `qwen2-vl-72b` | Alibaba Qwen2-VL 72B |

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
  "model": "qwen2.5-vl-32b-instruct"
}
```

## Endpoint

MCP endpoint: `/look/mcp`
