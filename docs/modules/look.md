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
| `prompt` | string | ✓ | — | What to extract or analyze |
| `model` | string | | `deepseek-v4.1-flash` | Vision model to use |
| `response_format` | string | | `text` | `text` or `json` |

### Supported Models

| Model | Description |
|-------|-------------|
| `deepseek-v4.1-flash` | DeepSeek V4.1 Flash vision model (default) |
| `kimi-k2.6` | Moonshot AI vision model |
| `kimi-k2.5` | Moonshot AI vision model |
| `qwen3.5-122b-a10b` | Alibaba Qwen3.5 vision model |
| `qwen3.8-omni-flash` | Alibaba Qwen3.8 native omni-modal model (1M context) |

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

## Endpoint

MCP endpoint: `/look/mcp`
