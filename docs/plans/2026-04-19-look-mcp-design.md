# Look MCP Server Design

## Overview

A new MCP server that provides image understanding capabilities using Gitee AI's vision models. The `look` tool accepts an image (local path or remote URL) and a prompt, returning extracted/analyzed information.

## Architecture

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│   MCP Client    │────▶│   LookServer     │────▶│   Gitee AI API  │
│  (Claude/etc)   │     │   /look/mcp      │     │  /v1/chat/...   │
└─────────────────┘     └──────────────────┘     └─────────────────┘
                               │
                               ▼
                        ┌──────────────────┐
                        │   Tools.Look     │
                        │   - image input  │
                        │   - prompt       │
                        │   - model select │
                        └──────────────────┘
```

**Pattern:** Follows existing `ImageGenServer` + `Tools.ImageGen` structure.

## Tool: `look`

### Parameters

| Parameter | Type | Required | Default | Description |
|-----------|------|----------|---------|-------------|
| `image` | string | ✓ | — | Local path (`/path/to/img.png`) or URL (`https://...`) |
| `prompt` | string | ✓ | — | What to extract/analyze from the image |
| `model` | string | | `glm-5v-turbo` | Vision model to use |
| `response_format` | string | | `text` | `text` or `json` |

### Supported Models

- `glm-5v-turbo` (default) - Zhipu AI efficient vision model
- `Qwen2.5-VL-72B-Instruct` - Alibaba's flagship vision model
- `Qwen2-VL-72B-Instruct` - Previous generation
- `GLM-4V` - Zhipu AI vision model
- `deepseek-vl` - DeepSeek vision

## Image Input Handling

### Remote URLs
Pass directly as `image_url`:
```elixir
%{type: "image_url", image_url: %{url: "https://example.com/image.png"}}
```

### Local Files
Read file, base64 encode, pass as data URI:
```elixir
%{type: "image_url", image_url: %{url: "data:image/png;base64,iVBORw0..."}}
```

**Supported formats:** png, jpg, jpeg, gif, webp, bmp

## API Request Format

```elixir
POST https://ai.gitee.com/v1/chat/completions
Headers: Authorization: Bearer {api_key}

{
  "model": "glm-5v-turbo",
  "messages": [
    {
      "role": "user",
      "content": [
        {"type": "text", "text": "Extract all text from this image"},
        {"type": "image_url", "image_url": {"url": "data:image/png;base64,..."}}
      ]
    }
  ],
  "max_tokens": 4096
}
```

## Response Format

### Text Mode (default)
Returns the model's text response directly.

### JSON Mode
When `response_format: "json"`, adds `response_format: {"type": "json_object"}` to request.
The model returns valid JSON. User's prompt should request JSON output.

## Files to Create

| File | Purpose |
|------|---------|
| `lib/exhub/mcp/look_server.ex` | MCP server using Anubis.Server |
| `lib/exhub/mcp/tools/look.ex` | Tool implementation |
| `docs/modules/look.md` | User documentation |

## Error Handling

- Missing `giteeai_api_key` → "Gitee AI API key not configured. Run: mix scr.insert dev giteeai_api_key \"your-key\""
- Invalid image path → "File not found: {path}"
- Unsupported format → "Unsupported image format: {ext}"
- Remote URL fetch failure → "Failed to fetch remote image: {reason}"
- API error → Propagate HTTP status and message

## Configuration

Uses existing `giteeai_api_key` from application config:
```elixir
api_key = Application.get_env(:exhub, :giteeai_api_key, "")
```

## Route

Server accessible at: `/look/mcp`
