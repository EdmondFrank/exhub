# exhub-image-gen

The `exhub-image-gen` module provides MCP-based AI image generation using the
[Gitee AI](https://moark.com) image generation API (OpenAI-compatible).

## Setup

### Configuration

The image gen server requires a Gitee AI API key stored in SecretVault:

```bash
mix scr.insert dev giteeai_api_key "your-gitee-ai-api-key"
```

> If you already configured `giteeai_api_key` for `exhub-web-tools`, no additional
> setup is needed — the same key is shared.

The server starts automatically with the Exhub application.

## MCP Endpoint

```
POST /image-gen/mcp
```

## Tool: `image_gen`

Generate a high-quality image from a text description.

### Parameters

| Parameter             | Type    | Required | Default                          | Description                                                              |
|-----------------------|---------|----------|----------------------------------|--------------------------------------------------------------------------|
| `prompt`              | string  | ✓        | —                                | Text description of the image to generate. Be specific and detailed.     |
| `model`               | string  |          | `qwen-image-2.0`                 | Model to use (see table below)                                           |
| `size`                | string  |          | `1024x1024`                      | Output image dimensions (see sizes below)                                |
| `negative_prompt`     | string  |          | Standard quality negative prompt | Elements to avoid. Not supported by `Kolors`. Sent top-level for WAN models. |
| `guidance_scale`      | float   |          | Model default                    | How closely the model follows the prompt. Not supported by `Qwen-Image`. |
| `num_inference_steps` | integer |          | Model default                    | Denoising steps. Higher = better quality but slower.                     |
| `n`                   | integer |          | `1`                              | Number of images to generate (1-4). WAN models only.                    |
| `seed`                | integer |          | `0` (WAN models)                 | Random seed for reproducible generation. WAN models only.               |
| `prompt_extend`       | boolean |          | `true` (WAN models)              | Auto-enhance the prompt using AI. WAN models only.                      |

### Supported Models

| Model                              | `negative_prompt` | `guidance_scale` | `num_inference_steps` | Default steps | Default scale |
|------------------------------------|:-----------------:|:----------------:|:---------------------:|:-------------:|:-------------:|
| `qwen-image-2.0` *(default)*       | ✓                 | —                | ✓                     | 30            | —             |
| `qwen-image-2.0-pro`               | ✓                 | —                | ✓                     | 30            | —             |
| `Qwen-Image`                       | ✓                 | —                | ✓                     | 30            | —             |
| `Qwen-Image-2512`                  | ✓                 | —                | ✓                     | 30            | —             |
| `Qwen-Image-Layered`               | ✓                 | —                | ✓                     | 30            | —             |
| `wan2.7-image`                     | ✓                 | ✓                | ✓                     | 30            | 7.5           |
| `wan2.7-image-pro`                 | ✓                 | ✓                | ✓                     | 30            | 7.5           |
| `Kolors`                           | —                 | ✓                | ✓                     | 25            | 7.5           |
| `GLM-Image`                        | ✓                 | ✓                | ✓                     | 30            | 1.5           |
| `flux-1-schnell`                   | —                 | ✓                | ✓                     | 4             | 0.0           |
| `FLUX.1-dev`                       | ✓                 | ✓                | ✓                     | 28            | 3.5           |
| `FLUX_1-Krea-dev`                  | ✓                 | ✓                | ✓                     | 28            | 3.5           |
| `FLUX.1-Kontext-dev`               | ✓                 | ✓                | ✓                     | 28            | 3.5           |
| `FLUX.2-dev`                       | ✓                 | ✓                | ✓                     | 20            | 7.5           |
| `FLUX.2-klein-9B`                  | —                 | ✓                | ✓                     | 8             | 3.5           |
| `FLUX.2-klein-4B`                  | —                 | ✓                | ✓                     | 8             | 3.5           |
| `stable-diffusion-xl-base-1.0`     | ✓                 | ✓                | ✓                     | 30            | 7.5           |
| `stable-diffusion-3.5-large-turbo` | ✓                 | ✓                | ✓                     | 8             | 1.0           |
| `stable-diffusion-3-medium`        | ✓                 | ✓                | ✓                     | 28            | 7.0           |
| `CogView4_6B`                      | ✓                 | ✓                | ✓                     | 50            | 7.5           |
| `HiDream-I1-Full`                  | ✓                 | ✓                | ✓                     | 50            | 7.0           |
| `z-image-turbo`                    | —                 | ✓                | ✓                     | 8             | 3.5           |
| `Z-Image`                          | ✓                 | ✓                | ✓                     | 28            | 5.0           |
| `LongCat-Image`                    | ✓                 | ✓                | ✓                     | 28            | 5.0           |

### Supported Sizes

| Size                    | Aspect Ratio    |
|-------------------------|-----------------|
| `256x256`               | 1:1 small       |
| `512x512`               | 1:1             |
| `1024x1024` *(default)* | 1:1             |
| `1024x576`              | 16:9 landscape  |
| `576x1024`              | 9:16 portrait   |
| `1024x768`              | 4:3 landscape   |
| `768x1024`              | 3:4 portrait    |
| `1024x640`              | 16:10 landscape |
| `640x1024`              | 10:16 portrait  |
| `2048x2048`             | 1:1 high-res    |

> WAN models (`wan2.7-image`, `wan2.7-image-pro`) additionally support the size aliases `1K`, `2K`, and `4K`. When a WAN model is selected, pixel sizes (e.g. `1024x1024`) are automatically converted to the nearest alias (`≤1024 → 1K`, `≤2048 → 2K`, `>2048 → 4K`), and the default size is `2K`.

### Response Format

```json
{
  "image_url": "https://moark.com/...",
  "model": "Qwen-Image",
  "size": "1024x1024",
  "prompt": "a cat sitting on a mountain at sunset",
  "params": {
    "response_format": "url",
    "negative_prompt": "...",
    "num_inference_steps": 30
  }
}
```

Display the image with markdown: `![Generated Image](image_url)`

## Usage Examples

### Basic generation (default model)

```json
{
  "prompt": "a serene mountain lake at golden hour, photorealistic, 8k"
}
```

### Kolors with custom guidance

```json
{
  "prompt": "a futuristic city skyline at night, neon lights, cyberpunk style",
  "model": "Kolors",
  "size": "1024x576",
  "guidance_scale": 10.0,
  "num_inference_steps": 28
}
```

### FLUX.2-dev portrait

```json
{
  "prompt": "portrait of a young woman with flowing red hair, soft studio lighting, detailed",
  "model": "FLUX.2-dev",
  "size": "768x1024",
  "negative_prompt": "blurry, low quality, distorted face",
  "num_inference_steps": 30,
  "guidance_scale": 9.0
}
```

### WAN 2.7 with reproducible seed and prompt extension

```json
{
  "prompt": "a white siamese cat",
  "model": "wan2.7-image-pro",
  "size": "2K",
  "n": 1,
  "seed": 0,
  "prompt_extend": true,
  "negative_prompt": "不希望出现在图片中的内容。"
}
```

## Quality Tuning Tips

| Problem                              | Solution                                      |
|--------------------------------------|-----------------------------------------------|
| Image quality is low or lacks detail | Increase `num_inference_steps` (e.g. 25 → 35) |
| Image ignores prompt details         | Increase `guidance_scale` (e.g. 7.5 → 15)     |
| Image is oversaturated or distorted  | Decrease `guidance_scale`                     |
| Need a specific aspect ratio         | Choose the appropriate `size`                 |
| Want to avoid specific elements      | Use `negative_prompt` (where supported)       |
## Tool: `i2i`

Generate a new image guided by one or more existing images plus a text prompt
(image-to-image / image editing). Mirrors the GenClaw `i2i` tool
(`Exhub.Genclaw.Tools.I2I`) but is exposed as a standalone MCP tool on this
server, so it shares the `image_gen` model set, tuning params and response shape.

### Parameters

| Parameter               | Type            | Required | Default                          | Description                                                                  |
|-------------------------|-----------------|----------|----------------------------------|------------------------------------------------------------------------------|
| `image_path`            | string          | ✓        | —                                | Primary/source image. URL, base64 `data:` URI, or absolute / `~` file path.  |
| `prompt`                | string          | ✓        | —                                | Guidance for the resulting image.                                            |
| `reference_image_paths` | array of string |          | `[]`                             | Extra reference images for multi-image guidance (do not repeat `image_path`).|
| `model`                 | string          |          | `qwen-image-2.0`                 | Model to use (see below).                                                    |
| `size`                  | string          |          | `1024x1024`                      | Output image size (not sent for `qwen-image-2.0-pro`).                       |
| `negative_prompt`       | string          |          | Standard quality negative prompt | Elements to avoid.                                                           |
| `guidance_scale`        | float           |          | Model default                    | How closely the model follows the prompt.                                    |
| `num_inference_steps`   | integer         |          | Model default                    | Denoising steps.                                                             |
| `seed`                  | integer         |          | —                                | Random seed for reproducible generation.                                     |
| `quality`               | string          |          | —                                | Reserved. Currently has no effect.                                           |

### Image sources

`image_path` and every entry of `reference_image_paths` accept:

- a URL (`https://…`) — passed through (downloaded for the edits endpoint);
- a base64 `data:` URI — passed through;
- an absolute path or `~` shorthand — read and encoded (files over 2 MB are
  downscaled to 1280 px first).

### Models

| Model                        | Guidance                                                          |
|------------------------------|-------------------------------------------------------------------|
| `qwen-image-2.0` *(default)* | Multi-image guidance via `images: [...]` on `/v1/images/generations`. |
| `qwen-image-2.0-pro`         | Multi-image guidance.                                             |
| any other `image_gen` model  | Single-image edit via `/v1/images/edits` (primary image only).    |

Notes:

- `qwen-image-2.0-pro` rejects the `size` param on the generations endpoint
  (HTTP 400 `参数无效 'size'`), so `size` is omitted for it and the server
  default is used.
- Not every model supports the edits endpoint: Gitee AI currently accepts
  `gpt-image-2` and `FLUX.1-Kontext-dev` there; other models answer HTTP 400
  `暂不支持该接口`.

### Response format

Identical to `image_gen`:

```json
{
  "image_type": "url",
  "image_url": "https://moark.com/...",
  "image_b64": null,
  "model": "qwen-image-2.0",
  "size": "1024*1024",
  "prompt": "...",
  "params": {
    "negative_prompt": "...",
    "num_inference_steps": 30
  }
}
```

### Usage example

Transfer the outfit from a reference onto a subject:

```json
{
  "image_path": "/Users/me/Downloads/subject.png",
  "reference_image_paths": ["/Users/me/Downloads/outfit.png"],
  "prompt": "Keep the person and pose from the first image; dress them in the outfit from the second image; plain white studio background; photorealistic.",
  "model": "qwen-image-2.0"
}
```