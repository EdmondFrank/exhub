# exhub-image-gen

The `exhub-image-gen` module provides MCP-based AI image generation using the
[Gitee AI](https://ai.gitee.com) image generation API (OpenAI-compatible).

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
| `model`               | string  |          | `Qwen-Image`                     | Model to use (see table below)                                           |
| `size`                | string  |          | `1024x1024`                      | Output image dimensions (see sizes below)                                |
| `negative_prompt`     | string  |          | Standard quality negative prompt | Elements to avoid. Not supported by `Kolors`.                            |
| `guidance_scale`      | float   |          | Model default                    | How closely the model follows the prompt. Not supported by `Qwen-Image`. |
| `num_inference_steps` | integer |          | Model default                    | Denoising steps. Higher = better quality but slower.                     |

### Supported Models

| Model                              | `negative_prompt` | `guidance_scale` | `num_inference_steps` | Default steps | Default scale |
|------------------------------------|:-----------------:|:----------------:|:---------------------:|:-------------:|:-------------:|
| `Qwen-Image` *(default)*           | ✓                 | —                | ✓                     | 30            | —             |
| `Qwen-Image-2512`                  | ✓                 | —                | ✓                     | 30            | —             |
| `Kolors`                           | —                 | ✓                | ✓                     | 25            | 7.5           |
| `GLM-Image`                        | ✓                 | ✓                | ✓                     | 30            | 1.5           |
| `FLUX.1-schnell`                   | —                 | ✓                | ✓                     | 4             | 0.0           |
| `FLUX.1-dev`                       | ✓                 | ✓                | ✓                     | 28            | 3.5           |
| `FLUX.1-Krea-dev`                  | ✓                 | ✓                | ✓                     | 28            | 3.5           |
| `FLUX.2-dev`                       | ✓                 | ✓                | ✓                     | 20            | 7.5           |
| `FLUX.2-klein-9B`                  | —                 | ✓                | ✓                     | 8             | 3.5           |
| `FLUX.2-klein-4B`                  | —                 | ✓                | ✓                     | 8             | 3.5           |
| `HunyuanDiT-v1.2`                  | ✓                 | ✓                | ✓                     | 25            | 5.0           |
| `stable-diffusion-xl-base-1.0`     | ✓                 | ✓                | ✓                     | 30            | 7.5           |
| `stable-diffusion-3.5-large-turbo` | ✓                 | ✓                | ✓                     | 8             | 1.0           |
| `stable-diffusion-3-medium`        | ✓                 | ✓                | ✓                     | 28            | 7.0           |
| `CogView4-6B`                      | ✓                 | ✓                | ✓                     | 50            | 7.5           |
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

### Response Format

```json
{
  "image_url": "https://ai.gitee.com/...",
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

## Quality Tuning Tips

| Problem                              | Solution                                      |
|--------------------------------------|-----------------------------------------------|
| Image quality is low or lacks detail | Increase `num_inference_steps` (e.g. 25 → 35) |
| Image ignores prompt details         | Increase `guidance_scale` (e.g. 7.5 → 15)     |
| Image is oversaturated or distorted  | Decrease `guidance_scale`                     |
| Need a specific aspect ratio         | Choose the appropriate `size`                 |
| Want to avoid specific elements      | Use `negative_prompt` (where supported)       |
