# exhub-doc-extract

The `exhub-doc-extract` module provides MCP-based document text extraction using the
[Gitee AI](https://ai.gitee.com) Async Document Parse API powered by PaddleOCR-VL-1.5.

## Setup

### Configuration

The doc extract server requires a Gitee AI API key stored in SecretVault:

```bash
mix scr.insert dev giteeai_api_key "your-gitee-ai-api-key"
```

> If you already configured `giteeai_api_key` for `exhub-web-tools` or `exhub-image-gen`,
> no additional setup is needed — the same key is shared.

The server starts automatically with the Exhub application.

## MCP Endpoint

```
POST /doc-extract/mcp
```

## Tool: `doc_extract`

Extract and recognize text from documents (PDF, DOCX, images, etc.) using Gitee AI.

Powered by PaddleOCR-VL-1.5 via the Gitee AI Async Document Parse API.
Supports both local file paths and remote URLs as input.

The tool submits an async parsing task, waits for completion, then returns
the extracted content in Markdown format.

**Supported input formats:** PDF, DOCX, DOC, PNG, JPG, JPEG, TIFF, BMP, GIF, WEBP, etc.

**Returns:** Extracted text in Markdown format, with layout and structure preserved.

### Parameters

| Parameter       | Type    | Required | Default | Description                                              |
|-----------------|---------|----------|---------|----------------------------------------------------------|
| `file`          | string  | ✓        | —       | Path to a local document file or a remote http/https URL |
| `include_image` | boolean |          | `true`  | Whether to include image references in the output        |
| `output_format` | string  |          | `md`    | Output format: `md` for Markdown, `text` for plain text  |

### How It Works

The extraction is asynchronous:

1. **Submit** — The tool uploads the document (or sends the URL) to the Gitee AI Async Document Parse API
2. **Poll** — It polls the task status endpoint every 5 seconds
3. **Return** — Once complete, it returns the full extracted text

**Timeouts:**
- Maximum polling time: 5 minutes (60 attempts × 5 seconds)
- MCP transport timeout: 300 seconds

### Response Format

The tool returns the extracted text directly in Markdown (or plain text) format,
with layout and structure preserved.

Display the result as: 📖[Extracted Document Content]

## Usage Examples

### Extract from a local PDF

```json
{
  "file": "/path/to/document.pdf"
}
```

### Extract from a remote URL

```json
{
  "file": "https://example.com/report.pdf"
}
```

### Plain text output (no Markdown formatting)

```json
{
  "file": "/path/to/contract.docx",
  "output_format": "text",
  "include_image": false
}
```

## Notes

- **Large documents** may take longer to process; the 5-minute timeout should accommodate most files
- **Remote URLs** are downloaded server-side before processing
- **Image references** in the output are controlled by `include_image` (default: true)
- The default model `PaddleOCR-VL-1.5` provides high-quality OCR with layout preservation
