# Tools Guide

## File Operations
- Use file_read to inspect files before editing
- Use file_write for creating new files
- Use file_edit for modifying existing files (find-replace)

## Shell
- Use shell for commands, builds, and tests
- Prefer non-destructive commands
- Be cautious with rm, overwrite, and network operations

## Web Content Fetching

When fetching web content or online content, use the standard tool discovery workflow:

**Usage workflow:**
1. Use retrieve_tools with query web fetch or web search to discover available tools
2. Call the identified tool using MCP Tool Calls format
3. Pass the target URL as specified in the tool schema

**Example:**
retrieve_tools with query web fetch url content
Returns: web:web_fetch, web:web_search, etc.
Call: web:web_fetch with url: https://example.com

This approach is preferred over direct HTTP requests as it provides consistent tool interface through MCP.

## Web Content Searching

When searching the web for information, prioritize using the exa web search tool.

Exa provides AI-powered web search with neural relevance ranking.

**Usage workflow:**
1. First, retrieve available tools using retrieve_tools with the query exa web search.
2. Call the specific tool using the MCP Tool Calls format.
3. Pass the parameters as specified in the tool schema.

## Image Reading

When you need to understand or analyze an online image, use the `look` tool via the standard tool discovery workflow.

**Usage workflow:**
1. Use retrieve_tools with query `look image read understand` to discover the tool.
2. Call `look:look` with the image URL and a prompt describing what to extract or analyze.
3. Supported image formats: PNG, JPG, JPEG, GIF, WebP, BMP.

**Example:**
retrieve_tools with query `look image read understand`
Returns: look:look
Call: look:look with image: https://example.com/photo.png, prompt: "Describe what's in this image"

The `look` tool supports multiple vision models (default: kimi-k2.6). Use the `model` parameter to switch models if needed.

## Audio Transcription

When you need to transcribe audio to text, use the `listen` tool via the standard tool discovery workflow.

**Usage workflow:**
1. Use retrieve_tools with query `listen audio transcribe speech` to discover the tool.
2. Call `listen:listen` with the audio file path.
3. Supported formats: MP3, WAV, M4A, FLAC, OGG, OPUS, WebM, MP4, AAC.

**Example:**
retrieve_tools with query `listen audio transcribe speech`
Returns: listen:listen
Call: listen:listen with file: "/path/to/audio.mp3"

The `listen` tool uses OpenAI-compatible speech-to-text API (default: whisper-large-v3-turbo). Use the `model` parameter to switch models, and `language` for a language hint (e.g. `zh`, `en`).
## MCP Tool Calls

### Pre-invocation Rule
Before invoking any tool, or if unsure about available tools, use `retrieve_tools` to obtain a list of usable tools.

Example `retrieve_tools` call:
```json
{
  "limit": 15,
  "query": "Gitee list pull requests issues"
}
```

### Format
```
server:tool_name
```

### Examples
- ✅ Correct: `gitee:gitee-v5#list_user_notifications`
- ❌ Wrong: `list_user_notifications` (missing server prefix)
- ❌ Wrong: `gitee-v5#list_user_notifications` (missing server prefix)

### Components
| Part | Description | Example |
|------|-------------|----------|
| **server** | MCP server name | `gitee`, `github`, `grafana` |
| **tool_name** | Full tool name (with version) | `gitee-v5#list_user_notifications` |

### Getting the Values
From `retrieve_tools` results:
```json
{
  "name": "gitee-v5#list_user_notifications",
  "server": "gitee",
  "score": 0.201
},
{
  "name": "Github-list_issues",
  "server": "github",
  "score": 0.036
}
```
→ Call with: `{"server": "gitee", "name": "gitee-v5#list_user_notifications"}` or `{"server": "github", "name": "Github-list_issues"}`

→ Note: *Must* strictly follow the return values of server and name for invocation.

## Advanced Tools Usage

When executing complex or large tasks, or receiving specific instructions, you can use your master's computer workstation through two tool suites: **Desktop** for filesystem/process control and **Mac Use** for native macOS app automation.

### Desktop Commander

Provides comprehensive filesystem, editing, process control, and search capabilities.

**Usage workflow:**
1. Use `retrieve_tools` with the query `desktop` to obtain the tools.
2. Follow the MCP Tool Calls format to invoke the retrieved tools.

**Tool categories:**

| Category | Tools |
|----------|-------|
| **Filesystem** | `read_file`, `read_multiple_files`, `write_file`, `create_directory`, `list_directory`, `move_file`, `get_file_info`, `delete_file` |
| **Editing** | `edit_block` — targeted find-and-replace within a text file |
| **Process / Terminal** | `execute_command`, `start_process`, `read_process_output`, `interact_with_process`, `list_managed_processes`, `list_processes`, `kill_process` |
| **Search** | `search_files` — search by filename or inside file contents |

### Mac Use

Provides Playwright/Puppeteer-style control over any macOS application using the Accessibility API. Explore UI trees, interact with elements, take screenshots, and automate native apps.

**Usage workflow:**
1. Use `retrieve_tools` with the query `mac-use` to obtain the tools.
2. Follow the MCP Tool Calls format to invoke the retrieved tools.

**Tool categories:**

| Category | Tools |
|----------|-------|
| **App Discovery** | `list_apps` — list running macOS apps visible to accessibility |
| **Inspection** | `snapshot` — print accessibility tree; `get_attribute` — get element attribute value |
| **Interaction** | `click`, `dblclick`, `input` (append text), `fill` (clear then type), `press` (key combos), `hover`, `focus` |
| **Scrolling** | `scroll_to` — scroll element into view; `scroll` — scroll within element (up/down/left/right) |
| **Screenshots** | `screenshot` — occlusion-proof capture with optional OCR |
| **Utility** | `wait` — wait for element or sleep; `activate` — bring app to foreground |

**Important considerations:**
- This is your master's workstation, so do not casually delete or modify files without explicit permission.

## Agent Collaboration Usage

When executing complex or large tasks, or when receiving specific instructions, you can collaborate with your master's computer agent through the `mcacp` tool suite, which provides access to available tools and guidance.

**Usage workflow:**
1. Use `retrieve_tools` with the query `agent` to obtain the necessary tools for your task.
2. Use the `desktop` tool to ensure that both you and the agent are working within the same desktop environment.
3. Follow the MCP Tool Calls format to invoke the retrieved tools.

## Knowledge Base Management

When receiving specific instructions, you can use the `retrieve_tools` with query `brain` to obtain the relevant tools needed to complete the task.
