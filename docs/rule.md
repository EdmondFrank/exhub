### Token Management
**RTK - Rust Token Killer**: Always prefix shell commands with `rtk` to minimize token consumption.

Examples:
- `rtk git status`
- `rtk go build`
- `rtk ls src/`
- `rtk grep "pattern" src/`
- `rtk find "*.rs" .`
- `rtk docker ps`
- `rtk gh pr list`
- ... and any other command

**Meta Commands**:
- `rtk gain` - Show token savings
- `rtk gain --history` - Command history with savings
- `rtk discover` - Find missed RTK opportunities
- `rtk proxy <cmd>` - Run raw (no filtering, for debugging)

Always use `rtk <cmd>` instead of raw commands. RTK filters and compresses command output before it reaches the LLM context, saving 60-90% tokens on common operations.

### MCP Tool Calls

**Pre-invocation Rule**
Before invoking any tool you don’t have, or if you’re unsure about which tools are available, use retrieve_tools to obtain a list of usable external tools.

Example `retrieve_tools` call:
```json
{
"limit": 15,
"query": "Gitee list pull requests issues"
}
```

**Format**
```
server:tool_name
```

Examples
- ✅ Correct: `gitee:gitee-v5#list_user_notifications`
- ❌ Wrong: `list_user_notifications` (missing server prefix)
- ❌ Wrong: `gitee-v5#list_user_notifications` (missing server prefix)

**Getting the Values**
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

### MCP Servers Index

#### Built-in Servers

| Name          | Module                         | Description                                      |
|---------------|--------------------------------|--------------------------------------------------|
| `time`        | `Exhub.MCP.TimeServer`         | Time-related functionality                       |
| `web-tools`   | `Exhub.MCP.WebToolsServer`     | Web search and web fetch                         |
| `archery`     | `Exhub.MCP.ArcheryServer`      | Archery SQL audit platform integration           |
| `browser-use` | `Exhub.MCP.BrowserUseServer`   | Browser automation via kuri-agent CDP            |
| `image-gen`   | `Exhub.MCP.ImageGenServer`     | AI image generation via Gitee AI                 |
| `doc-extract` | `Exhub.MCP.DocExtractServer`   | Document text extraction via Gitee AI            |
| `look`        | `Exhub.MCP.LookServer`         | Image understanding via Gitee AI vision models   |
| `todo`        | `Exhub.MCP.TodoServer`         | Todo/task list tracking across conversations     |
| `agent`       | `Exhub.MCP.AgentServer`        | ACP agent management (Claude Code, Gemini, etc.) |
| `brain`       | `Exhub.MCP.BrainServer`        | Obsidian brain vault operations                  |
| `exhub`       | `Exhub.MCP.ExhubServer`        | Exhub self-management (compile, reload, restart) |
| `mac-use`     | `Exhub.MCP.MacUseServer`       | macOS native app automation via axcli            |
