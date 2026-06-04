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

### External MCP Servers Index

| Name        | Description                                                                       |
|-------------|-----------------------------------------------------------------------------------|
| time        | Time-related functionality                                                        |
| web-tools   | Web search and web fetch                                                          |
| archery     | Archery SQL audit platform integration                                            |
| browser-use | Browser automation via kuri-agent CDP                                             |
| image-gen   | AI image generation via Gitee AI                                                  |
| doc-extract | Document text extraction via Gitee AI                                             |
| todo        | Todo/task list tracking across conversations                                      |
| agent       | ACP agent management (Claude Code, Gemini, etc.)                                  |
| brain       | Obsidian brain vault operations                                                   |
| exhub       | Exhub self-management (compile, reload, restart)                                  |
| mac-use     | macOS native app automation via axcli                                             |
| amap        | Amap map and location services integration                                        |
| context7    | Pull up-to-date, version-specific documentation and code examples for any library |
| deepwiki    | Provides up-to-date documentation you can talk to, for every repo in the world    |
| github      | GitHub integration for pull requests, issues, and repository management           |
| gitee       | Gitee integration for pull requests, issues, and repository management            |
| search      | Web search and retrieval via integrated search engines                            |
