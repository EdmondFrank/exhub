# Brain (Obsidian Vault) Integration

Exhub provides an MCP-based interface to an [Obsidian](https://obsidian.md/) vault, turning it into a queryable "second brain" for LLMs and MCP clients.

## Overview

The Brain integration exposes vault operations as MCP tools, allowing LLMs and other MCP clients to:

- List notes and directories in the vault (with optional recursion and absolute paths)
- Search notes by content, filename, or tags (with optional scoping and absolute paths)

## Configuration

Set the vault path in `config/config.exs` (or `config/runtime.exs`):

```elixir
config :exhub, :obsidian_vault_path, "~/Documents/Obsidian/MyVault"
```

Defaults to `~/Documents/Obsidian` if not set. Both `~` and `~/...` paths are expanded automatically.

## MCP Server

The Brain MCP server runs at the `/brain/mcp` endpoint.

**Module:** `Exhub.MCP.BrainServer`

## Available Tools

| Tool                 | Description                                                      |
|----------------------|------------------------------------------------------------------|
| `brain_list_notes`   | List notes and directories in the vault or a subfolder           |
| `brain_search_vault` | Search notes by content, filename, or tags                       |

---

### `brain_list_notes`

List entries (notes and directories) in the vault, optionally scoped to a subfolder.

**Parameters:**

| Parameter   | Type    | Default | Description                                              |
|-------------|---------|---------|----------------------------------------------------------|
| `folder`    | string  | —       | Subfolder path relative to vault root (optional)         |
| `recursive` | boolean | `true`  | Recurse into subdirectories                              |
| `abs_path`  | boolean | `false` | Return absolute filesystem paths instead of relative     |

**Response format:**
- Directories are indicated with a trailing `/`
- Within each level, directories are listed before files, both sorted alphabetically
- Response always starts with `Vault: <path>` so the caller knows the vault root

**Examples:**

```json
// List everything recursively
{}

// List a specific folder, non-recursively
{ "folder": "journal/2024", "recursive": false }

// Return absolute paths
{ "folder": "projects", "abs_path": true }
```

---

### `brain_search_vault`

Search for notes by content, filename, or tags.

**Parameters:**

| Parameter        | Type    | Default      | Description                                                       |
|------------------|---------|--------------|-------------------------------------------------------------------|
| `query`          | string  | *(required)* | Search query. Prefix with `tag:` for tag search (e.g. `tag:work`) |
| `path`           | string  | —            | Limit search to this subfolder (optional)                         |
| `search_type`    | string  | `"content"`  | `"content"`, `"filename"`, or `"both"`                            |
| `case_sensitive` | boolean | `false`      | Enable case-sensitive matching                                    |
| `abs_path`       | boolean | `false`      | Return absolute filesystem paths instead of relative              |

**Search types:**

| Type       | Behaviour                                      |
|------------|------------------------------------------------|
| `content`  | Full-text search within note bodies (default)  |
| `filename` | Match against note filenames / relative paths  |
| `both`     | Union of content and filename results          |

**Tag search:**
Prefix the query with `tag:` to search for inline `#tags` and frontmatter tags.
Hierarchical tags are supported — `tag:project` also matches `#project/active`.

**Response format:**
- Response always starts with `Vault: <path>`
- Each matching file is listed with its matching lines (`L<n>: <text>`)
- Filename matches are shown as `Filename match: <path>`

**Examples:**

```json
// Content search
{ "query": "meeting notes" }

// Filename search
{ "query": "journal", "search_type": "filename" }

// Tag search (hierarchical)
{ "query": "tag:project" }

// Scoped search with absolute paths
{ "query": "TODO", "path": "projects", "abs_path": true }

// Case-sensitive search
{ "query": "TODO", "case_sensitive": true }
```

---

## Architecture

```
Exhub.MCP.BrainServer          ← Anubis MCP server, mounted at /brain/mcp
├── Exhub.MCP.Tools.Brain.ListNotes    ← brain_list_notes tool
├── Exhub.MCP.Tools.Brain.SearchVault  ← brain_search_vault tool
└── Exhub.MCP.Brain.Helpers            ← shared vault path & file utilities
```

## See Also

- [Obsidian](https://obsidian.md/)
- [MCP Documentation](https://modelcontextprotocol.io/)
