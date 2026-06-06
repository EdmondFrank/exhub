# Brain (Obsidian Vault) Integration

Exhub provides an MCP-based interface to an [Obsidian](https://obsidian.md/) vault, turning it into a queryable "second brain" for LLMs and MCP clients.

## Overview

The Brain integration exposes vault operations as MCP tools, allowing LLMs and other MCP clients to:

- List notes and directories in the vault (with optional recursion and absolute paths)
- Search notes by content, filename, or tags (with optional scoping and absolute paths)
- Create new notes with optional subfolder placement and initial content

## Configuration

Set the vault path in `config/config.exs` (or `config/runtime.exs`):

```elixir
config :exhub, :obsidian_vault_path, "~/Documents/Obsidian/MyVault"
```

Defaults to `~/Documents/Obsidian` if not set. Both `~` and `~/...` paths are expanded automatically.

### Gitignore Support

The Brain MCP tools automatically respect `.gitignore` files in your Obsidian vault. When a `.gitignore` file is present in the vault root, files and directories matching the patterns will be excluded from listing and search results.

**Features:**
- Supports standard gitignore patterns including wildcards (`*`, `**`, `?`)
- Supports negation patterns (`!pattern`) to un-ignore files
- Supports directory-only patterns (`pattern/`)
- Supports path-specific patterns (`dir/file.txt`)
- Automatically loads `.gitignore` from vault root
- Can be disabled via configuration

**Configuration:**

To disable gitignore support:

```elixir
config :exhub, :brain_gitignore_enabled, false
```

**Examples:**

If your `.gitignore` contains:

```
# Ignore all .tmp files
*.tmp

# Ignore secret directory
secret/

# But don't ignore important.tmp
!important.tmp
```

Then:
- `brain_list_notes` will not show `secret/` directory or `.tmp` files (except `important.tmp`)
- `brain_search_vault` will not search in ignored files

## MCP Server

The Brain MCP server runs at the `/brain/mcp` endpoint.

**Module:** `Exhub.MCP.BrainServer`

## Available Tools

| Tool                 | Description                                                      |
|----------------------|------------------------------------------------------------------|
| `brain_list_notes`   | List notes and directories in the vault or a subfolder           |
| `brain_search_vault` | Search notes by content, filename, or tags                       |
| `brain_create_note`  | Create a new note in the vault                                   |
| `brain_move_note`    | Move or rename a note within the vault                           |

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

### `brain_create_note`

Create a new note in the vault. Parent directories are created automatically.

By default, returns an error if the note already exists. Set `overwrite: true` to replace it.

**Parameters:**

| Parameter    | Type    | Default      | Description                                              |
|--------------|---------|--------------|----------------------------------------------------------|
| `filename`   | string  | *(required)* | Note name (`.md` added automatically if missing)         |
| `folder`     | string  | —            | Subfolder path relative to vault root (optional)         |
| `content`    | string  | `""`         | Initial note content                                     |
| `overwrite`  | boolean | `false`      | Allow replacing an existing note                         |

**Response format:**
- Response starts with `Vault: <path>`
- On success: `Created: <relative-path>`
- On failure: error message (e.g. note exists, path outside vault)

**Examples:**

```json
// Simple note at vault root
{ "filename": "ideas" }

// Note in subfolder (directories created automatically)
{ "filename": "weekly-review", "folder": "journal/2026" }

// With initial content
{ "filename": "todo", "folder": "projects", "content": "- [ ] Task 1\n- [ ] Task 2" }

// Overwrite existing note
{ "filename": "scratch", "overwrite": true, "content": "Fresh start" }
```

---

### `brain_move_note`

Move or rename a note within the vault. Parent directories for the destination
are created automatically.

**Parameters:**

| Parameter     | Type    | Default      | Description                                                  |
|---------------|---------|--------------|--------------------------------------------------------------|
| `source`      | string  | *(required)* | Current path relative to vault root (`.md` added if missing) |
| `destination` | string  | *(required)* | New path relative to vault root (`.md` added if missing)     |
| `overwrite`   | boolean | `false`      | Allow replacing an existing note at the destination          |

**Examples:**

```json
// Rename in place
{ "source": "drafts/idea", "destination": "projects/idea" }

// Move to different folder (directories created automatically)
{ "source": "old/meeting", "destination": "archive/2026/meeting" }

// Overwrite existing destination
{ "source": "tmp/notes", "destination": "inbox/notes", "overwrite": true }
```

---


## Architecture

```
Exhub.MCP.BrainServer          ← Anubis MCP server, mounted at /brain/mcp
├── Exhub.MCP.Tools.Brain.ListNotes    ← brain_list_notes tool
├── Exhub.MCP.Tools.Brain.SearchVault  ← brain_search_vault tool
├── Exhub.MCP.Tools.Brain.CreateNote   ← brain_create_note tool
├── Exhub.MCP.Tools.Brain.MoveNote     ← brain_move_note tool
└── Exhub.MCP.Brain.Helpers            ← shared vault path & file utilities
```

## See Also

- [Obsidian](https://obsidian.md/)
- [MCP Documentation](https://modelcontextprotocol.io/)
