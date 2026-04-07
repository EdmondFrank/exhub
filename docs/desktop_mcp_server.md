# Desktop MCP Server

## Overview

The Desktop MCP Server provides a set of MCP (Model Context Protocol) tools that give an LLM agent direct access to the local filesystem and process management. These tools enable reading, writing, searching, and manipulating files and directories on the host system.

All tool responses are TOON-encoded (not JSON) to reduce token consumption by 30–60%, while maintaining full readability and semantic equivalence. If TOON encoding fails for any reason, the system automatically falls back to JSON encoding.

## Architecture

- **Tool modules**: All tools live under `Exhub.MCP.Tools.Desktop.*`
- **Shared helpers**: `Exhub.MCP.Desktop.Helpers` provides common utilities:
  - `toon_response/2` — Encodes a map as TOON text and adds it to a tool response
  - `expand_path/1` — Resolves `~` and `~/...` to the user home directory; passes other paths through unchanged
- **Tool structure**: Each tool uses `use Anubis.Server.Component, type: :tool` and implements:
  - `name/0` — Returns the tool name string
  - `description/0` — Returns a detailed description string
  - `schema/1` — Defines parameter schema with types, defaults, and descriptions
  - `execute/2` — Executes the tool logic, returning `{:reply, resp, frame}`

## Tools Reference

### read_file

Reads the contents of a file from the filesystem. Supports both plain text files and document files (PDF, DOCX, images) with automatic text extraction.

**Supported File Types**

| Category   | Extensions                                                                                  | Behavior                                          |
|------------|---------------------------------------------------------------------------------------------|---------------------------------------------------|
| Text files | `.txt`, `.md`, `.ex`, `.exs`, `.json`, `.yaml`, `.xml`, `.html`, `.css`, `.js`, `.ts`, etc. | Line-based reading with offset/length             |
| Documents  | `.pdf`, `.docx`, `.doc`                                                                     | Text extraction via Gitee AI PaddleOCR-VL-1.5     |
| Images     | `.png`, `.jpg`, `.jpeg`, `.tiff`, `.bmp`, `.gif`, `.webp`                                   | OCR text extraction via Gitee AI PaddleOCR-VL-1.5 |

**Parameters**

| Name      | Type    | Required | Default | Description                                                                                  |
|-----------|---------|----------|---------|----------------------------------------------------------------------------------------------|
| `path`    | string  | yes      | —       | Absolute path to the file to read                                                            |
| `offset`  | integer | no       | `0`     | Line number to start reading from (0-based, e.g. 100 skips first 100 lines); text files only |
| `length`  | integer | no       | `1000`  | Maximum number of lines to read; text files only                                             |
| `extract` | boolean | no       | `true`  | Whether to attempt document extraction for non-text files                                    |

**Return Value (success) — Text Files**

| Field         | Type    | Description                          |
|---------------|---------|--------------------------------------|
| `path`        | string  | The absolute path that was read      |
| `offset`      | integer | The offset that was applied          |
| `lines_read`  | integer | Number of lines actually returned    |
| `total_lines` | integer | Total lines in the file              |
| `content`     | string  | The file content (newline-separated) |

**Return Value (success) — Document Files**

| Field     | Type   | Description                                  |
|-----------|--------|----------------------------------------------|
| `path`    | string | The absolute path that was read              |
| `content` | string | The extracted text content (Markdown format) |
| `type`    | string | The detected document type (e.g., `"pdf"`)   |

**Document Extraction Requirements**

Document extraction requires the Gitee AI API key to be configured (same as the `doc_extract` tool). See [docs/modules/doc-extract.md](docs/modules/doc-extract.md) for setup instructions.

**Error Cases**

- `"File not found: #{path}"` — File does not exist
- `"Permission denied: #{path}"` — Insufficient permissions
- `"Path is a directory: #{path}"` — Path points to a directory
- `"Invalid path"` — Path is empty or not a valid string
- `"Document extraction failed: #{reason}"` — Failed to extract text from document

---

### write_file

Writes or appends content to a file on the filesystem. Creates the file and any missing parent directories if they do not exist.

**Parameters**

| Name      | Type   | Required | Default       | Description                             |
|-----------|--------|----------|---------------|-----------------------------------------|
| `path`    | string | yes      | —             | Absolute path to the file to write      |
| `content` | string | yes      | —             | The text content to write to the file   |
| `mode`    | string | no       | `"overwrite"` | Write mode: `"overwrite"` or `"append"` |

**Modes**

- `"overwrite"` — Replaces the entire file with new content (default)
- `"append"` — Adds content to the end of an existing file

**Return Value (success)**

| Field           | Type    | Description                        |
|-----------------|---------|------------------------------------|
| `path`          | string  | The absolute path that was written |
| `bytes_written` | integer | Number of bytes written            |

**Error Cases**

- `"Permission denied: #{path}"` — Insufficient permissions
- `"Cannot create directory #{dir}: #{reason}"` — Failed to create parent directories
- `"Invalid path"` — Path is empty or not a valid string

---

### list_directory

Lists the contents of a directory with optional recursion depth, glob pattern filter, and modified-time display.

**Parameters**

| Name            | Type    | Required | Default | Description                                             |
|-----------------|---------|----------|---------|---------------------------------------------------------|
| `path`          | string  | yes      | —       | Absolute path to the directory to list                  |
| `depth`         | integer | no       | `0`     | Recursion depth (0 = immediate children only)           |
| `show_modified` | boolean | no       | `false` | Include last modified time in entries                   |
| `pattern`       | string  | no       | `nil`   | Glob pattern to filter entries (e.g. `*.rb`, `**/*.ex`) |

**Pattern Behavior**

- Directories are always recursed into, but only entries matching the pattern are included in results
- Empty directories are suppressed when a pattern is active (they carry no matches)
- Supports `*` (matches any characters except `/`), `**` (matches any characters including `/`), and `?` (matches any single character except `/`)

**Return Value (success)**

| Field     | Type | Description        |
|-----------|------|--------------------|
| `entries` | list | List of entry maps |

Each entry contains:

| Field      | Type   | Description                                                               |
|------------|--------|---------------------------------------------------------------------------|
| `path`     | string | Relative path from the requested directory; directories suffixed with `/` |
| `size`     | string | Human-readable size (`"42 B"`, `"1.5 KB"`, `"2.3 MB"`, `"1.0 GB"`)        |
| `modified` | string | (Optional) ISO 8601 timestamp when `show_modified: true`                  |

**Error Cases**

- `"Not a directory: #{path}"` — Path is not a directory
- `"Directory not found: #{path}"` — Directory does not exist
- `"Permission denied: #{path}"` — Insufficient permissions

---

### delete_file

Deletes a file or directory from the filesystem.

**Parameters**

| Name        | Type    | Required | Default | Description                                                |
|-------------|---------|----------|---------|------------------------------------------------------------|
| `path`      | string  | yes      | —       | Absolute path to the file or directory to delete           |
| `recursive` | boolean | no       | `false` | If true, delete directories and their contents recursively |

**Return Value (success)**

| Field     | Type   | Description                        |
|-----------|--------|------------------------------------|
| `path`    | string | The absolute path that was deleted |
| `message` | string | `"Deleted successfully."`          |

**Error Cases**

- `"Path not found: #{path}"` — Path does not exist
- `"Permission denied: #{path}"` — Insufficient permissions
- `"Directory is not empty. Use recursive: true to delete non-empty directories."` — Attempting to delete non-empty directory without `recursive: true`
- `"eexist"` — Generic error when directory is not empty

---

### move_file

Moves or renames a file or directory on the filesystem. Creates missing destination parent directories automatically.

**Parameters**

| Name          | Type   | Required | Default | Description                                    |
|---------------|--------|----------|---------|------------------------------------------------|
| `source`      | string | yes      | —       | Absolute path of the file or directory to move |
| `destination` | string | yes      | —       | Absolute path of the new location              |

**Cross-Device Moves**

If the source and destination are on different filesystems (returns `:exdev` error), the tool automatically performs a copy-then-delete operation using `File.cp_r/2` and `File.rm_rf/1`.

**Return Value (success)**

| Field         | Type   | Description             |
|---------------|--------|-------------------------|
| `source`      | string | The original path       |
| `destination` | string | The new path            |
| `message`     | string | `"Moved successfully."` |

**Error Cases**

- `"Source not found: #{path}"` — Source path does not exist
- `"Permission denied"` — Insufficient permissions
- `"Cannot create destination directory: #{reason}"` — Failed to create parent directories
- `"Copy failed: #{reason}"` — Cross-device copy operation failed

---

### create_directory

Creates a directory and all missing parent directories (equivalent to `mkdir -p`). Succeeds silently if the directory already exists.

**Parameters**

| Name   | Type   | Required | Default | Description                              |
|--------|--------|----------|---------|------------------------------------------|
| `path` | string | yes      | —       | Absolute path of the directory to create |

**Return Value (success)**

| Field | Type | Description |
|-------|------|-------------|
| `message` | string | `"Directory created successfully."` |
| `path` | string | The absolute path that was created |

**Error Cases**

- `"Permission denied: #{path}"` — Insufficient permissions
- `"Failed to create directory: #{reason}"` — Other filesystem errors

---

### edit_block

Performs a targeted find-and-replace edit within a text file. This is the preferred tool for surgical edits — use it over rewriting the whole file when only a small section needs to change.

**Features**

- **Exact case-sensitive match**: The search string must match exactly
- **Line-ending normalization**: Automatically detects file line endings (LF/CRLF/CR) and normalizes the search string, so `old_string` with `\n` will match files with `\r\n` line endings
- **Replacement count guard**: Errors if the actual occurrence count differs from `expected_replacements`
- **Fuzzy-match fallback**: When exact match fails, searches for similar text using Levenshtein distance and reports the closest match with a character-level diff
- **Large-edit warning**: Warns when search or replace text exceeds 50 lines

**Parameters**

| Name                    | Type    | Required | Default | Description                        |
|-------------------------|---------|----------|---------|------------------------------------|
| `file_path`             | string  | yes      | —       | Absolute path to the file to edit  |
| `old_string`            | string  | yes      | —       | The exact text to find and replace |
| `new_string`            | string  | yes      | —       | The replacement text               |
| `expected_replacements` | integer | no       | `1`     | Number of replacements expected    |

**Return Value (success)**

| Field       | Type   | Description                                                 |
|-------------|--------|-------------------------------------------------------------|
| `file_path` | string | The absolute path that was edited                           |
| `message`   | string | Success message with replacement count and optional warning |

**Error Cases**

- `"File not found: #{path}"` — File does not exist
- `"Permission denied: #{path}"` — Insufficient permissions
- `"old_string cannot be empty — an empty search string would match everywhere."` — Empty search string
- `"Expected #{expected} replacement(s) but found #{count} occurrence(s)..."` — Count mismatch with actionable suggestions
- `"Exact match not found in #{file_path}, but a similar passage was found with X% similarity..."` — Fuzzy match found above 70% threshold, includes character-level diff `{-removed-}{+added+}`
- `"String not found in #{file_path}. The closest match..."` — No match above threshold

**Character-Level Diff Format**

When a fuzzy match is found, the error includes a diff showing what differs:

```
common_prefix{-removed_text-}{+added_text+}common_suffix
```

This helps identify exactly what needs to be corrected in `old_string`.

---

### search_files

Searches for files by name or searches within file contents. Uses ripgrep (`rg`) if available, falls back to `grep`, then native Elixir implementation.

**Parameters**

| Name            | Type    | Required | Default   | Description                                                         |
|-----------------|---------|----------|-----------|---------------------------------------------------------------------|
| `path`          | string  | yes      | —         | Absolute path to the directory to search in                         |
| `pattern`       | string  | yes      | —         | The search pattern (substring or regex)                             |
| `search_type`   | string  | no       | `"files"` | `"files"` or `"content"`                                            |
| `file_pattern`  | string  | no       | `nil`     | Glob pattern to filter files (e.g. `*.ex`), only for content search |
| `ignore_case`   | boolean | no       | `true`    | Case-insensitive matching                                           |
| `max_results`   | integer | no       | `50`      | Maximum number of results to return                                 |
| `context_lines` | integer | no       | `2`       | Number of context lines around content matches                      |

#### search_type: "files"

Finds files and directories whose names match the pattern (case-insensitive substring by default).

**Return Value (success)**

| Field         | Type    | Description                     |
|---------------|---------|---------------------------------|
| `path`        | string  | The directory that was searched |
| `pattern`     | string  | The search pattern used         |
| `search_type` | string  | `"files"`                       |
| `results`     | list    | List of result maps             |
| `count`       | integer | Number of results               |

Each result contains:

| Field  | Type   | Description                     |
|--------|--------|---------------------------------|
| `path` | string | Full path to the file/directory |
| `name` | string | Base name of the file/directory |
| `type` | string | `"file"` or `"directory"`       |

#### search_type: "content"

Finds files whose contents contain the pattern.

**Return Value (success)**

| Field         | Type    | Description                     |
|---------------|---------|---------------------------------|
| `path`        | string  | The directory that was searched |
| `pattern`     | string  | The search pattern used         |
| `search_type` | string  | `"content"`                     |
| `results`     | list    | List of result maps             |
| `count`       | integer | Number of files with matches    |

Each result contains:

| Field     | Type   | Description           |
|-----------|--------|-----------------------|
| `path`    | string | Full path to the file |
| `matches` | list   | List of match maps    |

Each match contains:

| Field         | Type    | Description                                        |
|---------------|---------|----------------------------------------------------|
| `line_number` | integer | 1-based line number of the match                   |
| `line`        | string  | The matching line text                             |
| `context`     | string  | Multi-line string showing context around the match |

**Context Format**

The `context` field shows `context_lines` before and after each match:

```
   5: line before match
   6: another context line
=> 7: the matching line
   8: line after match
   9: more context
```

- The match line is prefixed with `=>`
- Context lines are prefixed with spaces
- Each line shows its 1-based line number
- **Each match gets an independent context window** — overlapping windows between nearby matches do NOT merge

**Error Cases**

- `"Not a directory: #{path}"` — Path is not a directory
- `"Directory not found: #{path}"` — Directory does not exist
- `"Unknown search_type: #{search_type}. Use \"files\" or \"content\"."` — Invalid search type

---

### terminate_process

Terminates a managed process started with `start_process`. Sends SIGTERM to the process.

**Parameters**

| Name         | Type   | Required | Default | Description                                |
|--------------|--------|----------|---------|--------------------------------------------|
| `process_id` | string | yes      | —       | The process ID returned by `start_process` |

**Return Value (success)**

| Field        | Type   | Description                          |
|--------------|--------|--------------------------------------|
| `process_id` | string | The process ID that was terminated   |
| `status`     | string | The process status after termination |

**Error Cases**

- `"Process not found: #{process_id}"` — No process with that ID exists
- `"Process has no associated system PID"` — Process entry exists but has no PID

## Response Format

### Success Response

All successful responses are TOON-encoded as plain text with key-value pairs:

```
path: /Users/example/file.txt
lines_read: 10
total_lines: 100
content: "file contents here"
```

TOON encoding eliminates JSON syntax overhead (no quotes around keys, no braces, no commas), reducing token consumption by 30–60% while remaining fully readable.

### Error Response

Error responses set `isError: true` on the response struct with a plain-text error message:

```
Failed to read file: File not found: /nonexistent/path.txt
```

## Testing

Tests live under `test/exhub/mcp/tools/desktop/`. Each test file covers one tool.

**Test Structure**

- Tests use `ExUnit.Case, async: true`
- Each test creates a temporary directory in `System.tmp_dir!()` with a random suffix
- `on_exit/1` callback cleans up the temporary directory
- Tests verify both success cases and error handling

**Running Tests**

```bash
# Run all desktop tool tests
mix test test/exhub/mcp/tools/desktop/

# Run a specific test file
mix test test/exhub/mcp/tools/desktop/read_file_test.exs

# Run with verbose output
mix test test/exhub/mcp/tools/desktop/ --trace
```

**Special Setup for search_files**

The `SearchFilesTest` module starts the `:exile` application via `setup_all` to enable ripgrep/grep subprocess support:

```elixir
setup_all do
  Application.ensure_all_started(:exile)
  :ok
end
```

## Shell Configuration

The Desktop MCP Server allows you to customize the shell used for executing commands. By default, it uses `sh`, but you can configure any shell executable.

### Configuration

Set the shell in your `config/config.exs` or `config/runtime.exs`:

```elixir
config :exhub, :shell, "bash"
```

Or via environment variable in `runtime.exs`:

```elixir
config :exhub, :shell, System.get_env("EXHUB_SHELL", "sh")
```

### Supported Shells

The following shells are automatically detected and use appropriate flags:

| Shell  | Arguments Used | Notes                                     |
|--------|----------------|-------------------------------------------|
| `sh`   | `-l -c`        | Default shell, uses login shell mode      |
| `bash` | `-l -c`        | Bourne-again shell with login mode        |
| `zsh`  | `-l -c`        | Z shell with login mode                   |
| `dash` | `-l -c`        | Debian Almquist shell                     |
| `ksh`  | `-l -c`        | Korn shell                                |
| `fish` | `-c`           | Friendly interactive shell (no `-l` flag) |
| Other  | `-c`           | Falls back to `-c` for unknown shells     |

### Affected Tools

The custom shell configuration applies to:

- `execute_command` — Executes shell commands with the configured shell
- `start_process` — Starts long-running processes with the configured shell
- `search_files` — Uses the configured shell for grep fallback operations

### Examples

```elixir
# Use bash with its full feature set
config :exhub, :shell, "bash"

# Use zsh on macOS
config :exhub, :shell, "zsh"

# Use fish shell
config :exhub, :shell, "fish"

# Use a specific path to a shell
config :exhub, :shell, "/usr/local/bin/bash"
```

## Path Expansion

All `path` parameters support tilde expansion via `Helpers.expand_path/1`:

| Input            | Output                                      |
|------------------|---------------------------------------------|
| `~`              | User home directory (`System.user_home!()`) |
| `~/foo/bar`      | `$HOME/foo/bar`                             |
| `/absolute/path` | `/absolute/path` (unchanged)                |
| `relative/path`  | `relative/path` (unchanged)                 |
| `nil`            | `nil` (for optional path parameters)        |

This allows tools to accept user-friendly paths like `~/Documents/file.txt` without requiring the caller to expand them first.
