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
| Office     | `.docx`, `.xlsx`, `.pptx`                                                                   | Local text extraction via [nanoxml](https://github.com/justrach/nanoxml) CLI — no API needed |
| Documents  | `.pdf`, `.doc`                                                                              | Text extraction via Gitee AI Unlimited-OCR        |
| Images     | `.png`, `.jpg`, `.jpeg`, `.tiff`, `.bmp`, `.gif`, `.webp`                                   | OCR text extraction via Gitee AI Unlimited-OCR    |

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

- **Office files** (`.docx`, `.xlsx`, `.pptx`): Extracted locally via the [nanoxml](https://github.com/justrach/nanoxml) CLI binary — no API key or network access required. The `nanoxml` binary must be on `PATH`.
- **Other documents** (`.pdf`, `.doc`) and **images** (`.png`, `.jpg`, etc.): Require the Gitee AI API key to be configured (same as the `doc_extract` tool). See [docs/modules/doc-extract.md](docs/modules/doc-extract.md) for setup instructions.

**Error Cases**

- `"File not found: #{path}"` — File does not exist
- `"Permission denied: #{path}"` — Insufficient permissions
- `"Path is a directory: #{path}"` — Path points to a directory
- `"Invalid path"` — Path is empty or not a valid string
- `"Document extraction failed: #{reason}"` — Failed to extract text from document

---

### read_multiple_files

Reads multiple files in a single call with parallel execution. Returns results for each file with success status and content or error message. Supports optional offset/length for text files and document extraction for supported file types.

**Parameters**

| Name      | Type            | Required | Default | Description                                                                                  |
|-----------|-----------------|----------|---------|----------------------------------------------------------------------------------------------|
| `paths`   | list of strings | yes      | —       | List of absolute paths to the files to read                                                  |
| `offset`  | integer         | no       | `0`     | Line number to start reading from (0-based, e.g. 100 skips first 100 lines); text files only |
| `length`  | integer         | no       | `2000`  | Maximum number of lines to read; text files only                                             |
| `extract` | boolean         | no       | `false` | Whether to attempt document extraction for non-text files                                    |

**Return Value (success)**

| Field     | Type | Description                     |
|-----------|------|---------------------------------|
| `results` | list | List of result maps (see below) |

Each result in `results` contains:

| Field         | Type    | Description                                         |
|---------------|---------|-----------------------------------------------------|
| `path`        | string  | The path that was read                              |
| `success`     | boolean | Whether the read was successful                     |
| `content`     | string  | The file content (present when `success: true`)     |
| `lines_read`  | integer | Number of lines actually returned (text files only) |
| `total_lines` | integer | Total lines in the file (text files only)           |
| `error`       | string  | Error message (present when `success: false`)       |

**Parallel Execution**

Files are read concurrently using `Task.async_stream/3` with `max_concurrency: 10`. Results maintain the same order as the input `paths` list, regardless of which file finishes reading first.

**UTF-8 Sanitization**

All file content is sanitized to ensure valid UTF-8 encoding. Invalid byte sequences are replaced with the Unicode replacement character (U+FFFD) to prevent encoding errors in the response.

**Document Extraction**

When `extract: true`:

- **Office files** (`.docx`, `.xlsx`, `.pptx`): Extracted locally via the [nanoxml](https://github.com/justrach/nanoxml) CLI — no API key required.
- **Other documents** (`.pdf`, `.doc`) and **images**: Processed using Gitee AI Unlimited-OCR for text extraction. See [docs/modules/doc-extract.md](docs/modules/doc-extract.md) for setup instructions.

**Error Cases**

Each file is processed independently, so errors are reported per-file in the `results` array with `success: false` and an `error` field:

- `"File not found: #{path}"` — File does not exist
- `"Permission denied: #{path}"` — Insufficient permissions
- `"Invalid path: #{path}"` — Path is not absolute or valid
- Document extraction errors (when `extract: true`)

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

Searches a codebase in one of three modes (`search_type`): `semantic` (the default) — probe-backed, AST-aware BM25 code search returning whole code blocks; `glob` — glob matching over relative file and directory paths (mirrors aider-desk's `power_glob`); `content` — literal/regex content matching with context lines. The `glob` and `content` modes use ripgrep (`rg`) if available and fall back to a native Elixir implementation; `semantic` shells out to the `probe` CLI instead.

**Parameters**

| Name            | Type    | Required              | Default      | Description                                                          |
|-----------------|---------|-----------------------|--------------|----------------------------------------------------------------------|
| `path`          | string  | yes                   | —            | Absolute path (or `~` shorthand) to the directory to search in       |
| `search_type`   | string  | no                    | `"semantic"` | `"semantic"` (default), `"glob"` or `"content"`                     |
| `query`         | string  | for `semantic`        | —            | Semantic query; Elasticsearch syntax and hints supported             |
| `pattern`       | string  | for `glob`/`content`  | —            | For `glob`, a glob pattern relative to `path` (trailing `/` = directories only); for `content`, substring/regex |
| `file_pattern`  | string  | no                    | `nil`        | Glob pattern to filter files (e.g. `*.ex`), only for content search  |
| `allow_tests`   | boolean | no                    | `false`      | Include test files in semantic results                               |
| `exact`         | boolean | no                    | `false`      | Tokenization-free, case-insensitive semantic match                   |
| `max_results`   | integer | no                    | `1000`/`50`  | Max results (`glob` default `1000`, `content` `50`); optional for `semantic` |
| `max_tokens`    | integer | no                    | `5000`       | Max tokens of code content returned by semantic search               |
| `language`      | string  | no                    | `nil`        | Restrict semantic search to one language (see below)                 |
| `ignore_case`   | boolean | no                    | `true`       | Case-insensitive matching (`content`)                                |
| `ignore`        | list    | no                    | `nil`        | Glob patterns to exclude from `glob` results                         |
| `include_ignored` | boolean | no                  | `false`      | Include `.gitignore`/`.ignore`/`.rgignore` matches and hidden dotfiles |
| `include_dirs`  | boolean | no                    | `true`       | Include directories (suffixed `/`) in `glob` results                 |
| `context_lines` | integer | no                    | `2`          | Number of context lines around content matches                       |

#### search_type: "semantic" (default)

AST-aware, BM25-ranked code search over `query`, backed by the [`probe`](https://github.com/probelabs/probe) CLI. Unlike `glob`/`content` this returns whole code blocks (with file path and block line numbers) rather than individual matching lines, and it understands Elasticsearch-style query syntax.

**Query syntax**

| Syntax        | Example                                                | Meaning                                                     |
|---------------|--------------------------------------------------------|-------------------------------------------------------------|
| Required term | `+authenticate`                                        | Term must appear                                            |
| Excluded term | `-migration`                                           | Term must not appear                                        |
| Boolean       | `parse AND token`                                      | Both terms                                                  |
| Phrase        | `"reset password"`                                     | Exact phrase                                                |
| Hint          | `ext:ex`, `dir:lib`, `lang:elixir`, `file:src/**/*.py` | Restrict by extension / directory / language / glob         |

**Probe invocation**

`semantic` mode shells out to `probe` (the `glob`/`content` modes never do):

```
probe search [--exact] [--allow-tests] [--max-results N] --max-tokens N [--language L] --timeout 300 -- <query> <path>
```

`--max-tokens` defaults to `5000`; the probe-side timeout is fixed at 300 s (ExHub kills the subprocess after 310 s). Defaults mirror aider-desk's `semantic_search` Power Tool.

**Return Value (success)**

Plain text — not a structured/TOON payload:

```
Pattern: <query>
Path: <path>
---
File: /abs/path/to/file.ext

<line>  <code>
...

---
```

**Error Cases**

- `"Missing required parameter: query (for search_type \"semantic\")."` — `query` omitted or blank
- `"The 'probe' binary was not found. ..."` — no `probe` on `PATH` and `:probe_binary` unset
- `"Semantic search failed: <reason>"` — probe exited non-zero or could not be started
- `"No results found."` — probe produced no output

**Caveats**

- **Non-deterministic ranking.** `probe` re-indexes per invocation and tie-breaks nondeterministically, so identical repeated calls can return the same files in a different order (and occasionally a slightly different byte size). Compare medians across repeats rather than single runs.
- **Build-dependent results.** Distinct `probe` builds can report the same version (`probe-code 0.6.0`) yet behave differently; in local measurements the npm-bundled build (`~/.bun/bin/probe`) was ~2.5x slower and returned ~2x the output tokens of the native build. Pin `:probe_binary` (see [Probe Binary](#probe-binary)) when reproducible results matter.

#### search_type: "glob"

Finds files and directories whose paths match a glob `pattern` (relative to `path`) and returns paths relative to `path`. Mirrors aider-desk's `power_glob`.

Supports `*`, `**`, `?`, `[...]` and `{a,b}` (e.g. `src/**/*.ts`, `*.md`, `config/*.{ex,exs}`). A pattern without a `/` is anchored to `path`, so `*.ex` matches only files directly under `path`. **Directories are included by default and suffixed with `/`** (set `include_dirs: false` for files only); a pattern ending in `/` returns directories only. Directories are discovered as the ancestors of the files ripgrep reports, so a directory with no non-ignored file (e.g. an empty one) is not returned. Hidden dotfiles and entries matched by `.gitignore`/`.ignore`/`.rgignore` are excluded unless `include_ignored` is true; pass `ignore` to exclude additional globs. Results are capped at `max_results` (default 1000) with a 5000-entry walk limit — truncation is reported via `limit_reached`/`notice` rather than silently dropped.

**Return Value (success)**

| Field           | Type    | Description                          |
|-----------------|---------|--------------------------------------|
| `path`          | string  | The directory that was searched      |
| `pattern`       | string  | The glob pattern used                |
| `search_type`   | string  | `"glob"`                             |
| `results`       | list    | Paths relative to `path` (directories end in `/`) |
| `count`         | integer | Number of results                    |
| `limit_reached` | boolean | `true` when results were truncated   |
| `notice`        | string  | Truncation guidance, when truncated  |

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
- `"Unknown search_type: #{search_type}. Use \"semantic\", \"glob\" or \"content\"."` — Invalid search type

---

### interact_with_process

Sends input to an interactive process's stdin. Use this to interact with REPLs, interactive shells, or any process that requires user input.

**Parameters**

| Name         | Type   | Required | Default | Description                                |
|--------------|--------|----------|---------|--------------------------------------------|
| `process_id` | string | yes      | —       | The process ID returned by `start_process` |
| `input`      | string | yes      | —       | The text to send to the process's stdin    |

**Notes**

- The input is sent directly to the process's stdin without any newline appended
- Add `\n` to your input if you want to simulate pressing Enter
- Only works with processes started with `start_process(interactive: true)`

**Return Value (success)**

| Field        | Type    | Description             |
|--------------|---------|-------------------------|
| `success`    | boolean | `true`                  |
| `process_id` | string  | The process ID          |
| `input_sent` | string  | The input that was sent |

**Return Value (error)**

| Field    | Type    | Description                                              |
|----------|---------|----------------------------------------------------------|
| `success` | boolean | `false`                                                 |
| `error`  | string  | Error description                                        |

**Error Cases**

- `"Process not found: #{process_id}"` — No process with that ID exists
- `"Process #{process_id} is not interactive. Start with interactive: true."` — Process was not started in interactive mode
- `"Process #{process_id} has no port reference"` — Internal error, port not initialized
- `"Process #{process_id} is not running"` — Process has completed or crashed

**Example**

```bash
# Start an interactive Python REPL
start_process(command="python3 -i", interactive=true)
# Returns: {"process_id": "proc_123_456", "pid": 12345, "interactive": true}

# Send a command to the REPL (note: need to add newline)
interact_with_process(process_id="proc_123_456", input="print('Hello, World!')\n")
# Returns: {"success": true, "process_id": "proc_123_456", "input_sent": "print('Hello, World!')\n"}

# Read the output
read_process_output(process_id="proc_123_456")
# Returns: {"output": "Hello, World!\n", "status": "running", ...}
```

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

The `SearchFilesTest` module starts the `:exile` application via `setup_all` to enable ripgrep/grep and probe subprocess support. Semantic-mode tests therefore assume a `probe` binary is resolvable (config or `PATH`); they assert on output shape rather than a fixed ranking, since `probe` ordering is non-deterministic.

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

## Probe Binary

`search_files` in `semantic` mode (the default) shells out to the `probe` CLI. ExHub resolves it from the `:exhub, :probe_binary` config, falling back to the first `probe` on the system `PATH` (`System.find_executable("probe")`). The `glob` and `content` modes are unaffected — they use `rg`/native Elixir.

Pin it when reproducible results matter, since different builds (even those reporting the same version) rank results differently and run at different speeds:

```elixir
# config/config.exs
config :exhub, :probe_binary, "/usr/local/bin/probe"
```

Set it to `nil` (or delete the key) to fall back to `PATH` lookup. As with the shell, the value can be overridden in `config/runtime.exs`.

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
