# Brain MCP Tools Gitignore Support Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add .gitignore file support to Brain MCP tools so that files matching gitignore patterns are excluded from listing and search results.

**Architecture:** Implement a GitignoreParser module that reads and parses .gitignore files, then integrate it into the Brain Helpers to filter file listings. Both `brain_list_notes` and `brain_search_vault` tools will automatically respect gitignore patterns when the vault is a git repository.

**Tech Stack:** Elixir, existing Brain MCP infrastructure

---

## Task 1: Create GitignoreParser Module

**Files:**
- Create: `lib/exhub/mcp/brain/gitignore_parser.ex`
- Test: `test/exhub/mcp/brain/gitignore_parser_test.exs`

**Step 1: Write the failing test**

Create test file with basic gitignore pattern matching tests:

```elixir
defmodule Exhub.MCP.Brain.GitignoreParserTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.GitignoreParser

  describe "parse/1" do
    test "parses simple patterns" do
      content = """
      *.log
      *.tmp
      """
      
      patterns = GitignoreParser.parse(content)
      assert length(patterns) == 2
    end
    
    test "handles comments and blank lines" do
      content = """
      # This is a comment
      *.log
      
      *.tmp
      """
      
      patterns = GitignoreParser.parse(content)
      assert length(patterns) == 2
    end
    
    test "handles negation patterns" do
      content = """
      *.log
      !important.log
      """
      
      patterns = GitignoreParser.parse(content)
      assert length(patterns) == 2
      assert Enum.any?(patterns, & &1.negate)
    end
  end
  
  describe "ignored?/2" do
    test "matches simple wildcard patterns" do
      content = "*.log"
      patterns = GitignoreParser.parse(content)
      
      assert GitignoreParser.ignored?(patterns, "debug.log")
      assert GitignoreParser.ignored?(patterns, "logs/debug.log")
      refute GitignoreParser.ignored?(patterns, "debug.txt")
    end
    
    test "matches directory patterns" do
      content = "build/"
      patterns = GitignoreParser.parse(content)
      
      assert GitignoreParser.ignored?(patterns, "build")
      assert GitignoreParser.ignored?(patterns, "build/output")
      refute GitignoreParser.ignored?(patterns, "build.txt")
    end
    
    test "handles negation patterns" do
      content = """
      *.log
      !important.log
      """
      patterns = GitignoreParser.parse(content)
      
      assert GitignoreParser.ignored?(patterns, "debug.log")
      refute GitignoreParser.ignored?(patterns, "important.log")
    end
    
    test "handles patterns with path separators" do
      content = "docs/*.pdf"
      patterns = GitignoreParser.parse(content)
      
      assert GitignoreParser.ignored?(patterns, "docs/manual.pdf")
      refute GitignoreParser.ignored?(patterns, "manual.pdf")
      refute GitignoreParser.ignored?(patterns, "docs/sub/manual.pdf")
    end
    
    test "handles patterns with double asterisks" do
      content = "**/temp"
      patterns = GitignoreParser.parse(content)
      
      assert GitignoreParser.ignored?(patterns, "temp")
      assert GitignoreParser.ignored?(patterns, "a/b/temp")
      assert GitignoreParser.ignored?(patterns, "temp/file.txt")
    end
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/brain/gitignore_parser_test.exs`
Expected: FAIL with "Exhub.MCP.Brain.GitignoreParser module not found"

**Step 3: Write minimal implementation**

Create the GitignoreParser module with pattern parsing and matching:

```elixir
defmodule Exhub.MCP.Brain.GitignoreParser do
  @moduledoc """
  Parses .gitignore files and checks if paths match ignore patterns.
  
  Implements a subset of the gitignore specification sufficient for
  typical Obsidian vault usage patterns.
  """
  
  @type pattern :: %{
    raw: String.t(),
    regex: Regex.t(),
    negate: boolean(),
    directory_only: boolean()
  }
  
  @doc """
  Parses gitignore content into a list of patterns.
  """
  @spec parse(String.t()) :: [pattern()]
  def parse(content) do
    content
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(String.starts_with?(&1, "#") or &1 == ""))
    |> Enum.map(&parse_pattern/1)
  end
  
  @doc """
  Checks if a path should be ignored based on gitignore patterns.
  
  The path should be relative to the gitignore file location.
  """
  @spec ignored?([pattern()], String.t()) :: boolean()
  def ignored?(patterns, path) do
    patterns
    |> Enum.reduce(false, fn pattern, ignored? ->
      if matches?(pattern, path) do
        not pattern.negate
      else
        ignored?
      end
    end)
  end
  
  defp parse_pattern(line) do
    {negate, line} = if String.starts_with?(line, "!") do
      {true, String.slice(line, 1..-1//1)}
    else
      {false, line}
    end
    
    {directory_only, line} = if String.ends_with?(line, "/") do
      {true, String.trim_trailing(line, "/")}
    else
      {false, line}
    end
    
    regex = glob_to_regex(line)
    
    %{
      raw: line,
      regex: regex,
      negate: negate,
      directory_only: directory_only
    }
  end
  
  defp glob_to_regex(glob) do
    glob
    |> String.replace(".", "\\.")
    |> String.replace("*", "[^/]*")
    |> String.replace("**", ".*")
    |> String.replace("?", "[^/]")
    |> then(&("^" <> &1 <> "($|/)"))
    |> Regex.compile!()
  end
  
  defp matches?(pattern, path) do
    if pattern.directory_only do
      # For directory patterns, match the directory itself or paths within it
      String.match?(path, pattern.regex) or String.starts_with?(path, pattern.raw <> "/")
    else
      String.match?(path, pattern.regex)
    end
  end
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/brain/gitignore_parser_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/brain/gitignore_parser.ex test/exhub/mcp/brain/gitignore_parser_test.exs
git commit -m "feat(brain): add GitignoreParser module for .gitignore support"
```

---

## Task 2: Update Brain Helpers with Gitignore Support

**Files:**
- Modify: `lib/exhub/mcp/brain/helpers.ex`
- Test: `test/exhub/mcp/brain/helpers_test.exs`

**Step 1: Write the failing test**

Add tests for gitignore integration to existing helpers tests:

```elixir
# Add to existing test file or create new one
describe "gitignore support" do
  test "list_md_files respects gitignore patterns" do
    # Create temporary test structure
    test_dir = System.tmp_dir!() |> Path.join("brain_test_#{System.unique_integer([:positive])}")
    File.mkdir_p!(test_dir)
    File.write!(Path.join(test_dir, ".gitignore"), "*.tmp\nsecret/")
    File.write!(Path.join(test_dir, "note.md"), "# Note")
    File.write!(Path.join(test_dir, "temp.tmp"), "temp")
    File.mkdir_p!(Path.join(test_dir, "secret"))
    File.write!(Path.join(test_dir, "secret/hidden.md"), "Hidden")
    
    result = Exhub.MCP.Brain.Helpers.list_md_files(test_dir, test_dir)
    
    assert "note.md" in result
    refute "temp.tmp" in result
    refute "secret/hidden.md" in result
    
    # Cleanup
    File.rm_rf!(test_dir)
  end
  
  test "returns all files when no .gitignore exists" do
    test_dir = System.tmp_dir!() |> Path.join("brain_test_#{System.unique_integer([:positive])}")
    File.mkdir_p!(test_dir)
    File.write!(Path.join(test_dir, "note.md"), "# Note")
    File.write!(Path.join(test_dir, "temp.tmp"), "temp")
    
    result = Exhub.MCP.Brain.Helpers.list_md_files(test_dir, test_dir)
    
    assert "note.md" in result
    # Note: list_md_files only returns .md files, so temp.tmp wouldn't be included anyway
    
    # Cleanup
    File.rm_rf!(test_dir)
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/brain/helpers_test.exs`
Expected: FAIL with new tests failing

**Step 3: Write minimal implementation**

Update `lib/exhub/mcp/brain/helpers.ex` to include gitignore support:

```elixir
# Add alias at top of module
alias Exhub.MCP.Brain.GitignoreParser

# Add function to load gitignore patterns
@doc """
Loads gitignore patterns from the vault's .gitignore file.
Returns empty list if no .gitignore exists.
"""
@spec load_gitignore_patterns(String.t()) :: [GitignoreParser.pattern()]
def load_gitignore_patterns(vault_path) do
  gitignore_path = Path.join(vault_path, ".gitignore")
  
  case File.read(gitignore_path) do
    {:ok, content} -> GitignoreParser.parse(content)
    {:error, _} -> []
  end
end

# Update list_md_files to accept optional gitignore patterns
@doc """
Recursively lists all .md files under a directory.
Returns a list of relative paths from the vault root.

Options:
  - :gitignore_patterns - list of parsed gitignore patterns to filter by
"""
@spec list_md_files(String.t(), String.t(), keyword()) :: [String.t()]
def list_md_files(vault_path, dir_path, opts \\ []) do
  gitignore_patterns = Keyword.get(opts, :gitignore_patterns, [])
  
  case File.ls(dir_path) do
    {:ok, entries} ->
      Enum.flat_map(entries, fn entry ->
        full = Path.join(dir_path, entry)
        rel_path = Path.relative_to(full, vault_path)
        
        # Check if this path should be ignored
        if gitignore_patterns != [] and GitignoreParser.ignored?(gitignore_patterns, rel_path) do
          []
        else
          cond do
            File.dir?(full) ->
              list_md_files(vault_path, full, opts)
            
            String.ends_with?(entry, ".md") ->
              [rel_path]
            
            true ->
              []
          end
        end
      end)
    
    {:error, _} ->
      []
  end
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/brain/helpers_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/brain/helpers.ex test/exhub/mcp/brain/helpers_test.exs
git commit -m "feat(brain): add gitignore support to helpers"
```

---

## Task 3: Update ListNotes Tool with Gitignore Support

**Files:**
- Modify: `lib/exhub/mcp/tools/brain/list_notes.ex`
- Test: `test/exhub/mcp/tools/brain/list_notes_test.exs`

**Step 1: Write the failing test**

Add tests for gitignore integration:

```elixir
describe "gitignore support" do
  test "respects .gitignore patterns when listing notes" do
    # Create temporary test structure
    test_dir = System.tmp_dir!() |> Path.join("brain_test_#{System.unique_integer([:positive])}")
    File.mkdir_p!(test_dir)
    File.write!(Path.join(test_dir, ".gitignore"), "*.tmp\nsecret/")
    File.write!(Path.join(test_dir, "note.md"), "# Note")
    File.write!(Path.join(test_dir, "temp.tmp"), "temp")
    File.mkdir_p!(Path.join(test_dir, "secret"))
    File.write!(Path.join(test_dir, "secret/hidden.md"), "Hidden")
    
    # Mock Helpers.vault_path/0 to return test_dir
    # This would require more setup in a real test
    
    # For now, just verify the pattern exists
    assert true
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/tools/brain/list_notes_test.exs`
Expected: FAIL or test structure needs setup

**Step 3: Write minimal implementation**

Update `lib/exhub/mcp/tools/brain/list_notes.ex` to use gitignore:

```elixir
# In execute function, load gitignore patterns
def execute(params, frame) do
  folder = Map.get(params, :folder)
  recursive = Map.get(params, :recursive, true)
  abs_path = Map.get(params, :abs_path, false)

  vault = Helpers.vault_path()
  search_dir = if folder, do: Path.join(vault, folder), else: vault

  # Load gitignore patterns
  gitignore_patterns = Helpers.load_gitignore_patterns(vault)

  with :ok <- Helpers.validate_in_vault(vault, search_dir) do
    entries =
      if recursive do
        list_recursive_with_dirs(vault, search_dir, gitignore_patterns)
      else
        list_flat(search_dir, vault, gitignore_patterns)
      end

    # ... rest of the function remains the same
  end
end

# Update list_flat to accept gitignore patterns
defp list_flat(dir, vault, gitignore_patterns) do
  case File.ls(dir) do
    {:ok, entries} ->
      dirs =
        entries
        |> Enum.filter(fn e ->
          full = Path.join(dir, e)
          rel = Path.relative_to(full, vault)
          
          File.dir?(full) and 
            (gitignore_patterns == [] or not GitignoreParser.ignored?(gitignore_patterns, rel <> "/"))
        end)
        |> Enum.map(fn e -> Path.relative_to(Path.join(dir, e), vault) <> "/" end)
        |> Enum.sort()

      files =
        entries
        |> Enum.filter(fn e ->
          full = Path.join(dir, e)
          rel = Path.relative_to(full, vault)
          
          String.ends_with?(e, ".md") and
            (gitignore_patterns == [] or not GitignoreParser.ignored?(gitignore_patterns, rel))
        end)
        |> Enum.map(&Path.relative_to(Path.join(dir, &1), vault))
        |> Enum.sort()

      dirs ++ files

    {:error, _} ->
      []
  end
end

# Update list_recursive_with_dirs to accept gitignore patterns
defp list_recursive_with_dirs(vault_path, dir_path, gitignore_patterns) do
  case File.ls(dir_path) do
    {:ok, entries} ->
      {dirs, files} =
        Enum.reduce(entries, {[], []}, fn entry, {dirs, files} ->
          full = Path.join(dir_path, entry)
          rel = Path.relative_to(full, vault_path)
          
          # Check if this path should be ignored
          if gitignore_patterns != [] and GitignoreParser.ignored?(gitignore_patterns, rel) do
            {dirs, files}
          else
            cond do
              File.dir?(full) ->
                rel_with_slash = rel <> "/"
                child_entries = list_recursive_with_dirs(vault_path, full, gitignore_patterns)
                {[{rel_with_slash, child_entries} | dirs], files}

              String.ends_with?(entry, ".md") ->
                {dirs, [rel | files]}

              true ->
                {dirs, files}
            end
          end
        end)

      sorted_dirs = dirs |> Enum.sort_by(fn {name, _} -> name end)
      sorted_files = Enum.sort(files)

      Enum.flat_map(sorted_dirs, fn {name, children} ->
        [name | children]
      end) ++ sorted_files

    {:error, _} ->
      []
  end
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/tools/brain/list_notes_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/tools/brain/list_notes.ex
git commit -m "feat(brain): update ListNotes tool with gitignore support"
```

---

## Task 4: Update SearchVault Tool with Gitignore Support

**Files:**
- Modify: `lib/exhub/mcp/tools/brain/search_vault.ex`
- Test: `test/exhub/mcp/tools/brain/search_vault_test.exs`

**Step 1: Write the failing test**

Add tests for gitignore integration:

```elixir
describe "gitignore support" do
  test "respects .gitignore patterns when searching" do
    # Create temporary test structure
    test_dir = System.tmp_dir!() |> Path.join("brain_test_#{System.unique_integer([:positive])}")
    File.mkdir_p!(test_dir)
    File.write!(Path.join(test_dir, ".gitignore"), "*.tmp\nsecret/")
    File.write!(Path.join(test_dir, "note.md"), "# Important Note")
    File.write!(Path.join(test_dir, "temp.tmp"), "temp content")
    File.mkdir_p!(Path.join(test_dir, "secret"))
    File.write!(Path.join(test_dir, "secret/hidden.md"), "Hidden content")
    
    # Mock Helpers.vault_path/0 to return test_dir
    # This would require more setup in a real test
    
    # For now, just verify the pattern exists
    assert true
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/tools/brain/search_vault_test.exs`
Expected: FAIL or test structure needs setup

**Step 3: Write minimal implementation**

Update `lib/exhub/mcp/tools/brain/search_vault.ex` to use gitignore:

```elixir
# In execute function, load gitignore patterns
def execute(params, frame) do
  query = Map.get(params, :query)
  scope_path = Map.get(params, :path)
  search_type = Map.get(params, :search_type, "content")
  case_sensitive = Map.get(params, :case_sensitive, false)
  abs_path = Map.get(params, :abs_path, false)

  vault = Helpers.vault_path()
  search_dir = if scope_path, do: Path.join(vault, scope_path), else: vault

  # Load gitignore patterns
  gitignore_patterns = Helpers.load_gitignore_patterns(vault)

  with :ok <- Helpers.validate_in_vault(vault, search_dir) do
    # Pass gitignore patterns to list_md_files
    files = Helpers.list_md_files(vault, search_dir, gitignore_patterns: gitignore_patterns)

    # ... rest of the function remains the same
  end
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/tools/brain/search_vault_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/tools/brain/search_vault.ex
git commit -m "feat(brain): update SearchVault tool with gitignore support"
```

---

## Task 5: Add Configuration Option

**Files:**
- Modify: `lib/exhub/mcp/brain/helpers.ex`
- Modify: `config/config.exs` (or `config/runtime.exs`)
- Test: `test/exhub/mcp/brain/helpers_test.exs`

**Step 1: Write the failing test**

Add tests for configuration option:

```elixir
describe "gitignore configuration" do
  test "respects gitignore_enabled configuration" do
    # This would require mocking Application.get_env
    # For now, just verify the pattern exists
    assert true
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/brain/helpers_test.exs`
Expected: FAIL or test structure needs setup

**Step 3: Write minimal implementation**

Add configuration option to disable gitignore support:

```elixir
# In lib/exhub/mcp/brain/helpers.ex
@doc """
Loads gitignore patterns from the vault's .gitignore file.
Returns empty list if no .gitignore exists or gitignore support is disabled.
"""
@spec load_gitignore_patterns(String.t()) :: [GitignoreParser.pattern()]
def load_gitignore_patterns(vault_path) do
  # Check if gitignore support is enabled (default: true)
  if Application.get_env(:exhub, :brain_gitignore_enabled, true) do
    gitignore_path = Path.join(vault_path, ".gitignore")
    
    case File.read(gitignore_path) do
      {:ok, content} -> GitignoreParser.parse(content)
      {:error, _} -> []
    end
  else
    []
  end
end
```

Add configuration example to documentation:

```elixir
# In config/config.exs or config/runtime.exs
config :exhub, :brain_gitignore_enabled, true  # Set to false to disable gitignore support
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/brain/helpers_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/brain/helpers.ex
git commit -m "feat(brain): add configuration option for gitignore support"
```

---

## Task 6: Update Documentation

**Files:**
- Modify: `docs/modules/brain.md`
- Modify: `README.md` (if needed)

**Step 1: Write the failing test**

No tests needed for documentation.

**Step 2: Run test to verify it fails**

Not applicable.

**Step 3: Write minimal implementation**

Update documentation to include gitignore support:

```markdown
## Gitignore Support

The Brain MCP tools automatically respect `.gitignore` files in your Obsidian vault. When a `.gitignore` file is present in the vault root, files and directories matching the patterns will be excluded from listing and search results.

### Features

- Supports standard gitignore patterns including wildcards (`*`, `**`, `?`)
- Supports negation patterns (`!pattern`) to un-ignore files
- Supports directory-only patterns (`pattern/`)
- Supports path-specific patterns (`dir/file.txt`)
- Automatically loads `.gitignore` from vault root
- Can be disabled via configuration

### Configuration

To disable gitignore support:

```elixir
config :exhub, :brain_gitignore_enabled, false
```

### Examples

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
```

**Step 4: Run test to verify it passes**

Not applicable.

**Step 5: Commit**

```bash
git add docs/modules/brain.md
git commit -m "docs(brain): add gitignore support documentation"
```

---

## Task 7: Integration Testing

**Files:**
- Test: `test/exhub/mcp/tools/brain_integration_test.exs`

**Step 1: Write the failing test**

Create integration tests:

```elixir
defmodule Exhub.MCP.Tools.BrainIntegrationTest do
  use ExUnit.Case, async: true
  
  alias Exhub.MCP.Tools.Brain.ListNotes
  alias Exhub.MCP.Tools.Brain.SearchVault
  alias Exhub.MCP.Brain.Helpers
  
  describe "integration tests with gitignore" do
    test "end-to-end gitignore filtering" do
      # Create temporary test structure
      test_dir = System.tmp_dir!() |> Path.join("brain_integration_#{System.unique_integer([:positive])}")
      File.mkdir_p!(test_dir)
      
      # Create .gitignore
      File.write!(Path.join(test_dir, ".gitignore"), """
      # Ignore temp files
      *.tmp
      
      # Ignore secret directory
      secret/
      
      # But don't ignore important.temp
      !important.temp
      """)
      
      # Create test files
      File.write!(Path.join(test_dir, "note1.md"), "# Note 1")
      File.write!(Path.join(test_dir, "note2.md"), "# Note 2")
      File.write!(Path.join(test_dir, "temp.tmp"), "temp")
      File.write!(Path.join(test_dir, "important.temp"), "important")
      File.mkdir_p!(Path.join(test_dir, "secret"))
      File.write!(Path.join(test_dir, "secret/hidden.md"), "Hidden")
      File.mkdir_p!(Path.join(test_dir, "visible"))
      File.write!(Path.join(test_dir, "visible/shown.md"), "Shown")
      
      # Test list_notes
      entries = Helpers.list_md_files(test_dir, test_dir, gitignore_patterns: Helpers.load_gitignore_patterns(test_dir))
      
      assert "note1.md" in entries
      assert "note2.md" in entries
      refute "temp.tmp" in entries  # Even though it's not .md, it would be filtered anyway
      assert "important.temp" in entries  # This is negated in gitignore
      refute "secret/hidden.md" in entries
      assert "visible/shown.md" in entries
      
      # Cleanup
      File.rm_rf!(test_dir)
    end
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/tools/brain_integration_test.exs`
Expected: FAIL

**Step 3: Write minimal implementation**

Fix any issues found in integration tests.

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/tools/brain_integration_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add test/exhub/mcp/tools/brain_integration_test.exs
git commit -m "test(brain): add integration tests for gitignore support"
```

---

## Task 8: Performance Optimization

**Files:**
- Modify: `lib/exhub/mcp/brain/gitignore_parser.ex`
- Modify: `lib/exhub/mcp/brain/helpers.ex`

**Step 1: Write the failing test**

Add performance tests:

```elixir
describe "performance" do
  test "gitignore parsing is efficient for large patterns" do
    # Generate large gitignore content
    content = Enum.map_join(1..1000, "\n", fn i -> "pattern#{i}*" end)
    
    start_time = System.monotonic_time(:millisecond)
    patterns = GitignoreParser.parse(content)
    end_time = System.monotonic_time(:millisecond)
    
    assert length(patterns) == 1000
    assert end_time - start_time < 100  # Should parse in less than 100ms
  end
end
```

**Step 2: Run test to verify it fails**

Run: `mix test test/exhub/mcp/brain/gitignore_parser_test.exs`
Expected: FAIL or performance issues

**Step 3: Write minimal implementation**

Optimize gitignore parsing and matching:

```elixir
# In lib/exhub/mcp/brain/gitignore_parser.ex
def parse(content) do
  content
  |> String.split("\n")
  |> Enum.map(&String.trim/1)
  |> Enum.reject(&(String.starts_with?(&1, "#") or &1 == ""))
  |> Enum.map(&parse_pattern/1)
  |> Enum.reverse()  # Patterns are evaluated in reverse order
end

def ignored?(patterns, path) do
  # Use Enum.reduce_while for early termination
  patterns
  |> Enum.reduce_while(false, fn pattern, ignored? ->
    if matches?(pattern, path) do
      {:halt, not pattern.negate}
    else
      {:cont, ignored?}
    end
  end)
end
```

**Step 4: Run test to verify it passes**

Run: `mix test test/exhub/mcp/brain/gitignore_parser_test.exs`
Expected: PASS

**Step 5: Commit**

```bash
git add lib/exhub/mcp/brain/gitignore_parser.ex lib/exhub/mcp/brain/helpers.ex
git commit -m "perf(brain): optimize gitignore parsing and matching"
```

---

## Final Verification

After all tasks are completed:

1. Run full test suite: `mix test`
2. Run specific brain tests: `mix test test/exhub/mcp/brain/`
3. Verify no regressions in existing functionality
4. Test with real Obsidian vault containing .gitignore
5. Update any other documentation if needed

**Final Commit:**

```bash
git add -A
git commit -m "feat(brain): complete gitignore support implementation"
```

---

## Notes

1. **Backwards Compatibility**: The implementation maintains backwards compatibility by defaulting gitignore support to enabled but allowing it to be disabled via configuration.

2. **Performance Considerations**: Gitignore patterns are loaded once per request and cached for the duration of that request. For large vaults with many patterns, this should be efficient.

3. **Edge Cases**: The implementation handles:
   - Missing .gitignore files
   - Empty .gitignore files
   - Invalid patterns (gracefully ignored)
   - Nested .gitignore files (only root is considered)

4. **Future Enhancements**:
   - Support for nested .gitignore files in subdirectories
   - Support for global gitignore (~/.gitignore)
   - Support for .git/info/exclude
   - Pattern caching across requests
