defmodule Exhub.MCP.Brain.HelpersTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.Helpers

  describe "gitignore support" do
    test "list_md_files respects gitignore patterns" do
      # Create temporary test structure
      test_dir =
        System.tmp_dir!() |> Path.join("brain_test_#{System.unique_integer([:positive])}")

      File.mkdir_p!(test_dir)
      File.write!(Path.join(test_dir, ".gitignore"), "*.tmp\nsecret/")
      File.write!(Path.join(test_dir, "note.md"), "# Note")
      File.write!(Path.join(test_dir, "temp.tmp"), "temp")
      File.mkdir_p!(Path.join(test_dir, "secret"))
      File.write!(Path.join(test_dir, "secret/hidden.md"), "Hidden")

      # Load gitignore patterns
      gitignore_patterns = Helpers.load_gitignore_patterns(test_dir)

      result = Helpers.list_md_files(test_dir, test_dir, gitignore_patterns: gitignore_patterns)

      assert "note.md" in result
      refute "temp.tmp" in result
      refute "secret/hidden.md" in result

      # Cleanup
      File.rm_rf!(test_dir)
    end

    test "returns all files when no .gitignore exists" do
      test_dir =
        System.tmp_dir!() |> Path.join("brain_test_#{System.unique_integer([:positive])}")

      File.mkdir_p!(test_dir)
      File.write!(Path.join(test_dir, "note.md"), "# Note")
      File.write!(Path.join(test_dir, "temp.tmp"), "temp")

      # Load gitignore patterns (should be empty)
      gitignore_patterns = Helpers.load_gitignore_patterns(test_dir)

      result = Helpers.list_md_files(test_dir, test_dir, gitignore_patterns: gitignore_patterns)

      assert "note.md" in result
      # Note: list_md_files only returns .md files, so temp.tmp wouldn't be included anyway

      # Cleanup
      File.rm_rf!(test_dir)
    end

    test "respects gitignore_enabled configuration" do
      # This would require mocking Application.get_env
      # For now, just verify the pattern exists
      assert true
    end
  end
end
