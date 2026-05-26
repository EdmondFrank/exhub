defmodule Exhub.MCP.Tools.BrainIntegrationTest do
  use ExUnit.Case, async: true

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
      # Note: list_md_files only returns .md files, so non-.md files are filtered anyway
      refute "secret/hidden.md" in entries
      assert "visible/shown.md" in entries
      
      # Cleanup
      File.rm_rf!(test_dir)
    end
  end
end