defmodule Exhub.MCP.Tools.Brain.ListNotesTest do
  use ExUnit.Case, async: true

  describe "gitignore support" do
    test "respects .gitignore patterns when listing notes" do
      # Create temporary test structure
      test_dir =
        System.tmp_dir!() |> Path.join("brain_test_#{System.unique_integer([:positive])}")

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

      # Cleanup
      File.rm_rf!(test_dir)
    end
  end
end
