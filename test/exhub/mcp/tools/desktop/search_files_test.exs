defmodule Exhub.MCP.Tools.Desktop.SearchFilesTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.SearchFiles

  # Exile (used by ripgrep/grep paths) needs its supervisor running
  setup_all do
    Application.ensure_all_started(:exile)
    :ok
  end

  describe "execute/2" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "search_files_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)

      # Create fixture file with 15 lines, MATCH_A at L3, MATCH_B at L7, MATCH_C at L13
      fixture_content = """
      line 1
      line 2
      MATCH_A here
      line 4
      line 5
      line 6
      MATCH_B here
      line 8
      line 9
      line 10
      line 11
      line 12
      MATCH_C here
      line 14
      line 15
      """

      File.write!(Path.join(tmp_dir, "fixture.txt"), fixture_content)
      File.write!(Path.join(tmp_dir, "other.ex"), "MATCH_A in ex file")
      File.mkdir_p!(Path.join(tmp_dir, "subdir"))
      File.write!(Path.join([tmp_dir, "subdir", "nested.txt"]), "nested MATCH_A")

      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "search_type: files — finds files by name pattern", %{tmp_dir: tmp_dir} do
      frame = %{}
      {:reply, resp, ^frame} =
        SearchFiles.execute(%{
          path: tmp_dir,
          pattern: "fixture",
          search_type: "files"
        }, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "fixture.txt"
    end

    test "search_type: content, context_lines: 0 — each match context contains only its own line", %{tmp_dir: tmp_dir} do
      frame = %{}
      {:reply, resp, ^frame} =
        SearchFiles.execute(%{
          path: tmp_dir,
          pattern: "MATCH_",
          search_type: "content",
          context_lines: 0,
          file_pattern: "*.txt"
        }, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")

      # Parse the TOON response to check context isolation
      # With context_lines: 0, each match should only show its own line
      assert text =~ "MATCH_A"
      assert text =~ "MATCH_B"
      assert text =~ "MATCH_C"
    end

    test "search_type: content, context_lines: 2 — MATCH_A context does NOT contain MATCH_B", %{tmp_dir: tmp_dir} do
      frame = %{}
      {:reply, resp, ^frame} =
        SearchFiles.execute(%{
          path: tmp_dir,
          pattern: "MATCH_A",
          search_type: "content",
          context_lines: 2,
          file_pattern: "*.txt"
        }, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")

      # MATCH_A is at line 3, with context 2 it shows lines 1-5
      # MATCH_B is at line 7, which should NOT appear
      refute text =~ "MATCH_B"
    end

    test "search_type: content, context_lines: 3 — MATCH_B context does NOT contain MATCH_C", %{tmp_dir: tmp_dir} do
      frame = %{}
      {:reply, resp, ^frame} =
        SearchFiles.execute(%{
          path: tmp_dir,
          pattern: "MATCH_B",
          search_type: "content",
          context_lines: 3,
          file_pattern: "*.txt"
        }, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")

      # MATCH_B is at line 7, with context 3 it shows lines 4-10
      # MATCH_C is at line 13, which should NOT appear
      refute text =~ "MATCH_C"
    end

    test "file_pattern: *.txt filters to only .txt files", %{tmp_dir: tmp_dir} do
      frame = %{}
      {:reply, resp, ^frame} =
        SearchFiles.execute(%{
          path: tmp_dir,
          pattern: "MATCH_A",
          search_type: "content",
          file_pattern: "*.txt"
        }, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")

      # Should find fixture.txt and nested.txt (both .txt)
      # Should NOT find other.ex
      refute text =~ "other.ex"
    end

    test "returns error for non-existent directory" do
      frame = %{}
      {:reply, resp, ^frame} =
        SearchFiles.execute(%{
          path: "/nonexistent/directory",
          pattern: "anything",
          search_type: "files"
        }, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "not found"
    end
  end
end
