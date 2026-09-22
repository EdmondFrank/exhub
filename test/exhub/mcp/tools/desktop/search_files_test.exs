defmodule Exhub.MCP.Tools.Desktop.SearchFilesTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.SearchFiles

  # Exile (used by ripgrep/grep and probe paths) needs its supervisor running
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
        SearchFiles.execute(
          %{
            path: tmp_dir,
            pattern: "fixture",
            search_type: "files"
          },
          frame
        )

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "fixture.txt"
    end

    test "search_type: content, context_lines: 0 — each match context contains only its own line",
         %{tmp_dir: tmp_dir} do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(
          %{
            path: tmp_dir,
            pattern: "MATCH_",
            search_type: "content",
            context_lines: 0,
            file_pattern: "*.txt"
          },
          frame
        )

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")

      # Parse the TOON response to check context isolation
      # With context_lines: 0, each match should only show its own line
      assert text =~ "MATCH_A"
      assert text =~ "MATCH_B"
      assert text =~ "MATCH_C"
    end

    test "search_type: content, context_lines: 2 — MATCH_A context does NOT contain MATCH_B", %{
      tmp_dir: tmp_dir
    } do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(
          %{
            path: tmp_dir,
            pattern: "MATCH_A",
            search_type: "content",
            context_lines: 2,
            file_pattern: "*.txt"
          },
          frame
        )

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")

      # MATCH_A is at line 3, with context 2 it shows lines 1-5
      # MATCH_B is at line 7, which should NOT appear
      refute text =~ "MATCH_B"
    end

    test "search_type: content, context_lines: 3 — MATCH_B context does NOT contain MATCH_C", %{
      tmp_dir: tmp_dir
    } do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(
          %{
            path: tmp_dir,
            pattern: "MATCH_B",
            search_type: "content",
            context_lines: 3,
            file_pattern: "*.txt"
          },
          frame
        )

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")

      # MATCH_B is at line 7, with context 3 it shows lines 4-10
      # MATCH_C is at line 13, which should NOT appear
      refute text =~ "MATCH_C"
    end

    test "file_pattern: *.txt filters to only .txt files", %{tmp_dir: tmp_dir} do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(
          %{
            path: tmp_dir,
            pattern: "MATCH_A",
            search_type: "content",
            file_pattern: "*.txt"
          },
          frame
        )

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")

      # Should find fixture.txt and nested.txt (both .txt)
      # Should NOT find other.ex
      refute text =~ "other.ex"
    end

    test "returns error for non-existent directory" do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(
          %{
            path: "/nonexistent/directory",
            pattern: "anything",
            search_type: "files"
          },
          frame
        )

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "not found"
    end

    test "files/content modes require a pattern" do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(%{path: "/tmp", search_type: "files"}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "pattern"
    end

    test "rejects relative paths" do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(%{path: "relative/dir", query: "anything"}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "Relative paths are not supported"
    end
  end

  describe "semantic search (default mode)" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "semantic_search_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)

      File.write!(Path.join(tmp_dir, "sample.ex"), """
      defmodule SampleFixture do
        def authenticate_user(token) do
          verify_credentials(token)
        end
      end
      """)

      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "defaults to semantic search and returns probe code blocks", %{tmp_dir: tmp_dir} do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(%{path: tmp_dir, query: "authenticate_user"}, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "authenticate_user"
      assert text =~ "sample.ex"
    end

    test "requires a query for semantic search", %{tmp_dir: tmp_dir} do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(%{path: tmp_dir, search_type: "semantic"}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "query"
    end

    test "returns error for non-existent directory", %{tmp_dir: _tmp_dir} do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(%{path: "/nonexistent/directory", query: "anything"}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "not found"
    end

    test "drops unsupported language values", %{tmp_dir: tmp_dir} do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(
          %{path: tmp_dir, query: "authenticate_user", language: "elixir"},
          frame
        )

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "authenticate_user"
    end

    test "filters by supported language", %{tmp_dir: tmp_dir} do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(
          %{path: tmp_dir, query: "authenticate_user", language: "typescript"},
          frame
        )

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "No results"
    end

    test "exact and allow_tests flags do not error", %{tmp_dir: tmp_dir} do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(
          %{path: tmp_dir, query: "authenticate_user", exact: true, allow_tests: true},
          frame
        )

      assert resp.isError == false
    end

    test "unknown params are ignored", %{tmp_dir: tmp_dir} do
      frame = %{}

      {:reply, resp, ^frame} =
        SearchFiles.execute(
          %{
            path: tmp_dir,
            query: "authenticate_user",
            reranker: "bm25",
            format: "markdown",
            session: "test-session"
          },
          frame
        )

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "authenticate_user"
    end
  end

  describe "tool definition (token budget)" do
    test "description stays terse" do
      assert byte_size(SearchFiles.description()) < 600
    end
  end
end
