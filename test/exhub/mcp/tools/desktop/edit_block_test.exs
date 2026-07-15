defmodule Exhub.MCP.Tools.Desktop.EditBlockTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.EditBlock

  # Helper to extract error/success text from response
  defp response_text(resp) do
    resp.content
    |> Enum.find(&(Map.get(&1, "type") == "text"))
    |> Map.get("text")
  end

  describe "execute/2 — basic operations" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "edit_block_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "replaces a single occurrence of old_string with new_string", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "hello world")

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "world",
            new_string: "elixir"
          },
          frame
        )

      assert resp.isError == false
      assert File.read!(file_path) == "hello elixir"
    end

    test "returns error when old_string is not found (exact match)", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "hello world")

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "nonexistent",
            new_string: "replacement"
          },
          frame
        )

      assert resp.isError == true
      assert response_text(resp) =~ "not found"
    end

    test "returns error when old_string is empty", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "hello world")

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "",
            new_string: "replacement"
          },
          frame
        )

      assert resp.isError == true
      assert response_text(resp) =~ "cannot be empty"
    end

    test "returns error when occurrence count mismatches expected_replacements", %{
      tmp_dir: tmp_dir
    } do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "foo bar foo baz foo")

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "foo",
            new_string: "qux",
            expected_replacements: 1
          },
          frame
        )

      assert resp.isError == true
      assert response_text(resp) =~ "Expected 1 replacement"
      assert response_text(resp) =~ "found 3"
    end

    test "replaces multiple occurrences when expected_replacements matches count", %{
      tmp_dir: tmp_dir
    } do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "foo bar foo baz foo")

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "foo",
            new_string: "qux",
            expected_replacements: 3
          },
          frame
        )

      assert resp.isError == false
      assert File.read!(file_path) == "qux bar qux baz qux"
    end

    test "handles CRLF line endings — search string with LF still matches CRLF file", %{
      tmp_dir: tmp_dir
    } do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "line one\r\nline two\r\nline three")

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "line two\nline three",
            new_string: "replaced\nnew line"
          },
          frame
        )

      assert resp.isError == false
      content = File.read!(file_path)
      assert content =~ "replaced"
      assert content =~ "new line"
    end

    test "returns error for nonexistent file", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "nonexistent.txt")

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "foo",
            new_string: "bar"
          },
          frame
        )

      assert resp.isError == true
      assert response_text(resp) =~ "File not found"
    end
  end

  describe "execute/2 — content size limits" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "edit_block_size_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "rejects old_string exceeding 100 KB limit", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "some content")

      # Create a string just over 100 KB
      oversized = String.duplicate("x", 100_001)

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: oversized,
            new_string: "replacement"
          },
          frame
        )

      assert resp.isError == true
      text = response_text(resp)
      assert text =~ "old_string is"
      assert text =~ "bytes"
      assert text =~ "limit: 100000"
    end

    test "rejects new_string exceeding 100 KB limit", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "some content")

      oversized = String.duplicate("y", 100_001)

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "content",
            new_string: oversized
          },
          frame
        )

      assert resp.isError == true
      text = response_text(resp)
      assert text =~ "new_string is"
      assert text =~ "bytes"
      assert text =~ "limit: 100000"
    end

    test "accepts strings at the 100 KB limit", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      # 100_000 bytes exactly — at the limit, should be accepted
      exact = String.duplicate("a", 100_000)
      File.write!(file_path, exact)

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: exact,
            new_string: "replaced"
          },
          frame
        )

      assert resp.isError == false
      assert File.read!(file_path) == "replaced"
    end
  end

  describe "execute/2 — fuzzy match with Jaro distance" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "edit_block_fuzzy_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "fuzzy match: high similarity (>=95%) auto-replaces with warning", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      # Create a distinctive string that Jaro will match well
      original = "The quick brown fox jumps over the lazy dog"
      File.write!(file_path, original)

      # One-word difference — Jaro similarity should be very high
      search = "The quick brown fox jumps over the lazy cat"

      # Verify our test setup: Jaro should be >= 0.95
      jaro = String.jaro_distance(original, search)
      assert jaro >= 0.95, "Expected Jaro >= 0.95, got #{jaro}"

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: search,
            new_string: "REPLACED"
          },
          frame
        )

      assert resp.isError == false
      text = response_text(resp)
      assert text =~ "WARNING: Fuzzy match used"
      assert text =~ "% similarity"
      assert File.read!(file_path) =~ "REPLACED"
    end

    test "fuzzy match: medium similarity (70%-95%) shows character-level diff", %{
      tmp_dir: tmp_dir
    } do
      file_path = Path.join(tmp_dir, "test.txt")
      original = "function calculateTotalPrice(items, taxRate, discount) {"
      File.write!(file_path, original)

      # Enough difference to be in the 70%-95% range
      search = "function calcTotalPrice(items, tax_rate, discount) {"

      # Verify our test setup
      jaro = String.jaro_distance(original, search)
      assert jaro >= 0.70, "Expected Jaro >= 0.70, got #{jaro}"
      assert jaro < 0.95, "Expected Jaro < 0.95, got #{jaro}"

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: search,
            new_string: "REPLACED"
          },
          frame
        )

      assert resp.isError == true
      text = response_text(resp)
      assert text =~ "similar passage was found"
      assert text =~ "% similarity"
      assert text =~ "Character-level diff"
      assert text =~ "{-"
      assert text =~ "{+"
      # File should NOT be modified
      assert File.read!(file_path) == original
    end

    test "fuzzy match: low similarity (<70%) reports not found", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      original = "aaaa bbbb cccc dddd eeee"
      File.write!(file_path, original)

      # Completely different — Jaro should be very low
      search = "xxxx yyyy zzzz"

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: search,
            new_string: "REPLACED"
          },
          frame
        )

      assert resp.isError == true
      text = response_text(resp)
      assert text =~ "String not found"
      assert text =~ "below the 70% threshold"
      assert File.read!(file_path) == original
    end

    test "fuzzy match: similarity percentage in response matches Jaro distance", %{
      tmp_dir: tmp_dir
    } do
      file_path = Path.join(tmp_dir, "test.txt")
      original = "abcdefghij"
      File.write!(file_path, original)

      # Two chars different out of 10 — Jaro should be high
      search = "abcdefghik"

      expected_jaro = String.jaro_distance(original, search)
      expected_pct = round(expected_jaro * 100)

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: search,
            new_string: "REPLACED"
          },
          frame
        )

      text = response_text(resp)
      assert text =~ "#{expected_pct}% similarity"
    end
  end

  describe "execute/2 — large file fuzzy bail-out" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "edit_block_large_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "skips fuzzy search when file exceeds 10 MB", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "large.txt")
      # Create a file just over 10 MB
      large_content = String.duplicate("x", 10_000_001)
      File.write!(file_path, large_content)

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "this string does not exist in the file",
            new_string: "replacement"
          },
          frame
        )

      assert resp.isError == true
      text = response_text(resp)
      assert text =~ "Fuzzy search was skipped"
      assert text =~ "9765 KB"
    end

    test "performs fuzzy search when file is under 10 MB", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "medium.txt")
      # 1 MB file with recognizable content at the end
      padding = String.duplicate(".", 1_000_000)
      content = padding <> "the quick brown fox"
      File.write!(file_path, content)

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "the quick brown fax",
            new_string: "REPLACED"
          },
          frame
        )

      # Should attempt fuzzy search (not bail out)
      text = response_text(resp)
      # Should contain similarity info (not the bail-out message)
      refute text =~ "Fuzzy search was skipped"
    end
  end

  describe "execute/2 — line-ending normalization" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "edit_block_eol_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "preserves CRLF line endings in output when file uses CRLF", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "crlf.txt")
      File.write!(file_path, "line1\r\nline2\r\nline3")

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "line2\nline3",
            new_string: "new2\nnew3"
          },
          frame
        )

      assert resp.isError == false
      content = File.read!(file_path)
      # Output should preserve CRLF
      assert content == "line1\r\nnew2\r\nnew3"
    end

    test "preserves LF line endings in output when file uses LF", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "lf.txt")
      File.write!(file_path, "line1\nline2\nline3")

      frame = %{}

      {:reply, resp, ^frame} =
        EditBlock.execute(
          %{
            file_path: file_path,
            old_string: "line2\nline3",
            new_string: "new2\nnew3"
          },
          frame
        )

      assert resp.isError == false
      content = File.read!(file_path)
      assert content == "line1\nnew2\nnew3"
    end
  end
end
