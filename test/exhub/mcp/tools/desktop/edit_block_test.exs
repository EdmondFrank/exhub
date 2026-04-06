defmodule Exhub.MCP.Tools.Desktop.EditBlockTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.EditBlock

  describe "execute/2" do
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
        EditBlock.execute(%{
          file_path: file_path,
          old_string: "world",
          new_string: "elixir"
        }, frame)

      assert resp.isError == false
      assert File.read!(file_path) == "hello elixir"
    end

    test "returns error when old_string is not found (exact match)", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "hello world")

      frame = %{}
      {:reply, resp, ^frame} =
        EditBlock.execute(%{
          file_path: file_path,
          old_string: "nonexistent",
          new_string: "replacement"
        }, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "not found"
    end

    test "returns error when old_string is empty", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "hello world")

      frame = %{}
      {:reply, resp, ^frame} =
        EditBlock.execute(%{
          file_path: file_path,
          old_string: "",
          new_string: "replacement"
        }, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "cannot be empty"
    end

    test "returns error when occurrence count mismatches expected_replacements", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "foo bar foo baz foo")

      frame = %{}
      {:reply, resp, ^frame} =
        EditBlock.execute(%{
          file_path: file_path,
          old_string: "foo",
          new_string: "qux",
          expected_replacements: 1
        }, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "Expected 1 replacement"
      assert text =~ "found 3"
    end

    test "replaces multiple occurrences when expected_replacements matches count", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "foo bar foo baz foo")

      frame = %{}
      {:reply, resp, ^frame} =
        EditBlock.execute(%{
          file_path: file_path,
          old_string: "foo",
          new_string: "qux",
          expected_replacements: 3
        }, frame)

      assert resp.isError == false
      assert File.read!(file_path) == "qux bar qux baz qux"
    end

    test "handles CRLF line endings — search string with LF still matches CRLF file", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      # File with CRLF line endings
      File.write!(file_path, "line one\r\nline two\r\nline three")

      frame = %{}
      {:reply, resp, ^frame} =
        EditBlock.execute(%{
          file_path: file_path,
          old_string: "line two\nline three",
          new_string: "replaced\nnew line"
        }, frame)

      assert resp.isError == false
      content = File.read!(file_path)
      assert content =~ "replaced"
      assert content =~ "new line"
    end

    test "fuzzy match: when old_string is close but not exact, error message contains similarity percentage", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "the quick brown fox jumps over the lazy dog")

      frame = %{}
      {:reply, resp, ^frame} =
        EditBlock.execute(%{
          file_path: file_path,
          old_string: "the quick brown fox jumps over the lazy cat",
          new_string: "replacement"
        }, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "% similarity"
    end
  end
end
