defmodule Exhub.MCP.Tools.Desktop.ReadFileTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.ReadFile

  describe "execute/2" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "read_file_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "reads a file and returns its content", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "line one\nline two\nline three")

      frame = %{}
      {:reply, resp, ^frame} = ReadFile.execute(%{path: file_path}, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "line one"
      assert text =~ "line two"
      assert text =~ "line three"
    end

    test "returns correct total_lines and lines_read", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "a\nb\nc\nd\ne")

      frame = %{}
      {:reply, resp, ^frame} = ReadFile.execute(%{path: file_path}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "total_lines: 5"
      assert text =~ "lines_read: 5"
    end

    test "offset skips the first N lines", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "first\nsecond\nthird\nfourth")

      frame = %{}
      {:reply, resp, ^frame} = ReadFile.execute(%{path: file_path, offset: 2}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      refute text =~ "first"
      refute text =~ "second"
      assert text =~ "third"
      assert text =~ "fourth"
    end

    test "length limits lines returned", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "test.txt")
      File.write!(file_path, "a\nb\nc\nd\ne")

      frame = %{}
      {:reply, resp, ^frame} = ReadFile.execute(%{path: file_path, length: 2}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "lines_read: 2"
      # content field should contain only the first 2 lines
      assert text =~ ~s(content: "a\\nb")
      refute text =~ ~s(content: "a\\nb\\nc")
    end

    test "returns error for non-existent file" do
      frame = %{}
      {:reply, resp, ^frame} = ReadFile.execute(%{path: "/nonexistent/path/file.txt"}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "File not found"
    end

    test "returns error for a directory path", %{tmp_dir: tmp_dir} do
      frame = %{}
      {:reply, resp, ^frame} = ReadFile.execute(%{path: tmp_dir}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "is a directory"
    end
  end
end
