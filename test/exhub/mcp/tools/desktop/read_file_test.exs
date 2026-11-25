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

  describe "line anchors and digests" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "read_file_stamp_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "with_lines prefixes each line with its 1-based number", %{tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "a.txt")
      File.write!(path, "alpha\nbeta\ngamma")

      text = read_text(%{path: path, with_lines: true})

      assert text =~ ~s(content: "1|alpha\\n2|beta\\n3|gamma")
    end

    test "with_lines numbers from offset while offset stays 0-based", %{tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "a.txt")
      File.write!(path, "alpha\nbeta\ngamma")

      text = read_text(%{path: path, offset: 1, with_lines: true})

      assert text =~ ~s(content: "2|beta\\n3|gamma")
      refute text =~ "1|alpha"
    end

    test "returns a 12-char sha256 and mtime", %{tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "a.txt")
      File.write!(path, "alpha")

      text = read_text(%{path: path})

      assert text =~ ~r/sha256: "?[0-9a-f]{12}/
      assert text =~ ~r/mtime: \d+/
    end

    test "an unchanged since_hash returns metadata only", %{tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "a.txt")
      File.write!(path, "alpha\nbeta")

      sha = sha_of(read_text(%{path: path}))
      text = read_text(%{path: path, since_hash: sha})

      assert text =~ "unchanged: true"
      assert text =~ sha
      refute text =~ "content:"
      refute text =~ "alpha"
    end

    test "a stale since_hash still returns the content", %{tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "a.txt")
      File.write!(path, "alpha")

      sha = sha_of(read_text(%{path: path}))
      File.write!(path, "changed")
      text = read_text(%{path: path, since_hash: sha})

      refute text =~ "unchanged"
      assert text =~ "changed"
    end

    test "offset past the end returns zero lines", %{tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "a.txt")
      File.write!(path, "alpha\nbeta")

      text = read_text(%{path: path, offset: 50})

      assert text =~ "lines_read: 0"
    end

    test "a trailing newline does not add a phantom line", %{tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "a.txt")
      File.write!(path, "alpha\nbeta\n")

      text = read_text(%{path: path, with_lines: true})

      assert text =~ "total_lines: 2"
      assert text =~ ~s(content: "1|alpha\\n2|beta")
    end

    test "an empty file has no lines", %{tmp_dir: tmp_dir} do
      path = Path.join(tmp_dir, "empty.txt")
      File.write!(path, "")

      text = read_text(%{path: path})

      assert text =~ "total_lines: 0"
      assert text =~ "lines_read: 0"
    end
  end

  defp read_text(params) do
    {:reply, resp, %{}} = ReadFile.execute(params, %{})
    resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
  end

  defp sha_of(text) do
    [_, sha] = Regex.run(~r/sha256: "?([0-9a-f]{12})/, text)
    sha
  end
end
