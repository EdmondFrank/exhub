defmodule Exhub.MCP.Tools.Desktop.ListDirectoryTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.ListDirectory

  describe "execute/2" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "list_dir_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      File.mkdir_p!(Path.join(tmp_dir, "subdir"))
      File.write!(Path.join(tmp_dir, "file1.txt"), "content")
      File.write!(Path.join(tmp_dir, "file2.ex"), "code")
      File.write!(Path.join([tmp_dir, "subdir", "nested.txt"]), "nested")

      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "lists immediate children (depth=0)", %{tmp_dir: tmp_dir} do
      frame = %{}
      {:reply, resp, ^frame} = ListDirectory.execute(%{path: tmp_dir}, frame)

      assert resp.isError == false
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "file1.txt"
      assert text =~ "file2.ex"
      assert text =~ "subdir/"
    end

    test "directories are suffixed with /", %{tmp_dir: tmp_dir} do
      frame = %{}
      {:reply, resp, ^frame} = ListDirectory.execute(%{path: tmp_dir}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "subdir/"
    end

    test "depth=1 recurses one level", %{tmp_dir: tmp_dir} do
      frame = %{}
      {:reply, resp, ^frame} = ListDirectory.execute(%{path: tmp_dir, depth: 1}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "nested.txt"
    end

    test "pattern filters entries (e.g. *.txt)", %{tmp_dir: tmp_dir} do
      frame = %{}
      {:reply, resp, ^frame} = ListDirectory.execute(%{path: tmp_dir, pattern: "*.txt"}, frame)

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "file1.txt"
      refute text =~ "file2.ex"
    end

    test "returns error for non-existent directory" do
      frame = %{}
      {:reply, resp, ^frame} = ListDirectory.execute(%{path: "/nonexistent/dir"}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "Directory not found"
    end

    test "returns error when path is a file", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "file1.txt")

      frame = %{}
      {:reply, resp, ^frame} = ListDirectory.execute(%{path: file_path}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "Not a directory"
    end
  end
end
