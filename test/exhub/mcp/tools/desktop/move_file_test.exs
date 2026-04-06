defmodule Exhub.MCP.Tools.Desktop.MoveFileTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.MoveFile

  describe "execute/2" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "move_file_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "moves a file to a new path", %{tmp_dir: tmp_dir} do
      source = Path.join(tmp_dir, "source.txt")
      dest = Path.join(tmp_dir, "dest.txt")
      File.write!(source, "move me")

      frame = %{}
      {:reply, resp, ^frame} = MoveFile.execute(%{source: source, destination: dest}, frame)

      assert resp.isError == false
      refute File.exists?(source)
      assert File.read!(dest) == "move me"
    end

    test "renames a file within the same directory", %{tmp_dir: tmp_dir} do
      source = Path.join(tmp_dir, "old_name.txt")
      dest = Path.join(tmp_dir, "new_name.txt")
      File.write!(source, "rename me")

      frame = %{}
      {:reply, resp, ^frame} = MoveFile.execute(%{source: source, destination: dest}, frame)

      assert resp.isError == false
      refute File.exists?(source)
      assert File.read!(dest) == "rename me"
    end

    test "creates missing destination parent directories", %{tmp_dir: tmp_dir} do
      source = Path.join(tmp_dir, "file.txt")
      dest = Path.join(tmp_dir, "new/nested/dir/file.txt")
      File.write!(source, "nested move")

      frame = %{}
      {:reply, resp, ^frame} = MoveFile.execute(%{source: source, destination: dest}, frame)

      assert resp.isError == false
      refute File.exists?(source)
      assert File.read!(dest) == "nested move"
    end

    test "returns error when source does not exist", %{tmp_dir: tmp_dir} do
      dest = Path.join(tmp_dir, "dest.txt")

      frame = %{}
      {:reply, resp, ^frame} = MoveFile.execute(%{source: "/nonexistent/file.txt", destination: dest}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "not found"
    end
  end
end
