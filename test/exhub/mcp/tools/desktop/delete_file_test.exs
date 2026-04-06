defmodule Exhub.MCP.Tools.Desktop.DeleteFileTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.DeleteFile

  describe "execute/2" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "delete_file_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "deletes a regular file", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "to_delete.txt")
      File.write!(file_path, "content")

      frame = %{}
      {:reply, resp, ^frame} = DeleteFile.execute(%{path: file_path}, frame)

      assert resp.isError == false
      refute File.exists?(file_path)
    end

    test "deletes an empty directory (recursive=false)", %{tmp_dir: tmp_dir} do
      dir_path = Path.join(tmp_dir, "empty_dir")
      File.mkdir_p!(dir_path)

      frame = %{}
      {:reply, resp, ^frame} = DeleteFile.execute(%{path: dir_path, recursive: false}, frame)

      assert resp.isError == false
      refute File.exists?(dir_path)
    end

    test "returns error when deleting non-empty directory without recursive", %{tmp_dir: tmp_dir} do
      dir_path = Path.join(tmp_dir, "non_empty_dir")
      File.mkdir_p!(dir_path)
      File.write!(Path.join(dir_path, "file.txt"), "content")

      frame = %{}
      {:reply, resp, ^frame} = DeleteFile.execute(%{path: dir_path, recursive: false}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "eexist"
      assert File.exists?(dir_path)
    end

    test "deletes non-empty directory with recursive=true", %{tmp_dir: tmp_dir} do
      dir_path = Path.join(tmp_dir, "full_dir")
      File.mkdir_p!(dir_path)
      File.write!(Path.join(dir_path, "file.txt"), "content")

      frame = %{}
      {:reply, resp, ^frame} = DeleteFile.execute(%{path: dir_path, recursive: true}, frame)

      assert resp.isError == false
      refute File.exists?(dir_path)
    end

    test "returns error for non-existent path" do
      frame = %{}
      {:reply, resp, ^frame} = DeleteFile.execute(%{path: "/nonexistent/path"}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "not found"
    end
  end
end
