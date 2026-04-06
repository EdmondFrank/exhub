defmodule Exhub.MCP.Tools.Desktop.WriteFileTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.WriteFile

  describe "execute/2" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "write_file_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "writes content to a new file (overwrite mode)", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "new_file.txt")

      frame = %{}
      {:reply, resp, ^frame} = WriteFile.execute(%{path: file_path, content: "hello world"}, frame)

      assert resp.isError == false
      assert File.read!(file_path) == "hello world"

      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "bytes_written: 11"
    end

    test "overwrites existing file content", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "existing.txt")
      File.write!(file_path, "old content")

      frame = %{}
      {:reply, resp, ^frame} = WriteFile.execute(%{path: file_path, content: "new content"}, frame)

      assert resp.isError == false
      assert File.read!(file_path) == "new content"
    end

    test "appends content to an existing file", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "append.txt")
      File.write!(file_path, "first ")

      frame = %{}
      {:reply, resp, ^frame} = WriteFile.execute(%{path: file_path, content: "second", mode: "append"}, frame)

      assert resp.isError == false
      assert File.read!(file_path) == "first second"
    end

    test "creates missing parent directories automatically", %{tmp_dir: tmp_dir} do
      file_path = Path.join(tmp_dir, "deeply/nested/dir/file.txt")

      frame = %{}
      {:reply, resp, ^frame} = WriteFile.execute(%{path: file_path, content: "nested content"}, frame)

      assert resp.isError == false
      assert File.read!(file_path) == "nested content"
    end

    test "returns error for invalid (empty) path" do
      frame = %{}
      {:reply, resp, ^frame} = WriteFile.execute(%{path: "", content: "test"}, frame)

      assert resp.isError == true
      text = resp.content |> Enum.find(&(Map.get(&1, "type") == "text")) |> Map.get("text")
      assert text =~ "Invalid path"
    end
  end
end
