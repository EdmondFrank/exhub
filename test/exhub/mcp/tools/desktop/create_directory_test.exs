defmodule Exhub.MCP.Tools.Desktop.CreateDirectoryTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.CreateDirectory

  describe "execute/2" do
    setup do
      tmp_dir = Path.join(System.tmp_dir!(), "create_dir_test_#{:rand.uniform(999_999)}")
      File.mkdir_p!(tmp_dir)
      on_exit(fn -> File.rm_rf!(tmp_dir) end)
      {:ok, tmp_dir: tmp_dir}
    end

    test "creates a new directory", %{tmp_dir: tmp_dir} do
      dir_path = Path.join(tmp_dir, "new_dir")

      frame = %{}
      {:reply, resp, ^frame} = CreateDirectory.execute(%{path: dir_path}, frame)

      assert resp.isError == false
      assert File.dir?(dir_path)
    end

    test "creates nested directories (mkdir -p behaviour)", %{tmp_dir: tmp_dir} do
      dir_path = Path.join(tmp_dir, "a/b/c/d")

      frame = %{}
      {:reply, resp, ^frame} = CreateDirectory.execute(%{path: dir_path}, frame)

      assert resp.isError == false
      assert File.dir?(dir_path)
    end

    test "succeeds silently when directory already exists", %{tmp_dir: tmp_dir} do
      dir_path = Path.join(tmp_dir, "existing_dir")
      File.mkdir_p!(dir_path)

      frame = %{}
      {:reply, resp, ^frame} = CreateDirectory.execute(%{path: dir_path}, frame)

      assert resp.isError == false
      assert File.dir?(dir_path)
    end
  end
end
