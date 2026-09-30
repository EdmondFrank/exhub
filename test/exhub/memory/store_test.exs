defmodule Exhub.Memory.StoreTest do
  use ExUnit.Case, async: false

  alias Exhub.Memory.Store

  setup do
    dir = tmp_dir()
    Application.put_env(:exhub, :obsidian_vault_path, dir)

    on_exit(fn ->
      Application.delete_env(:exhub, :obsidian_vault_path)
      File.rm_rf(dir)
    end)

    {:ok, dir: dir}
  end

  test "create/read round-trips metadata and body" do
    assert {:ok, id, path} = Store.create(%{"title" => "T", "kind" => "workflow"}, "body text")
    assert File.exists?(path)

    assert {:ok, record} = Store.read(id)
    assert record.meta["memory_id"] == id
    assert record.meta["title"] == "T"
    assert record.meta["kind"] == "workflow"
    assert record.meta["status"] == "candidate"
    assert record.meta["created_at"] != nil
    assert record.body == "body text"
  end

  test "list/1 filters by status, kind and project" do
    {:ok, a, _} =
      Store.create(%{"status" => "approved", "kind" => "workflow", "project" => "exhub"}, "a")

    {:ok, b, _} = Store.create(%{"status" => "candidate", "kind" => "gotcha"}, "b")

    {:ok, c, _} =
      Store.create(
        %{"status" => "approved", "kind" => "workflow", "tags" => ["project/other"]},
        "c"
      )

    ids = fn opts -> Store.list(opts) |> Enum.map(& &1.meta["memory_id"]) |> Enum.sort() end

    assert ids.(status: "approved") == Enum.sort([a, c])
    assert ids.(status: "candidate") == [b]
    assert ids.(project: "exhub") == [a]
    assert ids.(project: "other") == [c]
  end

  test "update/3 merges frontmatter and keeps the body when none is given" do
    {:ok, id, _} = Store.create(%{"status" => "candidate"}, "draft")
    assert {:ok, meta} = Store.update(id, %{"status" => "approved"})
    assert meta["status"] == "approved"

    assert {:ok, record} = Store.read(id)
    assert record.meta["status"] == "approved"
    assert record.body == "draft"
  end

  test "read/1 returns :not_found for an unknown id" do
    assert {:error, :not_found} = Store.read("memory_missing")
  end

  defp tmp_dir do
    dir =
      Path.join(
        System.tmp_dir!(),
        "exhub_mem_" <> (:crypto.strong_rand_bytes(6) |> Base.encode16(case: :lower))
      )

    File.mkdir_p!(dir)
    dir
  end
end
