defmodule Exhub.Memory.ReviewTest do
  use ExUnit.Case, async: false

  alias Exhub.Memory.Review
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

  test "approve/2 moves a candidate to approved" do
    {:ok, id, _} = Store.create(%{"status" => "candidate", "title" => "T"}, "a draft lesson")

    assert {:ok, meta} = Review.approve(id, reason: "reviewed")
    assert meta["status"] == "approved"
    assert meta["reviewed_at"] != nil
  end

  test "approve/2 refuses an empty lesson body" do
    {:ok, id, _} = Store.create(%{"status" => "candidate"}, " ")

    assert {:error, message} = Review.approve(id)
    assert message =~ "lesson body"
  end

  test "approve/2 refuses the placeholder body" do
    {:ok, id, _} = Store.create(%{"status" => "candidate"}, "no lesson text was extracted")

    assert {:error, message} = Review.approve(id)
    assert message =~ "lesson body"
  end

  test "approve/2 records the reviewed text over the draft" do
    {:ok, id, _} = Store.create(%{"status" => "candidate", "title" => "Draft"}, "draft")

    assert {:ok, _meta} = Review.approve(id, body: "Reviewed lesson", title: "Better title")

    assert {:ok, record} = Store.read(id)
    assert record.body == "Reviewed lesson"
    assert record.meta["title"] == "Better title"
  end

  test "approve/2 refuses possible secrets" do
    {:ok, id, _} = Store.create(%{"status" => "candidate"}, "key sk-abcdefghijklmnop1234")

    assert {:error, message} = Review.approve(id)
    assert message =~ "secret"
  end

  test "reject/2 marks the candidate rejected" do
    {:ok, id, _} = Store.create(%{"status" => "candidate"}, "lesson")
    assert {:ok, meta} = Review.reject(id, "not reusable")
    assert meta["status"] == "rejected"
  end

  test "supersede/3 links the old memory to its replacement" do
    {:ok, old, _} = Store.create(%{"status" => "approved"}, "old lesson")
    {:ok, new, _} = Store.create(%{"status" => "approved"}, "new lesson")

    assert {:ok, result} = Review.supersede(old, new, "covered by the new one")
    assert result.superseded == old

    assert {:ok, old_record} = Store.read(old)
    assert old_record.meta["status"] == "superseded"
    assert old_record.meta["superseded_by"] == new

    assert {:ok, new_record} = Store.read(new)
    assert new_record.meta["supersedes"] == old
  end

  test "unknown ids report not found" do
    assert {:error, message} = Review.approve("memory_missing")
    assert message =~ "not found"
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
