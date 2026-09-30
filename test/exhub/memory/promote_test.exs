defmodule Exhub.Memory.PromoteTest do
  use ExUnit.Case, async: false

  alias Exhub.Memory.Promote
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

  test "promote/1 writes a SKILL.md with provenance", %{dir: dir} do
    {:ok, id, _} =
      Store.create(
        %{
          "status" => "approved",
          "title" => "Run tests with --no-start",
          "applicability" => "when running tests",
          "kind" => "convention",
          "project" => "exhub"
        },
        "Use mix test --no-start for targeted files."
      )

    assert {:ok, result} = Promote.promote(id)
    assert result.slug == "run-tests-with-no-start"

    full = Path.join(dir, result.path)
    assert File.exists?(full)

    content = File.read!(full)
    assert content =~ "exhub_memory_id: #{id}"
    assert content =~ "Use mix test --no-start for targeted files."
  end

  test "promote/1 refuses to overwrite without force", %{dir: _dir} do
    {:ok, id, _} = Store.create(%{"status" => "approved", "title" => "Skill"}, "lesson")

    assert {:ok, _} = Promote.promote(id)
    assert {:error, message} = Promote.promote(id)
    assert message =~ "already exists"
    assert {:ok, _} = Promote.promote(id, force: true)
  end

  test "promote/1 refuses non-approved memory" do
    {:ok, id, _} = Store.create(%{"status" => "candidate", "title" => "x"}, "y")
    assert {:error, message} = Promote.promote(id)
    assert message =~ "approved"
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
