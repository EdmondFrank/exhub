defmodule Exhub.Memory.RecallTest do
  use ExUnit.Case, async: false

  alias Exhub.Memory.Recall
  alias Exhub.Memory.Store

  setup do
    dir = tmp_dir()
    Application.put_env(:exhub, :obsidian_vault_path, dir)
    Application.put_env(:exhub, :memory, recall: [filter: false, limit: 5])

    on_exit(fn ->
      Application.delete_env(:exhub, :obsidian_vault_path)
      Application.delete_env(:exhub, :memory)
      File.rm_rf(dir)
    end)

    seed()
    {:ok, dir: dir}
  end

  test "returns only approved memory matched by every keyword" do
    results = Recall.search("kuri daemon", filter: false)

    assert length(results) == 1
    assert hd(results)["kind"] == "debugging_pattern"
    assert hd(results)["memory_id"] =~ "memory_"
    assert is_number(hd(results)["score"])
  end

  test "never returns candidates" do
    results = Recall.search("kuri", filter: false)
    assert Enum.all?(results, &(&1["status"] == "approved"))
    refute Enum.any?(results, &(&1["title"] == "Kuri candidate"))
  end

  test "ANDs the query terms" do
    assert Recall.search("kuri zzz", filter: false) == []
  end

  test "scopes by project (explicit field or project/<name> tag)" do
    assert length(Recall.search("tests", project: "exhub", filter: false)) == 1
    assert Recall.search("tests", project: "other", filter: false) == []
  end

  test "filters by kind" do
    assert [result] = Recall.search("", kind: "convention", filter: false)
    assert result["title"] =~ "targeted tests"
  end

  test "format/2 includes the memory id and title" do
    [result | _] = Recall.search("kuri daemon", filter: false)
    text = Recall.format([result], "kuri daemon")

    assert text =~ result["memory_id"]
    assert text =~ "kuri"
  end

  test "format/2 with no results says so" do
    assert Recall.format([], "x") =~ "No approved memory"
  end

  defp seed do
    Store.create(
      %{
        "status" => "approved",
        "kind" => "debugging_pattern",
        "title" => "Fix kuri daemon crash",
        "project" => "exhub",
        "tags" => ["project/exhub"],
        "applicability" => "when kuri daemon exits"
      },
      "Restart the kuri daemon after changing the config."
    )

    Store.create(
      %{
        "status" => "approved",
        "kind" => "convention",
        "title" => "Run targeted tests with --no-start",
        "project" => "exhub",
        "tags" => ["project/exhub"],
        "applicability" => "when running tests"
      },
      "Use mix test --no-start for targeted files."
    )

    Store.create(
      %{
        "status" => "candidate",
        "kind" => "workflow",
        "title" => "Kuri candidate",
        "project" => "exhub"
      },
      "kuri daemon draft, not approved"
    )
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
