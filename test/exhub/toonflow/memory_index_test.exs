defmodule Exhub.Toonflow.MemoryIndexTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.Memory.Index

  setup do
    root = Path.join(System.tmp_dir!(), "toonflow_idx_#{System.unique_integer([:positive])}")
    notes = Path.join([root, "workspaces", "projA", "memory", "notes"])
    File.mkdir_p!(notes)

    n1 = Path.join(notes, "n1.md")
    n2 = Path.join(notes, "n2.md")
    File.write!(n1, "# 基调\n\n悬疑冷峻的雨夜，青衫少年林昭。")
    File.write!(n2, "# 角色\n\n林昭是主角，性格孤傲。")

    server = :"toonflow_index_test_#{System.unique_integer([:positive])}"

    {:ok, pid} =
      Index.start_link(
        name: server,
        path: Path.join(root, "toonflow_index.db"),
        root: root,
        embedder: Exhub.Toonflow.MemoryIndexTest.Embedder
      )

    previous = Application.get_env(:exhub, :toonflow_index_server)
    Application.put_env(:exhub, :toonflow_index_server, server)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf(root)
      restore(:toonflow_index_server, previous)
    end)

    %{server: server, files: [n1, n2]}
  end

  test "indexes note files and searches within a project", %{files: files} do
    assert {:ok, summary} = Index.rebuild(files)
    assert summary["indexed"] == 2
    assert summary["failed"] == 0
    assert summary["chunks"] > 0

    assert Index.chunk_count() > 0
    assert Index.ready?()

    assert {:ok, hits} = Index.search("林昭", project: "projA", top_k: 3)
    assert hits != []
    assert Enum.all?(hits, &(&1["project"] == "projA"))
    assert Enum.all?(hits, &is_number(&1["similarity"]))

    assert {:ok, []} = Index.search("林昭", project: "projB")
  end

  test "re-embeds only changed files", %{files: [n1, n2]} do
    assert {:ok, %{"indexed" => 2}} = Index.rebuild([n1, n2])
    assert {:ok, %{"changed" => 0, "indexed" => 0}} = Index.rebuild([n1, n2])

    File.write!(n1, "# 基调\n\n改为明亮喜剧的基调。")
    assert {:ok, %{"changed" => 1, "indexed" => 1}} = Index.rebuild([n1, n2])
  end

  test "searching an empty index errors" do
    assert {:error, reason} = Index.search("anything")
    assert reason =~ "empty"
  end

  test "project_of derives the project name from a path" do
    assert Index.project_of("/root", "/root/workspaces/foo/memory/notes/x.md") == "foo"
    assert Index.project_of("/root", "/elsewhere/x.md") == nil
  end

  defp restore(key, nil), do: Application.delete_env(:exhub, key)
  defp restore(key, value), do: Application.put_env(:exhub, key, value)
end

defmodule Exhub.Toonflow.MemoryIndexTest.Embedder do
  @moduledoc false
  # Deterministic bag-of-graphemes embedding: each unique character maps to a
  # dimension, so texts sharing characters have higher cosine similarity. Dim 16.

  @dim 16

  def dimension, do: @dim

  def encode(text) when is_binary(text), do: {:ok, vector(text)}

  def encode_batch(texts) when is_list(texts), do: {:ok, Enum.map(texts, &vector/1)}

  defp vector(text) do
    text
    |> String.graphemes()
    |> Enum.reduce(List.duplicate(0.0, @dim), fn g, acc ->
      idx = :erlang.phash2(g, @dim)
      List.update_at(acc, idx, &(&1 + 1.0))
    end)
  end
end
