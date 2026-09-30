defmodule Exhub.Toonflow.MemoryTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{Memory, Store}
  alias Exhub.Toonflow.Memory.Index

  setup do
    root = Path.join(System.tmp_dir!(), "toonflow_mem_#{System.unique_integer([:positive])}")
    store = :"toonflow_mem_store_#{System.unique_integer([:positive])}"
    {:ok, pid} = Store.start_link(root_dir: root, name: store)

    index = :"toonflow_mem_index_#{System.unique_integer([:positive])}"

    {:ok, ipid} =
      Index.start_link(
        name: index,
        path: Path.join(root, "toonflow_index.db"),
        root: root,
        embedder: Exhub.Toonflow.MemoryIndexTest.Embedder
      )

    previous = Application.get_env(:exhub, :toonflow_index_server)
    Application.put_env(:exhub, :toonflow_index_server, index)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      if Process.alive?(ipid), do: GenServer.stop(ipid)
      File.rm_rf(root)
      restore(:toonflow_index_server, previous)
    end)

    %{server: store, root: root}
  end

  test "exports notes to markdown, indexes and recalls them", %{server: server, root: root} do
    assert {:ok, _} = Store.create_project("mem", [], server)

    assert {:ok, _} =
             Memory.add_note(
               "mem",
               [kind: "style", title: "基调", text: "悬疑冷峻的雨夜，青衫少年林昭。"],
               server
             )

    assert {:ok, _} =
             Memory.add_note("mem", [kind: "cast", title: "主角", text: "林昭是主角，性格孤傲。"], server)

    assert {:ok, exported} = Memory.export_notes("mem", [], server)
    assert exported["exported"] == 2
    assert Enum.all?(exported["files"], &File.exists?/1)
    assert exported["dir"] == Path.join([root, "workspaces", "mem", "memory", "notes"])

    assert {:ok, summary} = Memory.index("mem", [], server)
    assert summary["indexed"] == 2

    assert {:ok, hits} = Memory.search("mem", [query: "林昭", top_k: 2], server)
    assert hits != []
    assert Enum.all?(hits, &(&1["project"] == "mem"))

    assert {:ok, text} = Memory.recall("mem", [query: "林昭"], server)
    assert text =~ "项目记忆"
  end

  test "recall is best-effort when the index is empty", %{server: server} do
    assert {:ok, _} = Store.create_project("mem2", [], server)
    assert {:ok, ""} = Memory.recall("mem2", [query: "林昭"], server)
  end

  test "search requires a query", %{server: server} do
    assert {:ok, _} = Store.create_project("mem3", [], server)
    assert {:error, :missing_query} = Memory.search("mem3", [], server)
  end

  test "recall with a blank query short-circuits" do
    assert {:ok, ""} = Memory.recall("whatever", [], :none)
  end

  defp restore(key, nil), do: Application.delete_env(:exhub, key)
  defp restore(key, value), do: Application.put_env(:exhub, key, value)
end
