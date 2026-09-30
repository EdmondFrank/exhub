defmodule Exhub.Toonflow.StoreTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{Store, Workspace}

  setup do
    root = Path.join(System.tmp_dir!(), "toonflow_store_#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    name = :"toonflow_store_test_#{System.unique_integer([:positive])}"
    {:ok, pid} = Store.start_link(name: name, root_dir: root)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf(root)
    end)

    {:ok, root: root, server: name}
  end

  test "creates, lists and fetches a project", %{root: root, server: server} do
    assert {:ok, project} = Store.create_project("my-drama", [description: "demo"], server)
    assert project["name"] == "my-drama"
    assert project["id"] =~ ~r/^prj_/
    assert project["meta"] == %{"description" => "demo"}

    assert File.dir?(Workspace.project_dir(root, "my-drama"))
    assert File.exists?(Workspace.project_db(root, "my-drama"))
    assert File.exists?(Workspace.project_json(root, "my-drama"))
    assert File.dir?(Workspace.assets_dir(root, "my-drama", "images"))

    assert {:ok, [listed]} = Store.list_projects(server)
    assert listed["name"] == "my-drama"

    assert {:ok, fetched} = Store.get_project("my-drama", server)
    assert fetched["id"] == project["id"]

    assert {:ok, by_id} = Store.get_project(project["id"], server)
    assert by_id["name"] == "my-drama"
  end

  test "rejects duplicate and invalid names", %{server: server} do
    assert {:ok, _} = Store.create_project("dup", [], server)
    assert {:error, :already_exists} = Store.create_project("dup", [], server)
    assert {:error, {:invalid_name, "Bad Name"}} = Store.create_project("Bad Name", [], server)
  end

  test "project_info reports paths and counts", %{server: server} do
    {:ok, project} = Store.create_project("info", [], server)

    assert {:ok, info} = Store.project_info("info", server)
    assert info["project"]["id"] == project["id"]
    assert info["exists"] == true
    assert info["counts"]["novels"] == 0
  end

  test "unknown project is not found", %{server: server} do
    assert {:error, :not_found} = Store.get_project("nope", server)
  end

  test "empty registry lists no projects", %{server: server} do
    assert {:ok, []} = Store.list_projects(server)
  end
end
