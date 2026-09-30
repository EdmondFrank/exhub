defmodule Exhub.Toonflow.WorkspaceTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Workspace

  test "builds project paths under the given root" do
    root = "/tmp/tf-root"

    assert Workspace.workspaces_root(root) == "/tmp/tf-root/workspaces"
    assert Workspace.registry_db(root) == "/tmp/tf-root/toonflow.db"
    assert Workspace.project_dir(root, "demo") == "/tmp/tf-root/workspaces/demo"
    assert Workspace.project_db(root, "demo") == "/tmp/tf-root/workspaces/demo/index.db"
    assert Workspace.project_json(root, "demo") == "/tmp/tf-root/workspaces/demo/project.json"
    assert Workspace.novels_dir(root, "demo") == "/tmp/tf-root/workspaces/demo/novels"

    assert Workspace.assets_dir(root, "demo", "images") ==
             "/tmp/tf-root/workspaces/demo/assets/images"

    assert Workspace.output_dir(root, "demo") == "/tmp/tf-root/workspaces/demo/output"
  end

  test "valid_name?/1 accepts slugs and rejects everything else" do
    assert Workspace.valid_name?("my-drama")
    assert Workspace.valid_name?("drama_1")
    assert Workspace.valid_name?("a.b-c_d")

    refute Workspace.valid_name?("My Drama")
    refute Workspace.valid_name?("-leading")
    refute Workspace.valid_name?("")
    refute Workspace.valid_name?(nil)
    refute Workspace.valid_name?(:atom)
    refute Workspace.valid_name?(String.duplicate("a", 65))
  end

  test "slugify/1 normalizes arbitrary strings" do
    assert Workspace.slugify("My Drama!") == "my-drama"
    assert Workspace.slugify("  Spaces  ") == "spaces"
    assert Workspace.slugify("a__b..c") == "a__b..c"
  end

  test "project_subdirs/0 lists the expected directories" do
    subdirs = Workspace.project_subdirs()
    assert "novels" in subdirs
    assert "assets/images" in subdirs
    assert "output" in subdirs
  end
end
