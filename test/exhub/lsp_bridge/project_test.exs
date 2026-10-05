defmodule Exhub.LspBridge.ProjectTest do
  # `async: false` — the config loader uses one global named ETS table.
  use ExUnit.Case, async: false

  alias Exhub.LspBridge.{Config, Project}

  setup do
    start_supervised!({Config, [name: :project_test_cfg]})

    dir = Path.join(System.tmp_dir!(), "exhub_proj_#{:erlang.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    on_exit(fn -> File.rm_rf(dir) end)
    {:ok, dir: dir}
  end

  test "find_project_root walks up to a projectFiles marker", %{dir: dir} do
    nested = Path.join([dir, "apps", "web", "lib"])
    File.mkdir_p!(nested)
    File.write!(Path.join(dir, "mix.exs"), "")

    file = Path.join(nested, "x.ex")
    File.write!(file, "")

    assert Project.find_project_root(file, ["mix.exs"]) == dir
    assert Project.find_project_root(file, ["no-such-project-file"]) == nil
  end

  test "project_path honours an explicit override", %{dir: dir} do
    file = Path.join(dir, "x.ex")
    assert Project.project_path(file, project_path: "/some/root") == "/some/root"
  end

  test "resolve a single server by name", %{dir: dir} do
    File.write!(Path.join(dir, "mix.exs"), "")
    file = Path.join(dir, "x.ex")

    assert {:ok, selection} = Project.resolve(file, single: "elixirLS")
    assert selection.profile == {:single, "elixirLS"}
    refute selection.multi
    assert [%Config{name: "elixirLS"}] = selection.servers
    assert selection.root == dir
  end

  test "resolve a single server by language id", %{dir: dir} do
    File.write!(Path.join(dir, "mix.exs"), "")

    assert {:ok, selection} =
             Project.resolve(Path.join(dir, "x.ex"), language_id: "elixir")

    assert selection.profile == {:single, "elixirLS"}
  end

  test "resolve a multi-server profile", %{dir: dir} do
    assert {:ok, selection} = Project.resolve(Path.join(dir, "x.py"), multi: "pyright_ruff")
    assert selection.multi
    assert selection.profile == {:multi, "pyright_ruff"}
    assert Enum.any?(selection.servers, &(&1.name == "pyright"))
    assert Enum.any?(selection.servers, &(&1.name == "ruff"))
  end

  test "elixirLS rejects a standalone file outside a project", %{dir: dir} do
    file = Path.join(dir, "x.ex")
    File.write!(file, "")

    assert {:error, {:unsupported_single_file, "elixirLS"}} =
             Project.resolve(file, single: "elixirLS")
  end

  test "unknown names and languages are reported", %{dir: dir} do
    file = Path.join(dir, "x.ex")

    assert {:error, {:unknown_server, "nope"}} = Project.resolve(file, single: "nope")

    assert {:error, {:no_server_for_language, "not-a-language"}} =
             Project.resolve(file, language_id: "not-a-language")

    assert {:error, :no_language} = Project.resolve(file, [])
  end

  test "ignored prefixes never resolve to a server" do
    path = Path.expand("~/.gem/specs/x.rb")
    assert Project.ignored_path?(path)
    assert {:error, {:ignored_path, ^path}} = Project.resolve(path, language_id: "ruby")

    refute Project.ignored_path?(Path.expand("~/code/proj/x.rb"))
  end
end
