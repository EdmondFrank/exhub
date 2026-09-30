defmodule Exhub.Toonflow.SnapshotTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.{DB, Schema, Snapshot, Store, Workspace}

  setup do
    root = Path.join(System.tmp_dir!(), "toonflow_snapshot_#{System.unique_integer([:positive])}")
    File.mkdir_p!(root)
    server = :"toonflow_snapshot_store_#{System.unique_integer([:positive])}"
    {:ok, pid} = Store.start_link(root_dir: root, name: server)

    on_exit(fn ->
      if Process.alive?(pid), do: GenServer.stop(pid)
      File.rm_rf(root)
    end)

    %{root: root, server: server}
  end

  describe "build/2" do
    test "assembles project, plan, shots (with asset urls), jobs and outputs", %{
      root: root,
      server: server
    } do
      assert {:ok, _} = Store.create_project("demo", [description: "d"], server)

      image = write_file(root, "demo", "assets/images/sht_1.png", "PNG")
      write_file(root, "demo", "output/E01.mp4", "MP4")

      assert :ok =
               Store.run_project(
                 "demo",
                 fn conn ->
                   DB.execute(
                     conn,
                     "INSERT INTO shots (id, script_id, idx, scene, shot_desc, size, camera, lighting, motion, prompt, meta_json) " <>
                       "VALUES (?,?,?,?,?,?,?,?,?,?,?)",
                     [
                       "sht_1",
                       "scr_1",
                       1,
                       "祖宅",
                       "林昭立于门前",
                       "中景",
                       "平视",
                       "冷光",
                       "推",
                       "青衫少年立于门前",
                       Schema.encode_json(%{"dialogue" => "你好", "characters" => ["林昭"]})
                     ]
                   )

                   DB.execute(
                     conn,
                     "INSERT INTO assets (id, shot_id, character_id, kind, path, url, prompt, meta_json, created_at) " <>
                       "VALUES (?,?,?,?,?,?,?,?,?)",
                     [
                       "ast_1",
                       "sht_1",
                       nil,
                       "image",
                       image,
                       nil,
                       "p",
                       nil,
                       "2026-09-30T00:00:00Z"
                     ]
                   )
                 end,
                 server
               )

      assert {:ok, snapshot} = Snapshot.build("demo", server)

      assert snapshot["project"]["name"] == "demo"
      assert snapshot["exists"] == true
      assert is_integer(snapshot["counts"]["images"])

      stages = Enum.map(snapshot["plan"]["stages"], & &1["stage"])
      assert stages == Exhub.Toonflow.Pipeline.stages()

      assert [shot] = snapshot["shots"]
      assert shot["id"] == "sht_1"
      assert shot["description"] == "林昭立于门前"
      assert shot["dialogue"] == "你好"
      assert shot["characters"] == ["林昭"]
      assert shot["image"] == "/toonflow/media/demo/assets/images/sht_1.png"
      assert shot["video"] == nil

      assert [output] = snapshot["outputs"]
      assert output["name"] == "E01.mp4"
      assert output["url"] == "/toonflow/media/demo/output/E01.mp4"
      assert output["size"] > 0

      assert snapshot["jobs"] == []
      assert is_binary(snapshot["at"])
    end

    test "returns an error for an unknown project", %{server: server} do
      assert {:error, :not_found} = Snapshot.build("nope", server)
    end
  end

  describe "project_list/1" do
    test "summarizes projects with counts", %{server: server} do
      assert {:ok, _} = Store.create_project("one", [], server)

      assert {:ok, [project]} = Snapshot.project_list(server)
      assert project["name"] == "one"
      assert is_map(project["counts"])
    end
  end

  describe "resolve_media/3" do
    setup %{root: root} do
      File.mkdir_p!(Path.join([Workspace.project_dir(root, "demo"), "assets", "images"]))
      sample = write_file(root, "demo", "assets/images/a.png", "PNG")
      %{sample: sample}
    end

    test "accepts a real allow-listed file", %{root: root, sample: sample} do
      assert {:ok, resolved} = Snapshot.resolve_media("demo", "assets/images/a.png", root)
      assert resolved == Path.expand(sample)
    end

    test "rejects traversal, absolute paths and unknown files", %{root: root} do
      assert {:error, :invalid_path} = Snapshot.resolve_media("demo", "../toonflow.db", root)
      assert {:error, :invalid_path} = Snapshot.resolve_media("demo", "/etc/passwd", root)

      assert {:error, :not_found} =
               Snapshot.resolve_media("demo", "assets/images/missing.png", root)
    end

    test "rejects files outside the served directories", %{root: root} do
      write_file(root, "demo", "chapters/1.txt", "chapter")
      assert {:error, :forbidden} = Snapshot.resolve_media("demo", "chapters/1.txt", root)
    end

    test "rejects symlinks", %{root: root} do
      outside = Path.join(root, "outside.txt")
      File.write!(outside, "secret")
      link = Path.join([Workspace.project_dir(root, "demo"), "assets", "images", "evil.png"])
      File.ln_s!(outside, link)

      assert {:error, :invalid_path} =
               Snapshot.resolve_media("demo", "assets/images/evil.png", root)
    end
  end

  describe "asset_url/3" do
    test "rewrites an in-project path to a media URL" do
      dir = "/tmp/p"
      asset = %{"path" => "/tmp/p/assets/images/x.png", "url" => nil}
      assert Snapshot.asset_url(asset, "demo", dir) == "/toonflow/media/demo/assets/images/x.png"
    end

    test "falls back to the remote url, then nil" do
      assert Snapshot.asset_url(%{"path" => nil, "url" => "https://x/y.png"}, "demo", "/tmp/p") ==
               "https://x/y.png"

      assert Snapshot.asset_url(%{"path" => "/elsewhere/x.png", "url" => ""}, "demo", "/tmp/p") ==
               nil

      assert Snapshot.asset_url(nil, "demo", "/tmp/p") == nil
    end
  end

  defp write_file(root, name, rel, body) do
    path = Path.join(Workspace.project_dir(root, name), rel)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, body)
    path
  end
end
