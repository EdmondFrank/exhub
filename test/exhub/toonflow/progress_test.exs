defmodule Exhub.Toonflow.ProgressTest do
  use ExUnit.Case, async: false

  alias Exhub.Toonflow.Progress

  setup do
    start_supervised!({Registry, keys: :duplicate, name: Progress.registry()})
    :ok
  end

  test "broadcasts reach subscribers of the same project" do
    assert :ok = Progress.subscribe("demo")
    assert :ok = Progress.stage_event("demo", "novel", "started", %{"chapters" => 2})

    assert_receive {:toonflow_progress, "demo",
                    %{
                      "type" => "stage",
                      "stage" => "novel",
                      "status" => "started",
                      "detail" => %{"chapters" => 2}
                    }}
  end

  test "does not deliver to subscribers of another project" do
    Progress.subscribe("demo")
    Progress.broadcast("other", %{"type" => "stage"})

    refute_receive {:toonflow_progress, _project, _event}, 50
  end

  test "unsubscribe stops delivery" do
    Progress.subscribe("demo")
    Progress.unsubscribe("demo")
    Progress.shot_event("demo", "images", "sht_1", "ok")

    refute_receive {:toonflow_progress, _project, _event}, 50
  end

  test "handles several subscribers for one project" do
    parent = self()

    spawn_link(fn ->
      Progress.subscribe("demo")
      send(parent, :subscribed)

      receive do
        {:toonflow_progress, project, event} -> send(parent, {:forwarded, project, event})
        :stop -> :ok
      end
    end)

    assert_receive :subscribed, 500
    Progress.subscribe("demo")
    Progress.job_event("demo", "job_1", "success")

    assert_receive {:toonflow_progress, "demo", %{"type" => "job", "job_id" => "job_1"}}
    assert_receive {:forwarded, "demo", %{"type" => "job"}}, 200
  end

  test "normalizes non-JSON detail terms so frames stay encodable" do
    Progress.subscribe("demo")
    Progress.shot_event("demo", "images", "sht_1", "error", {:image_failed, :boom})

    assert_receive {:toonflow_progress, "demo", %{"detail" => detail}}
    assert detail == "{:image_failed, :boom}"
    assert {:ok, _} = Jason.encode(%{"detail" => detail})
  end

  test "normalizes nested maps and non-atom keys" do
    Progress.subscribe("demo")
    Progress.stage_event("demo", "events", "error", %{{:a, :b} => %{c: 1}})

    assert_receive {:toonflow_progress, "demo", %{"detail" => detail}}
    # Atom keys are kept (Jason renders them as strings); only non-key terms are inspected.
    assert detail == %{"{:a, :b}" => %{c: 1}}
    assert {:ok, json} = Jason.encode(%{"detail" => detail})
    assert json =~ ~s("c":1)
  end

  test "is a no-op for a nil project" do
    assert :ok = Progress.subscribe(nil)
    assert :ok = Progress.broadcast(nil, %{"type" => "stage"})
    refute_receive {:toonflow_progress, _project, _event}, 50
  end
end
