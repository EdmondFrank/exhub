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

  test "normalizes a struct detail term without raising" do
    Progress.subscribe("demo")

    # A struct is a map but not enumerable, so the map clause of `normalize/1`
    # used to raise a `Protocol.UndefinedError` here — which, mid-run, took the
    # whole pipeline report down with it (an LLM transport timeout arrives as a
    # `%LangChain.LangChainError{}`).
    Progress.stage_event(
      "demo",
      "storyboard",
      "error",
      {:parse_failed, struct(LangChain.LangChainError, type: "unknown_error", message: "timeout")}
    )

    assert_receive {:toonflow_progress, "demo", %{"detail" => detail}}
    assert detail =~ "parse_failed"
    assert detail =~ "LangChainError"
    assert {:ok, _} = Jason.encode(%{"detail" => detail})
  end

  describe "poll_fn/3" do
    test "turns a poll tick into a running shot event" do
      Progress.subscribe("demo")
      fun = Progress.poll_fn("demo", "video", "sht_1")
      assert is_function(fun, 2)

      assert :ok = fun.(3, %{"status" => "running"})

      assert_receive {:toonflow_progress, "demo",
                      %{
                        "type" => "shot",
                        "stage" => "video",
                        "shot_id" => "sht_1",
                        "status" => "running",
                        "detail" => %{
                          "status" => "running",
                          "attempt" => 3,
                          "elapsed_s" => elapsed
                        }
                      }}

      assert is_integer(elapsed) and elapsed >= 0
    end

    test "defaults the provider status when the payload has none" do
      Progress.subscribe("demo")
      Progress.poll_fn("demo", "video", "sht_1").(0, %{})

      assert_receive {:toonflow_progress, "demo", %{"detail" => %{"status" => "pending"}}}
    end

    test "is nil for a blank project, so it can be passed through unconditionally" do
      assert Progress.poll_fn(nil, "video", "sht_1") == nil
      assert Progress.poll_fn("demo", "video", nil) == nil
    end
  end

  describe "heartbeat/4" do
    test "reports at once, then on every interval" do
      Progress.subscribe("demo")
      stop = Progress.heartbeat("demo", "image", "sht_1", interval_ms: 20)
      assert is_function(stop, 0)

      # The first event lands immediately, so the canvas' log is not silent
      # between `started` and the first tick.
      assert_receive {:toonflow_progress, "demo",
                      %{
                        "type" => "shot",
                        "stage" => "image",
                        "shot_id" => "sht_1",
                        "status" => "running",
                        "detail" => %{"status" => "generating", "elapsed_s" => elapsed}
                      }},
                     500

      assert is_integer(elapsed) and elapsed >= 0

      assert_receive {:toonflow_progress, "demo", %{"stage" => "image", "status" => "running"}},
                     500

      assert :ok = stop.()
    end

    test "merges extra detail, e.g. which step a render is on" do
      Progress.subscribe("demo")
      stop = Progress.heartbeat("demo", "keyframe", "sht_1", detail: %{"step" => "end"})

      assert_receive {:toonflow_progress, "demo",
                      %{"detail" => %{"step" => "end", "status" => "generating"}}},
                     500

      assert :ok = stop.()
    end

    test "stopping is synchronous, so nothing follows a terminal event" do
      Progress.subscribe("demo")
      stop = Progress.heartbeat("demo", "voice", "sht_1", interval_ms: 10)

      assert_receive {:toonflow_progress, "demo", %{"stage" => "voice"}}, 500

      assert :ok = stop.()
      # Whatever was published before the stop is already in this mailbox;
      # draining it makes a heartbeat that outlived the stop visible below.
      flush_mailbox()
      refute_receive {:toonflow_progress, _project, _event}, 100
    end

    test "is nil for a blank project or shot" do
      assert Progress.heartbeat(nil, "image", "sht_1") == nil
      assert Progress.heartbeat("demo", "image", nil) == nil
    end
  end

  test "is a no-op for a nil project" do
    assert :ok = Progress.subscribe(nil)
    assert :ok = Progress.broadcast(nil, %{"type" => "stage"})
    refute_receive {:toonflow_progress, _project, _event}, 50
  end

  defp flush_mailbox do
    receive do
      _message -> flush_mailbox()
    after
      0 -> :ok
    end
  end
end
