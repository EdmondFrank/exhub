defmodule Exhub.Toonflow.Progress do
  @moduledoc """
  Best-effort live-progress fan-out for the Toonflow UI (Phase 5).

  A thin wrapper over a duplicate-key `Registry`
  (`Exhub.Toonflow.Progress.Registry`, supervised in `Application`). Pipeline
  stages publish `stage` / `shot` events here; the Toonflow websocket
  (`Exhub.Toonflow.SocketHandler`) subscribes per project and forwards them to
  the browser.

  Every function is defensive: if the registry is not running (unit tests, a
  partially-started VM) publishing is a no-op. A UI concern must never be able
  to break a pipeline run.
  """

  @registry Exhub.Toonflow.Progress.Registry

  @type event :: map()

  @doc "The registry name (used by the `Application` child spec and tests)."
  @spec registry() :: atom()
  def registry, do: @registry

  @doc "Subscribe the calling process to `project`'s progress events."
  @spec subscribe(String.t() | nil) :: :ok
  def subscribe(project) when is_binary(project) do
    _ = Registry.register(@registry, key(project), nil)
    :ok
  rescue
    _ -> :ok
  end

  def subscribe(_), do: :ok

  @doc "Stop receiving `project`'s progress events."
  @spec unsubscribe(String.t() | nil) :: :ok
  def unsubscribe(project) when is_binary(project) do
    _ = Registry.unregister(@registry, key(project))
    :ok
  rescue
    _ -> :ok
  end

  def unsubscribe(_), do: :ok

  @doc """
  Publish `event` to every process subscribed to `project`.

  Subscribers receive `{:toonflow_progress, project, event}`.
  """
  @spec broadcast(String.t() | nil, event()) :: :ok
  def broadcast(project, event) when is_binary(project) and is_map(event) do
    _ =
      Registry.dispatch(@registry, key(project), fn entries ->
        for {pid, _value} <- entries, do: send(pid, {:toonflow_progress, project, event})
      end)

    :ok
  rescue
    _ -> :ok
  end

  def broadcast(_project, _event), do: :ok

  @doc "Publish a stage lifecycle event (`\"started\"` / `\"ok\"` / `\"skipped\"` / `\"error\"`)."
  @spec stage_event(String.t() | nil, String.t(), String.t(), term()) :: :ok
  def stage_event(project, stage, status, detail \\ nil) do
    broadcast(project, %{
      "type" => "stage",
      "stage" => stage,
      "status" => status,
      "detail" => normalize(detail),
      "at" => now()
    })
  end

  @doc "Publish a per-shot media progress event."
  @spec shot_event(String.t() | nil, String.t(), String.t(), String.t(), term()) :: :ok
  def shot_event(project, stage, shot_id, status, detail \\ nil) do
    broadcast(project, %{
      "type" => "shot",
      "stage" => stage,
      "shot_id" => shot_id,
      "status" => status,
      "detail" => normalize(detail),
      "at" => now()
    })
  end

  @doc """
  An `on_progress` callback for `Exhub.Toonflow.HTTP.poll/4`.

  Async media generation exposes no percentage — a MoArk task reports only
  `status` (`queued` → `running` → `success`) plus when it started — so the
  honest live signal is a heartbeat: every poll tick becomes a `running` shot
  event carrying the provider status, the attempt number and the seconds
  elapsed since the callback was built. Without it the canvas hears `started`
  and then nothing for the 3–8 minutes a clip takes.

  Returns `nil` for a blank project, so a caller can pass it through
  unconditionally (`put_opt` / `Keyword.put` drop it and the poll simply does
  not report).
  """
  @spec poll_fn(String.t() | nil, String.t(), String.t()) ::
          (non_neg_integer(), map() -> :ok) | nil
  def poll_fn(project, stage, shot_id)
      when is_binary(project) and is_binary(stage) and is_binary(shot_id) do
    started = System.monotonic_time(:millisecond)

    fn attempt, payload ->
      shot_event(project, stage, shot_id, "running", %{
        "status" => status_of(payload),
        "attempt" => attempt,
        "elapsed_s" => elapsed_s(started)
      })
    end
  end

  def poll_fn(_project, _stage, _shot_id), do: nil

  @default_heartbeat_ms 10_000

  @doc """
  A timer heartbeat for a re-render that has no provider signal of its own.

  Only a clip can report its own progress: MoArk exposes a task status, so
  `poll_fn/3` turns every poll tick into a `running` shot event. An image, a
  keyframe or a voice line is one blocking request — the canvas hears `started`
  and then nothing until it ends, which reads as a hang. This publishes the same
  shape of event (`running` plus `elapsed_s`) on a fixed interval instead.

  `opts`: `:interval_ms` (default `#{@default_heartbeat_ms}`), `:status` (default
  `"generating"`), `:detail` (extra keys merged into every event — e.g. which of
  a two-frame keyframe render is in flight).

  Returns `nil` for a blank project or shot — the same passthrough convention as
  `poll_fn/3` — otherwise a 0-arity stop function. The stop is synchronous: once
  it returns no further event can be published, so a terminal `ok` / `error` is
  never followed by a stale `running` (which would reopen the canvas' badge).
  The heartbeat also dies with its caller, so a cancelled render leaves nothing
  behind.
  """
  @spec heartbeat(String.t() | nil, String.t(), String.t(), keyword()) :: (-> :ok) | nil
  def heartbeat(project, stage, shot_id, opts \\ [])

  def heartbeat(project, stage, shot_id, opts)
      when is_binary(project) and is_binary(stage) and is_binary(shot_id) do
    interval = Keyword.get(opts, :interval_ms, @default_heartbeat_ms)
    status = Keyword.get(opts, :status, "generating")
    extra = Keyword.get(opts, :detail) || %{}
    started = System.monotonic_time(:millisecond)
    owner = self()

    emit = fn ->
      detail = Map.merge(extra, %{"status" => status, "elapsed_s" => elapsed_s(started)})
      shot_event(project, stage, shot_id, "running", detail)
    end

    pid =
      spawn_link(fn ->
        # A heartbeat that outlived its render would keep re-opening the badge.
        owner_ref = Process.monitor(owner)
        emit.()
        beat(emit, interval, owner_ref)
      end)

    fn -> stop_heartbeat(pid) end
  end

  def heartbeat(_project, _stage, _shot_id, _opts), do: nil

  defp beat(emit, interval, owner_ref) do
    receive do
      :stop ->
        :ok

      {:DOWN, ^owner_ref, :process, _pid, _reason} ->
        :ok
    after
      interval ->
        emit.()
        beat(emit, interval, owner_ref)
    end
  end

  defp stop_heartbeat(pid) do
    ref = Process.monitor(pid)
    send(pid, :stop)

    receive do
      {:DOWN, ^ref, :process, ^pid, _reason} -> :ok
    end
  end

  @doc "Publish a job lifecycle event."
  @spec job_event(String.t() | nil, String.t(), String.t(), term()) :: :ok
  def job_event(project, job_id, status, detail \\ nil) do
    broadcast(project, %{
      "type" => "job",
      "job_id" => job_id,
      "status" => status,
      "detail" => normalize(detail),
      "at" => now()
    })
  end

  # Reason terms (`{:image_failed, :boom}`, an exception struct, …) must survive
  # JSON encoding in the socket. `Exhub.Toonflow.json_safe/1` is the shared rule,
  # and the place that explains why structs and tuples are the dangerous ones.
  defp normalize(detail), do: Exhub.Toonflow.json_safe(detail)

  defp status_of(%{"status" => status}) when is_binary(status), do: status
  defp status_of(_payload), do: "pending"

  defp elapsed_s(started), do: max(0, div(System.monotonic_time(:millisecond) - started, 1000))

  defp key(project), do: {:toonflow_progress, project}

  defp now do
    DateTime.utc_now() |> DateTime.truncate(:second) |> DateTime.to_iso8601()
  end
end
