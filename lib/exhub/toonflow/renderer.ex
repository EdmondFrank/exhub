defmodule Exhub.Toonflow.Renderer do
  @moduledoc """
  Re-render one shot's media after its properties were edited.

  The canvas inspector changes a shot's prompt, framing or line in place; the
  media generated from the old values stays on disk, and `Snapshot` flags it as
  stale. This re-runs the same generators the pipeline uses, per kind, and
  reports each one with a `shot` progress event so the canvas' live log shows
  what is happening: a clip reports on every provider poll tick, every other
  kind through a `Progress.heartbeat/4` timer, since a frame or a voice line is
  one blocking request with nothing to report in between.

  `start/4` runs the work in `Exhub.MCP.ToolTaskSupervisor`: a MiniMax clip takes
  minutes, so the HTTP request that asked for it must not be holding the render,
  and a hot reload or a closed browser tab must not cut it short.

  `normalize_kinds/1` and `kinds/0` are pure and unit-tested; `run/4` shells out
  to the image, video and voice generators.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Keyframe, Media, Progress, Store, Storyboard, Video, Voice}

  require Logger

  @kinds ~w(keyframe image video voice)
  @supervisor Exhub.MCP.ToolTaskSupervisor

  @doc "The shot media kinds the inspector can re-render."
  @spec kinds() :: [String.t()]
  def kinds, do: @kinds

  @doc """
  Normalize requested kinds: one name or a list, `"all"` for every kind, unknown
  values dropped. Pure.
  """
  @spec normalize_kinds(term()) :: [String.t()]
  def normalize_kinds("all"), do: @kinds
  def normalize_kinds(kind) when is_binary(kind), do: normalize_kinds([kind])

  def normalize_kinds(list) when is_list(list) do
    list
    |> Enum.map(&to_string/1)
    |> Enum.filter(&(&1 in @kinds))
    |> Enum.uniq()
    |> Enum.sort()
  end

  def normalize_kinds(_value), do: []

  @doc """
  Start a re-render in the background, after checking the shot exists.

  Returns `{:ok, pid}` or `{:error, reason}` (`:shot_not_found` for an unknown
  shot). `opts`: `:supervisor` (default `Exhub.MCP.ToolTaskSupervisor`).
  """
  @spec start(String.t(), String.t(), term(), keyword()) :: {:ok, pid()} | {:error, term()}
  def start(project, shot_id, kinds, opts \\ []) do
    with {:ok, _shot} <- Storyboard.get_shot(project, shot_id),
         kinds when kinds != [] <- normalize_kinds(kinds) do
      supervisor = Keyword.get(opts, :supervisor, @supervisor)
      Task.Supervisor.start_child(supervisor, fn -> run(project, shot_id, kinds) end)
    else
      [] -> {:error, :no_kinds}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Re-render `kinds` in order and report each with a `shot` progress event.

  Returns `{:ok, %{kind => {:ok, asset} | {:error, reason}}}`; one failing kind
  does not stop the others, since a stale clip is still worth rendering while its
  voice failed.
  """
  @spec run(String.t(), String.t(), term(), keyword()) :: {:ok, map()}
  def run(project, shot_id, kinds, opts \\ []) do
    server = Keyword.get(opts, :server, Store)

    results =
      kinds
      |> normalize_kinds()
      |> Enum.map(fn kind ->
        started = System.monotonic_time(:millisecond)
        Progress.shot_event(project, kind, shot_id, "started")
        stop = heartbeat(project, kind, shot_id)

        result =
          try do
            render(project, shot_id, kind, server)
          after
            if stop, do: stop.()
          end

        report(project, shot_id, kind, result, elapsed_s(started))
        {kind, result}
      end)

    {:ok, Map.new(results)}
  end

  # A clip reports its own progress (a `running` event per provider poll tick);
  # a frame or a voice line is a single blocking request, so the canvas would
  # otherwise see `started` and then silence until the render ends.
  defp heartbeat(_project, "video", _shot_id), do: nil

  defp heartbeat(project, kind, shot_id), do: Progress.heartbeat(project, kind, shot_id)

  # The progress event carries the reason to the browser, but the canvas' log is
  # ephemeral — log it too so a failed render can still be diagnosed once the tab
  # is closed (this lands in the `*exhub*` service log).
  defp report(project, shot_id, kind, result, elapsed_s) do
    case result do
      {:ok, _asset} ->
        Progress.shot_event(project, kind, shot_id, "ok", %{"elapsed_s" => elapsed_s})

      # A shot with no dialogue has no voice to render. Nothing failed, but the
      # badge must still close — an unhandled `{:skip, _}` used to take the task
      # down with a `CaseClauseError`, leaving it ticking until the next prune.
      {:skip, reason} ->
        Progress.shot_event(project, kind, shot_id, "ok", %{
          "skipped" => reason,
          "elapsed_s" => elapsed_s
        })

      {:error, reason} ->
        Logger.warning("[Toonflow.Renderer] #{kind} #{shot_id}: #{inspect(reason)}")

        Progress.shot_event(project, kind, shot_id, "error", %{
          "reason" => reason,
          "elapsed_s" => elapsed_s
        })
    end
  end

  defp elapsed_s(started), do: max(0, div(System.monotonic_time(:millisecond) - started, 1000))

  # `keyframes` renders a start frame always, and an end frame only when the shot
  # carries a prompt for one — that pair is what `fl2va` interpolates between.
  defp render(project, shot_id, "keyframe", server) do
    with {:ok, shot} <- Storyboard.get_shot(project, shot_id, server),
         {:ok, _start} <- Keyframe.generate(project, [shot_id: shot_id, type: "start"], server) do
      case Toonflow.blank((shot["meta"] || %{})["keyframe_end_prompt"]) do
        nil ->
          {:ok, nil}

        _prompt ->
          # Two frames are two generations; say which one is in flight.
          Progress.shot_event(project, "keyframe", shot_id, "running", %{
            "status" => "generating",
            "step" => "end"
          })

          Keyframe.generate(project, [shot_id: shot_id, type: "end"], server)
      end
    end
  end

  defp render(project, shot_id, "image", server),
    do: Media.generate_image(project, [shot_id: shot_id], server)

  defp render(project, shot_id, "video", server) do
    Video.generate_video(
      project,
      [shot_id: shot_id, on_progress: Progress.poll_fn(project, "video", shot_id)],
      server
    )
  end

  defp render(project, shot_id, "voice", server),
    do: Voice.generate_voice(project, [shot_id: shot_id], server)
end
