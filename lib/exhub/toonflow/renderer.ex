defmodule Exhub.Toonflow.Renderer do
  @moduledoc """
  Re-render one shot's media after its properties were edited.

  The canvas inspector changes a shot's prompt, framing or line in place; the
  media generated from the old values stays on disk, and `Snapshot` flags it as
  stale. This re-runs the same generators the pipeline uses, per kind, and
  reports each one with a `shot` progress event so the canvas' live log shows
  what is happening.

  `start/4` runs the work in `Exhub.MCP.ToolTaskSupervisor`: a MiniMax clip takes
  minutes, so the HTTP request that asked for it must not be holding the render,
  and a hot reload or a closed browser tab must not cut it short.

  `normalize_kinds/1` and `kinds/0` are pure and unit-tested; `run/4` shells out
  to the image, video and voice generators.
  """

  alias Exhub.Toonflow
  alias Exhub.Toonflow.{Keyframe, Media, Progress, Store, Storyboard, Video, Voice}

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
        Progress.shot_event(project, kind, shot_id, "started")
        result = render(project, shot_id, kind, server)
        report(project, shot_id, kind, result)
        {kind, result}
      end)

    {:ok, Map.new(results)}
  end

  defp report(project, shot_id, kind, result) do
    case result do
      {:ok, _asset} -> Progress.shot_event(project, kind, shot_id, "ok")
      {:error, reason} -> Progress.shot_event(project, kind, shot_id, "error", reason)
    end
  end

  # `keyframes` renders a start frame always, and an end frame only when the shot
  # carries a prompt for one — that pair is what `fl2va` interpolates between.
  defp render(project, shot_id, "keyframe", server) do
    with {:ok, shot} <- Storyboard.get_shot(project, shot_id, server),
         {:ok, _start} <- Keyframe.generate(project, [shot_id: shot_id, type: "start"], server) do
      case Toonflow.blank((shot["meta"] || %{})["keyframe_end_prompt"]) do
        nil -> {:ok, nil}
        _prompt -> Keyframe.generate(project, [shot_id: shot_id, type: "end"], server)
      end
    end
  end

  defp render(project, shot_id, "image", server),
    do: Media.generate_image(project, [shot_id: shot_id], server)

  defp render(project, shot_id, "video", server),
    do: Video.generate_video(project, [shot_id: shot_id], server)

  defp render(project, shot_id, "voice", server),
    do: Voice.generate_voice(project, [shot_id: shot_id], server)
end
