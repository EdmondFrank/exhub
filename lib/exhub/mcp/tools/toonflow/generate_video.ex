defmodule Exhub.MCP.Tools.Toonflow.GenerateVideo do
  @moduledoc """
  MCP Tool: `toonflow_generate_video` — render a clip for a shot (or a prompt).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.{Jobs, Video}

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_generate_video"

  @impl true
  def description do
    """
    Generate a video clip for a storyboard shot (or from a free `prompt`) via the
    shared MoArk async video API, saving it under `assets/videos/` and recording
    an asset row. The call submits the task and polls until it finishes.

    `task` is `t2va` (text-to-video) or `fl2va` (first/last-frame-to-video). When
    omitted it defaults to `fl2va` if the shot already has a frame image (used as
    the first frame), else `t2va`. `duration_seconds` (4-15, default 6),
    `aspect_ratio`, `model` and `seed` override the configured defaults. Returns
    the local `path` of the clip.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:shot_id, :string, description: "Render this shot (builds the prompt/frame).")

    field(:prompt, :string, description: "Free-form prompt (used as-is; alternative to shot_id).")

    field(:task, :string, description: "t2va or fl2va (default: inferred from the shot).")

    field(:model, :string,
      description: "Video model (default: the configured media video_model)."
    )

    field(:duration_seconds, :integer, description: "Clip duration in seconds (4-15; default 6).")

    field(:aspect_ratio, :string, description: "e.g. 16:9 (default), 9:16, 1:1.")
    field(:seed, :integer, description: "Optional random seed.")

    field(:first_frame, :string,
      description:
        "Image URL, data URI or local path; required for fl2va when the shot has no frame."
    )
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:shot_id, Helpers.opt(params, :shot_id))
      |> Helpers.put_opt(:prompt, Helpers.opt(params, :prompt))
      |> Helpers.put_opt(:task, Helpers.opt(params, :task))
      |> Helpers.put_opt(:model, Helpers.opt(params, :model))
      |> Helpers.put_opt(:aspect_ratio, Helpers.opt(params, :aspect_ratio))
      |> Helpers.put_opt(:first_frame, Helpers.opt(params, :first_frame))
      |> Helpers.put_opt(:duration_seconds, Map.get(params, :duration_seconds))
      |> Helpers.put_opt(:seed, Map.get(params, :seed))

    job_params = %{"shot_id" => Map.get(params, :shot_id), "task" => Map.get(params, :task)}

    case Jobs.run(project, "video", job_params, fn -> Video.generate_video(project, opts) end) do
      {:ok, asset} ->
        {:reply, Response.tool() |> Response.structured(Map.put(asset, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
