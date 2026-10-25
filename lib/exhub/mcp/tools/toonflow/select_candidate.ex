defmodule Exhub.MCP.Tools.Toonflow.SelectCandidate do
  @moduledoc """
  MCP Tool: `toonflow_select_candidate` — pick a candidate grid cell as a keyframe.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Keyframe

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_select_candidate"

  @impl true
  def description do
    """
    Promote one cell of a shot's candidate grid
    (`toonflow_generate_shot_candidates`) into the shot's start or end keyframe.

    Pass `cell` as the 0-based grid position (row-major, left-to-right,
    top-to-bottom — a 3×3 grid is 0..8) and it is cropped out with ffmpeg.
    Omit `cell` (or pass `"whole"`) to use the whole image — e.g. when a single
    candidate was generated.

    The result is recorded as a normal `keyframe_start` / `keyframe_end` asset,
    so the `videos` stage (`fl2va`) picks it up automatically. `candidates_id`
    selects a specific grid (default: the shot's latest).
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:shot_id, {:required, :string}, description: "Shot the keyframe belongs to.")

    field(:type, {:required, :string}, description: ~s(The keyframe to set: "start" or "end".))

    field(:cell, :string,
      description: "0-based grid cell (0..8), or \"whole\" for the whole image."
    )

    field(:candidates_id, :string, description: "A specific candidates asset id.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:shot_id, Helpers.opt(params, :shot_id))
      |> Helpers.put_opt(:type, Helpers.opt(params, :type))
      |> Helpers.put_opt(:cell, Helpers.opt(params, :cell))
      |> Helpers.put_opt(:candidates_id, Helpers.opt(params, :candidates_id))

    case Keyframe.select_candidate(project, opts) do
      {:ok, asset} ->
        {:reply, Response.tool() |> Response.structured(Map.put(asset, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
