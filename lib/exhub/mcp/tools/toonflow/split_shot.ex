defmodule Exhub.MCP.Tools.Toonflow.SplitShot do
  @moduledoc """
  MCP Tool: `toonflow_split_shot` — refine one shot into finer sub-shots.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Storyboard

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_split_shot"

  @impl true
  def description do
    """
    Split one over-packed shot into finer sub-shots with the director LLM
    (`storyboard.split.*`).

    The sub-shots replace the original **in place** — same order position, the
    script's shot indices resequenced — and inherit its scene, cast and outer
    keyframe prompts (the first sub-shot keeps the original's start frame, the
    last keeps its end frame), so start/end continuity survives the split. The
    replaced shot's generated assets (keyframes, frames, video, voice) are
    removed, since they described the coarser shot.

    Use `instructions` to steer the split ("拆成三个递进的镜头"), or follow up
    with `toonflow_prompt_set` on `storyboard.split.system`. Re-run
    `toonflow_generate_image` / `toonflow_generate_video` for the new sub-shots.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:shot_id, {:required, :string}, description: "The shot to split.")

    field(:instructions, :string, description: "How to split (free-form guidance).")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:shot_id, Helpers.opt(params, :shot_id))
      |> Helpers.put_opt(:instructions, Helpers.opt(params, :instructions))

    case Storyboard.split_shot(project, opts) do
      {:ok, summary} ->
        {:reply, Response.tool() |> Response.structured(Map.put(summary, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
