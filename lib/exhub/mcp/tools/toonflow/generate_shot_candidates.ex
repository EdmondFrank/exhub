defmodule Exhub.MCP.Tools.Toonflow.GenerateShotCandidates do
  @moduledoc """
  MCP Tool: `toonflow_generate_shot_candidates` — a candidate grid for a shot.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Media

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_generate_shot_candidates"

  @impl true
  def description do
    """
    Render a *candidate grid* for a shot: one image holding several alternative
    compositions, so you can pick a frame instead of regenerating blindly.

    `count` cells (default: the configured `media.candidates`, 9 → a 3×3 grid)
    are drawn in a single image and recorded with `kind` `"candidates"` and
    `meta.grid` describing the layout. Follow up with
    `toonflow_select_candidate` to crop a cell into the shot's keyframe.

    `prompt` overrides the shot's own prompt. No candidate image is used by the
    pipeline automatically — this is an opt-in director tool.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:shot_id, {:required, :string}, description: "Shot to propose candidates for.")

    field(:count, :integer,
      description: "Number of candidate cells (default: media.candidates, 9)."
    )

    field(:prompt, :string, description: "Override the shot's prompt.")
    field(:model, :string, description: "Image model (default: the configured image_model).")
    field(:size, :string, description: "Output size, e.g. 1024x1024 (default).")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:shot_id, Helpers.opt(params, :shot_id))
      |> Helpers.put_opt(:count, Map.get(params, :count))
      |> Helpers.put_opt(:prompt, Helpers.opt(params, :prompt))
      |> Helpers.put_opt(:model, Helpers.opt(params, :model))
      |> Helpers.put_opt(:size, Helpers.opt(params, :size))

    case Media.generate_candidates(project, opts) do
      {:ok, asset} ->
        {:reply, Response.tool() |> Response.structured(Map.put(asset, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
