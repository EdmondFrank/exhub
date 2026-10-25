defmodule Exhub.MCP.Tools.Toonflow.PipelinePlan do
  @moduledoc """
  MCP Tool: `toonflow_pipeline_plan` — pre-review the pipeline before running it.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Pipeline

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_pipeline_plan"

  @impl true
  def description do
    """
    Read-only pre-review of the pipeline: which stages are already satisfied
    (`done` / `partial` / `ready`), which stage runs next, and — per shot —
    whether the media stages still have work to do.

    Each shot entry is `{"idx", "link", "ready", "missing"}`, where `link` is how
    the clip is produced (`keyframe` — first/last frame interpolation, the
    default — `image` or `text`) and `missing` lists what is still absent. This
    is the cheap way to see the cost of a run before committing to it; note that
    candidate grids, variations and keyframe copies are opt-in and never counted
    as required.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
  end

  @impl true
  def execute(params, frame) do
    case Pipeline.plan(Map.get(params, :project)) do
      {:ok, plan} -> {:reply, Response.tool() |> Response.structured(plan), frame}
      {:error, reason} -> Helpers.error(frame, reason)
    end
  end
end