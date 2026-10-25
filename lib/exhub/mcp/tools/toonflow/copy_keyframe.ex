defmodule Exhub.MCP.Tools.Toonflow.CopyKeyframe do
  @moduledoc """
  MCP Tool: `toonflow_copy_keyframe` — carry a frame across a shot cut.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Keyframe

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_copy_keyframe"

  @impl true
  def description do
    """
    Copy an adjacent shot's keyframe onto this shot, so the cut does not jump.

    `direction: "prev_end"` copies the **previous** shot's end frame into this
    shot's **start** frame (a match cut on the incoming edge);
    `direction: "next_start"` copies the **next** shot's start frame into this
    shot's **end** frame. The image is duplicated locally, so regenerating the
    neighbour does not change the copy. Returns the new asset.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:shot_id, {:required, :string}, description: "The shot that receives the copied frame.")

    field(:direction, {:required, :string},
      description:
        "\"prev_end\" (copy previous end frame) or \"next_start\" (copy next start frame)."
    )
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:shot_id, Helpers.opt(params, :shot_id))
      |> Helpers.put_opt(:direction, Helpers.opt(params, :direction))

    case Keyframe.copy(project, opts) do
      {:ok, asset} ->
        {:reply, Response.tool() |> Response.structured(Map.put(asset, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
