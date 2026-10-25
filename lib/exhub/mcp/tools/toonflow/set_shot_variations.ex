defmodule Exhub.MCP.Tools.Toonflow.SetShotVariations do
  @moduledoc """
  MCP Tool: `toonflow_set_shot_variations` — pick a shot's wardrobe.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Storyboard

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_set_shot_variations"

  @impl true
  def description do
    """
    Choose which wardrobe variation of each character a shot uses.

    `selections` maps a character name to a variation name (or id), e.g.
    `{"林昭": "夜行衣"}`. The choice is stored on the shot
    (`meta.character_variations`) and conditions the shot's frames on that
    variation's reference image, so a costume change is per-shot instead of
    global. Pass `{}` to clear the selection. Returns the stored mapping.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:shot_id, {:required, :string}, description: "The shot to configure.")

    field(:selections, {:required, :map},
      description: "Map of character name to variation name (or id); {} clears."
    )
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)
    shot_id = Helpers.opt(params, :shot_id)
    selections = Helpers.opt(params, :selections) || %{}

    case Storyboard.set_shot_variations(project, shot_id, selections) do
      {:ok, result} ->
        {:reply, Response.tool() |> Response.structured(Map.put(result, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
