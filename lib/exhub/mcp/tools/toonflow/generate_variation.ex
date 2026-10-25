defmodule Exhub.MCP.Tools.Toonflow.GenerateVariation do
  @moduledoc """
  MCP Tool: `toonflow_generate_variation` — a character's wardrobe variation.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Assets

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_generate_variation"

  @impl true
  def description do
    """
    Create (or re-render) a wardrobe variation of a character and store its
    reference image.

    The image is conditioned on the character's base reference(s) with a
    "keep the face/hair/skin, change only the outfit" instruction, so the same
    actor can wear different clothes across shots without the face drifting.
    Give a shot the look with `toonflow_set_shot_variations`; the shot's frames
    are then conditioned on this variation instead of the base reference.
    Re-running with the same `character` + `name` updates the variation.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")

    field(:character, {:required, :string}, description: "Character name (as extracted).")

    field(:name, {:required, :string}, description: "Variation name, e.g. \"夜行衣\".")

    field(:visual_prompt, :string,
      description: "The outfit/look to render (default: a generic new look)."
    )

    field(:model, :string,
      description: "Image model (default: the configured media image_model)."
    )

    field(:size, :string, description: "Output size, e.g. 1024x1024 (default).")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:character, Helpers.opt(params, :character))
      |> Helpers.put_opt(:name, Helpers.opt(params, :name))
      |> Helpers.put_opt(:visual_prompt, Helpers.opt(params, :visual_prompt))
      |> Helpers.put_opt(:model, Helpers.opt(params, :model))
      |> Helpers.put_opt(:size, Helpers.opt(params, :size))

    case Assets.generate_variation(project, opts) do
      {:ok, variation} ->
        {:reply, Response.tool() |> Response.structured(Map.put(variation, "success", true)),
         frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
