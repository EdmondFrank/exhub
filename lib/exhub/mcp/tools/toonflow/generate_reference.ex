defmodule Exhub.MCP.Tools.Toonflow.GenerateReference do
  @moduledoc """
  MCP Tool: `toonflow_generate_reference` — a character/scene/prop reference image.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Assets

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_generate_reference"

  @impl true
  def description do
    """
    Render a reference ("setup") image for a character, scene or prop and record
    it on that row's `refs`.

    Character references are wardrobe/face locks conditioned into later frames
    (`i2i`), and scene references condition every shot set in that scene — so
    `toonflow_generate_image` / `toonflow_generate_keyframe` come out consistent
    instead of re-inventing the look per shot. Run it after
    `toonflow_extract_assets`. Defaults to a kind-specific setup prompt
    (full-body neutral pose on a plain background for characters; empty
    establishing shot for scenes) built from the subject's appearance or
    description; pass `prompt` to override it entirely.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")

    field(:kind, {:required, :string}, description: "\"character\", \"scene\" or \"prop\".")

    field(:name, {:required, :string}, description: "The subject's name (as extracted).")

    field(:prompt, :string, description: "Full prompt override (default: the setup prompt).")

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
      |> Helpers.put_opt(:kind, Helpers.opt(params, :kind))
      |> Helpers.put_opt(:name, Helpers.opt(params, :name))
      |> Helpers.put_opt(:prompt, Helpers.opt(params, :prompt))
      |> Helpers.put_opt(:model, Helpers.opt(params, :model))
      |> Helpers.put_opt(:size, Helpers.opt(params, :size))

    case Assets.generate_reference(project, opts) do
      {:ok, reference} ->
        {:reply, Response.tool() |> Response.structured(Map.put(reference, "success", true)),
         frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
