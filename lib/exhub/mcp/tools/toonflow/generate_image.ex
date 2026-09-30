defmodule Exhub.MCP.Tools.Toonflow.GenerateImage do
  @moduledoc """
  MCP Tool: `toonflow_generate_image` — render a frame for a shot (or a prompt).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Media

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_generate_image"

  @impl true
  def description do
    """
    Generate a frame image for a storyboard shot (or from a free `prompt`) via
    the shared Gitee AI / moark image API, saving it under `assets/images/` and
    recording an asset row.

    Pass `shot_id` to build the prompt from the shot and apply character
    consistency (character references are conditioned through `i2i` when
    available); the returned `path` is the local image. `model`/`size` override
    the configured defaults.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:shot_id, :string, description: "Render this shot (builds the prompt/references).")
    field(:prompt, :string, description: "Free-form prompt (used as-is; alternative to shot_id).")

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
      |> Helpers.put_opt(:shot_id, Helpers.opt(params, :shot_id))
      |> Helpers.put_opt(:prompt, Helpers.opt(params, :prompt))
      |> Helpers.put_opt(:model, Helpers.opt(params, :model))
      |> Helpers.put_opt(:size, Helpers.opt(params, :size))

    case Media.generate_image(project, opts) do
      {:ok, asset} ->
        {:reply, Response.tool() |> Response.structured(Map.put(asset, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
