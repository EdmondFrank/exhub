defmodule Exhub.MCP.Tools.Toonflow.GenerateKeyframe do
  @moduledoc """
  MCP Tool: `toonflow_generate_keyframe` — render a shot's start or end frame.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Keyframe

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_generate_keyframe"

  @impl true
  def description do
    """
    Render one keyframe — the first or last frame — of a storyboard shot,
    saving it under `assets/images/` as an asset of kind `keyframe_start` or
    `keyframe_end`.

    Keyframes drive `fl2va` video: `toonflow_generate_video` uses the shot's
    start frame as the first frame and its end frame as the interpolation
    target. The prompt resolves as `prompt` → the shot's
    `keyframe_<type>_prompt` (written by the storyboard) → the shot's own
    prompt/description with character consistency applied. Returns the local
    `path` of the frame.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:shot_id, {:required, :string}, description: "The shot to render a keyframe for.")

    field(:type, {:required, :string}, description: "\"start\" or \"end\".")

    field(:prompt, :string, description: "Override the shot's keyframe prompt.")
    field(:refs, {:list, :string}, description: "Extra reference images for i2i.")

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
      |> Helpers.put_opt(:type, Helpers.opt(params, :type))
      |> Helpers.put_opt(:prompt, Helpers.opt(params, :prompt))
      |> put_refs(Map.get(params, :refs))
      |> Helpers.put_opt(:model, Helpers.opt(params, :model))
      |> Helpers.put_opt(:size, Helpers.opt(params, :size))

    case Keyframe.generate(project, opts) do
      {:ok, asset} ->
        {:reply, Response.tool() |> Response.structured(Map.put(asset, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end

  defp put_refs(opts, refs) when is_list(refs) and refs != [], do: Keyword.put(opts, :refs, refs)
  defp put_refs(opts, _refs), do: opts
end
