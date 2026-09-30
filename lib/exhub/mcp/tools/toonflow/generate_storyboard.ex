defmodule Exhub.MCP.Tools.Toonflow.GenerateStoryboard do
  @moduledoc """
  MCP Tool: `toonflow_generate_storyboard` — script → shot list (DirectorAgent).
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Storyboard

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_generate_storyboard"

  @impl true
  def description do
    """
    Turn a script into an ordered shot list (分镜) using the DirectorAgent LLM
    call, grounded in the project's extracted character appearances.

    Each shot carries a scene, description, 景别 (`size`), lighting, 运镜
    (`motion`), the characters it features, and a text-to-image `prompt`.
    Uses the latest script or `script_id`; re-running replaces that script's
    shots (idempotent) and mirrors them to `storyboards/<script_id>.json`.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:script_id, :string, description: "Script to adapt (default: the latest script).")
    field(:instructions, :string, description: "Extra directing guidance (style, pacing).")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:script_id, Helpers.opt(params, :script_id))
      |> Helpers.put_opt(:instructions, Helpers.opt(params, :instructions))

    case Storyboard.generate_storyboard(project, opts) do
      {:ok, summary} ->
        {:reply, Response.tool() |> Response.structured(Map.put(summary, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
