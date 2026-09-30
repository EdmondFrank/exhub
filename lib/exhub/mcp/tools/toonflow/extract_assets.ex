defmodule Exhub.MCP.Tools.Toonflow.ExtractAssets do
  @moduledoc """
  MCP Tool: `toonflow_extract_assets` — cast/scene/prop extraction from a script.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Assets

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_extract_assets"

  @impl true
  def description do
    """
    Extract the cast (characters with stable appearances), scenes and props from
    a script, building the project's appearance database.

    Uses the latest script, or `script_id` when given. Re-running updates
    characters by name (idempotent) and mirrors the extraction to
    `characters/appearance.json`. Feed the result into `toonflow_generate_storyboard`
    (it grounds shots in these characters) and `toonflow_generate_image`
    (consistency via `i2i`).
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:script_id, :string, description: "Script to read (default: the latest script).")
    field(:instructions, :string, description: "Extra extraction guidance.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:script_id, Helpers.opt(params, :script_id))
      |> Helpers.put_opt(:instructions, Helpers.opt(params, :instructions))

    case Assets.extract_assets(project, opts) do
      {:ok, summary} ->
        {:reply, Response.tool() |> Response.structured(summary), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
