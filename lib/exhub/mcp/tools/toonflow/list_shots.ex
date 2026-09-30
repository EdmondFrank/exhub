defmodule Exhub.MCP.Tools.Toonflow.ListShots do
  @moduledoc """
  MCP Tool: `toonflow_list_shots` — list the shots of a storyboard.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Storyboard

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_list_shots"

  @impl true
  def description do
    """
    List shots (分镜): index, scene, description, 景别 (`size`), lighting, 运镜
    (`motion`) and the text-to-image `prompt`. Populate with
    `toonflow_generate_storyboard`, then render frames with
    `toonflow_generate_image` (`shot_id` = a shot's `id`).
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:script_id, :string, description: "Restrict to one script's shots.")
    field(:scene, :string, description: "Restrict to one scene.")
    field(:limit, :integer, description: "Maximum number of shots to return.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:script_id, Helpers.opt(params, :script_id))
      |> Helpers.put_opt(:scene, Helpers.opt(params, :scene))
      |> Helpers.put_opt(:limit, Helpers.opt(params, :limit))

    case Storyboard.list_shots(project, opts) do
      {:ok, shots} ->
        summary = %{"count" => length(shots), "shots" => shots}
        {:reply, Response.tool() |> Response.structured(summary), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
