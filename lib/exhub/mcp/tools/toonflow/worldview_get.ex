defmodule Exhub.MCP.Tools.Toonflow.WorldviewGet do
  @moduledoc """
  MCP Tool: `toonflow_worldview_get` — read the drama's worldview anchors.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Worldview

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_worldview_get"

  @impl true
  def description do
    """
    Read the project's worldview notes and the exact digest that is injected into
    the script/assets/storyboard instructions. Filter to one topic with `title`.
    Returns `count`, `notes` and `digest` (empty when nothing is set yet).
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:title, :string, description: "Filter to one anchor topic.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)
    title = Helpers.opt(params, :title)

    case Worldview.get(project, title: title) do
      {:ok, notes} ->
        summary = %{
          "count" => length(notes),
          "notes" => notes,
          "digest" => Worldview.digest(notes)
        }

        {:reply, Response.tool() |> Response.structured(summary), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
