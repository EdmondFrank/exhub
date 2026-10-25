defmodule Exhub.MCP.Tools.Toonflow.WorldviewSet do
  @moduledoc """
  MCP Tool: `toonflow_worldview_set` — record the drama's worldview anchors.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Worldview

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_worldview_set"

  @impl true
  def description do
    """
    Record (or replace) a worldview anchor for the project: the map, region,
    period, art style, music style — whatever must stay fixed.

    Unlike `toonflow_memory_add` (which is recalled by similarity), the worldview
    is *always* injected into the script, assets and storyboard instructions, so
    every stage generates inside the same world. Use one note per topic via
    `title` (default "世界观"), e.g. title "美术风格".
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:text, {:required, :string}, description: "The anchors, e.g. \"架空唐代，岭南水乡，水墨动画风格\".")

    field(:title, :string, description: "Anchor topic (default \"世界观\").")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:text, Helpers.opt(params, :text))
      |> Helpers.put_opt(:title, Helpers.opt(params, :title))

    case Worldview.set(project, opts) do
      {:ok, note} ->
        {:reply, Response.tool() |> Response.structured(Map.put(note, "success", true)), frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
