defmodule Exhub.MCP.Tools.Toonflow.ListProps do
  @moduledoc """
  MCP Tool: `toonflow_list_props` — list the project's props.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Assets

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_list_props"

  @impl true
  def description do
    """
    List the props extracted from a project's script(s): name, description and
    reference images. Populate with `toonflow_extract_assets` and give a prop its
    look with `toonflow_generate_reference` (kind `prop`).
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:name, :string, description: "Filter to a single prop name.")
    field(:limit, :integer, description: "Maximum number of props to return.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:name, Helpers.opt(params, :name))
      |> Helpers.put_opt(:limit, Helpers.opt(params, :limit))

    case Assets.list_props(project, opts) do
      {:ok, props} ->
        {:reply,
         Response.tool() |> Response.structured(%{"count" => length(props), "props" => props}),
         frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
