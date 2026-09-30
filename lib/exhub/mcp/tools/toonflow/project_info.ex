defmodule Exhub.MCP.Tools.Toonflow.ProjectInfo do
  @moduledoc """
  MCP Tool: `toonflow_project_info` — project metadata and workspace statistics.
  """

  alias Anubis.Server.Response
  alias Exhub.Toonflow.Store

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_project_info"

  @impl true
  def description do
    """
    Get a Toonflow project's metadata (id, name, timestamps) plus its workspace
    statistics: the project directory, database path, existence, and file counts
    for novels, chapters, scripts, characters, storyboards, images, videos, audio
    and output.
    """
  end

  schema do
    field(:name, {:required, :string}, description: "Project name (or id) to inspect.")
  end

  @impl true
  def execute(params, frame) do
    case Store.project_info(Map.get(params, :name)) do
      {:ok, info} ->
        resp = Response.tool() |> Response.structured(info)
        {:reply, resp, frame}

      {:error, :not_found} ->
        resp =
          Response.tool()
          |> Response.error("Project not found: #{inspect(Map.get(params, :name))}")

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to read project: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
