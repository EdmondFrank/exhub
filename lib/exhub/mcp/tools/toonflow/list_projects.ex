defmodule Exhub.MCP.Tools.Toonflow.ListProjects do
  @moduledoc """
  MCP Tool: `toonflow_list_projects` — list Toonflow workspace projects.
  """

  alias Anubis.Server.Response
  alias Exhub.Toonflow.Store

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_list_projects"

  @impl true
  def description do
    """
    List all Toonflow projects (AI short-drama workspaces), newest first.
    Returns each project's id, name, workspace path, metadata and timestamps.
    """
  end

  schema do
  end

  @impl true
  def execute(_params, frame) do
    case Store.list_projects() do
      {:ok, projects} ->
        resp =
          Response.tool()
          |> Response.structured(%{"projects" => projects, "count" => length(projects)})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to list projects: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
