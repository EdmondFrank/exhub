defmodule Exhub.MCP.Tools.Toonflow.ListChapters do
  @moduledoc """
  MCP Tool: `toonflow_list_chapters` — list a project's chapters.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Novel

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_list_chapters"

  @impl true
  def description do
    """
    List the chapters of a Toonflow project as lightweight summaries (id, index,
    title, text length and a preview). Full chapter text is not returned.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:novel_id, :string, description: "Restrict to one novel id.")
    field(:limit, :integer, description: "Maximum number of chapters to return.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:novel_id, Helpers.opt(params, :novel_id))
      |> Helpers.put_opt(:limit, Helpers.opt(params, :limit))

    case Novel.list_chapters(project, opts) do
      {:ok, chapters} ->
        resp =
          Response.tool()
          |> Response.structured(%{"chapters" => chapters, "count" => length(chapters)})

        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
