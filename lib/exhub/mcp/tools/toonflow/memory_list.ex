defmodule Exhub.MCP.Tools.Toonflow.MemoryList do
  @moduledoc """
  MCP Tool: `toonflow_memory_list` — list a project's memory notes.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Memory

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_memory_list"

  @impl true
  def description do
    """
    List a project's memory notes, newest first. Optionally filter by `kind`
    and cap the number returned with `limit`.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:kind, :string, description: "Only list notes of this kind.")
    field(:limit, :integer, description: "Maximum number of notes to return.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:kind, Helpers.opt(params, :kind))
      |> Helpers.put_opt(:limit, Map.get(params, :limit))

    case Memory.list_notes(project, opts) do
      {:ok, notes} ->
        resp =
          Response.tool()
          |> Response.structured(%{"notes" => notes, "count" => length(notes), "success" => true})

        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
