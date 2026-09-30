defmodule Exhub.MCP.Tools.Toonflow.ListEvents do
  @moduledoc """
  MCP Tool: `toonflow_list_events` — query a project's event graph.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Events

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_list_events"

  @impl true
  def description do
    """
    List events from the project's event graph, optionally filtered by chapter
    or kind (plot, conflict, reveal, emotion, action, dialogue, setup).
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:chapter_id, :string, description: "Filter by chapter id.")
    field(:kind, :string, description: "Filter by event kind.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:chapter_id, Helpers.opt(params, :chapter_id))
      |> Helpers.put_opt(:kind, Helpers.opt(params, :kind))

    case Events.list_events(project, opts) do
      {:ok, events} ->
        resp =
          Response.tool() |> Response.structured(%{"events" => events, "count" => length(events)})

        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
