defmodule Exhub.MCP.Tools.Toonflow.ExtractEvents do
  @moduledoc """
  MCP Tool: `toonflow_extract_events` — LLM chapter event-graph extraction.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Events

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_extract_events"

  @impl true
  def description do
    """
    Extract a structured event graph from chapter(s) using the LLM.

    Reads each chapter, asks the model for typed events (plot/conflict/reveal/
    emotion/action/dialogue/setup with a summary, characters, location and
    importance), and stores them. Re-running on the same chapter replaces its
    events. Omit `chapter_id` to process all chapters (bounded by `limit`).
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:chapter_id, :string, description: "Extract only this chapter (default: all).")
    field(:limit, :integer, description: "Maximum number of chapters to process.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:chapter_id, Helpers.opt(params, :chapter_id))
      |> Helpers.put_opt(:limit, Helpers.opt(params, :limit))

    case Events.extract_events(project, opts) do
      {:ok, summary} ->
        resp = Response.tool() |> Response.structured(summary)
        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
