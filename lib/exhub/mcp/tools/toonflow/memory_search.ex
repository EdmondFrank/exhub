defmodule Exhub.MCP.Tools.Toonflow.MemorySearch do
  @moduledoc """
  MCP Tool: `toonflow_memory_search` — semantically search a project's memory.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Memory

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_memory_search"

  @impl true
  def description do
    """
    Semantic (vector) search over a project's indexed memory notes. Run
    `toonflow_memory_index` first to export and embed the notes.

    Returns the closest chunks with a `similarity` score (1.0 = identical).
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:query, {:required, :string}, description: "Natural-language query.")
    field(:top_k, :integer, description: "Number of hits to return (default 5).")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)
    query = Helpers.opt(params, :query)
    top_k = Map.get(params, :top_k)

    case Memory.search(project, query: query, top_k: top_k) do
      {:ok, hits} ->
        resp =
          Response.tool()
          |> Response.structured(%{
            "query" => query,
            "results" => hits,
            "count" => length(hits),
            "success" => true
          })

        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
