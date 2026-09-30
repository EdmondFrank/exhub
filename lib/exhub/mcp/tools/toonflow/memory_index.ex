defmodule Exhub.MCP.Tools.Toonflow.MemoryIndex do
  @moduledoc """
  MCP Tool: `toonflow_memory_index` — export and (re)build the memory index.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Memory

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_memory_index"

  @impl true
  def description do
    """
    Export a project's memory notes to markdown and (re)build them into the
    shared `sqlite-vec` semantic index. Only notes whose content changed since
    the last build are re-embedded.

    Returns a summary (`scanned`, `changed`, `indexed`, `failed`, `chunks`).
    Requires the embedding API key configured under `:brain_rag`.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:kind, :string, description: "Only index notes of this kind.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)
    opts = Helpers.put_opt([], :kind, Helpers.opt(params, :kind))

    case Memory.index(project, opts) do
      {:ok, summary} ->
        resp = Response.tool() |> Response.structured(Map.put(summary, "success", true))
        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
