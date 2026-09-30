defmodule Exhub.MCP.Tools.Toonflow.MemoryAdd do
  @moduledoc """
  MCP Tool: `toonflow_memory_add` — record a project memory note.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Memory

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_memory_add"

  @impl true
  def description do
    """
    Add a memory note to a project — the director's persistent notes (tone,
    style, casting, continuity …). Notes are stored in the project database and
    can be exported and semantically searched with `toonflow_memory_index` /
    `toonflow_memory_search`, or recalled into generation stages via the
    pipeline's `recall` option.

    `meta` is an optional JSON object stored alongside the note.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Project name.")
    field(:text, {:required, :string}, description: "The note body (required).")

    field(:kind, :string,
      description: "Note kind, e.g. style / tone / casting (default \"note\")."
    )

    field(:title, :string, description: "Short title (default \"未命名\").")
    field(:meta, :string, description: "Optional JSON object with extra structured data.")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:kind, Helpers.opt(params, :kind))
      |> Helpers.put_opt(:title, Helpers.opt(params, :title))
      |> Helpers.put_opt(:text, Helpers.opt(params, :text))
      |> put_meta(Helpers.opt(params, :meta))

    case Memory.add_note(project, opts) do
      {:ok, note} ->
        resp = Response.tool() |> Response.structured(Map.put(note, "success", true))
        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end

  defp put_meta(opts, nil), do: opts

  defp put_meta(opts, json) do
    case Jason.decode(json) do
      {:ok, map} when is_map(map) -> Keyword.put(opts, :meta, map)
      _ -> opts
    end
  end
end
