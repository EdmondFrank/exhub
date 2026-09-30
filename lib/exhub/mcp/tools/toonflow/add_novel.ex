defmodule Exhub.MCP.Tools.Toonflow.AddNovel do
  @moduledoc """
  MCP Tool: `toonflow_add_novel` — ingest a novel and split it into chapters.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.Toonflow.Helpers
  alias Exhub.Toonflow.Novel

  use Anubis.Server.Component, type: :tool

  def name, do: "toonflow_add_novel"

  @impl true
  def description do
    """
    Ingest a novel into a Toonflow project and split it into chapters.

    Provide `path` (a local `.txt`/`.md` file, or `.pdf`/`.docx`/image which is
    extracted via Gitee AI OCR) or `text` (inline content). Chapters are
    detected from `第N章/回/节/篇/卷`, `Chapter N`, or Markdown headings; text
    without headings becomes a single "全文" chapter.

    Returns the novel id, chapter count and per-chapter metadata. Use
    `toonflow_list_chapters` to page through the result.
    """
  end

  schema do
    field(:project, {:required, :string}, description: "Target project name.")

    field(:path, :string,
      description: "Local path to a novel file (txt/md read directly; pdf/docx/images via OCR)."
    )

    field(:text, :string, description: "Inline novel text (alternative to `path`).")
    field(:title, :string, description: "Optional novel title (defaults to the file name).")
  end

  @impl true
  def execute(params, frame) do
    project = Map.get(params, :project)

    opts =
      []
      |> Helpers.put_opt(:path, Helpers.opt(params, :path))
      |> Helpers.put_opt(:text, Helpers.opt(params, :text))
      |> Helpers.put_opt(:title, Helpers.opt(params, :title))

    case Novel.add_novel(project, opts) do
      {:ok, result} ->
        resp = Response.tool() |> Response.structured(Map.put(result, "success", true))
        {:reply, resp, frame}

      {:error, reason} ->
        Helpers.error(frame, reason)
    end
  end
end
