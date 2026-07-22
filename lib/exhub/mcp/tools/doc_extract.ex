defmodule Exhub.MCP.Tools.DocExtract do
  @moduledoc """
  MCP Tool for extracting and recognizing text from documents (PDF, DOCX, etc.)
  using the Gitee AI Async Document Parse API (Unlimited-OCR / PaddleOCR-VL-1.5).

  Supports both local file paths and remote URLs as input.
  The extraction is asynchronous: the tool submits a task, polls until
  completion, then returns the full markdown-formatted text.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Tools.DocExtract.Client

  use Anubis.Server.Component, type: :tool

  def name, do: "doc_extract"

  @impl true
  def description do
    """
    Extract and recognize text from documents (PDF, DOCX, images, etc.) using Gitee AI.

    Powered by Unlimited-OCR (default) or PaddleOCR-VL-1.5 via the Gitee AI Async Document Parse API.
    Supports both local file paths and remote URLs as input.

    The tool submits an async parsing task, waits for completion, then returns
    the extracted content in Markdown format.

    **Supported input formats:** PDF, DOCX, DOC, PNG, JPG, JPEG, TIFF, BMP, etc.

    **Supported models:**
    - `Unlimited-OCR` (default) — High-accuracy OCR, best for text-heavy documents
    - `PaddleOCR-VL-1.5` — Vision-language model with layout preservation, best for tables and structured formats

    **Model selection guidance:**
    - Prefer `Unlimited-OCR` when the document contains a lot of text (articles, books, reports)
    - Prefer `PaddleOCR-VL-1.5` when table structure or technical layout formatting is important

    **Returns:** Extracted text in Markdown format, with layout and structure preserved.

    Display the result as: 📖[Extracted Document Content]
    """
  end

  schema do
    field(:file, {:required, :string},
      description:
        "Path to the document file (e.g. /path/to/doc.pdf) or a remote URL (http/https)"
    )

    field(:include_image, :boolean,
      description: "Whether to include image references in the output (default: true)"
    )

    field(:output_format, :string,
      description: "Output format: 'md' for Markdown (default), 'text' for plain text"
    )

    field(:model, :string,
      description:
        "OCR model: 'Unlimited-OCR' (default, best for text-heavy docs) or 'PaddleOCR-VL-1.5' (best for tables/structured layout)"
    )
  end

  @impl true
  def execute(params, frame) do
    file = Map.get(params, :file)
    include_image = Map.get(params, :include_image, true)
    output_format = Map.get(params, :output_format, "md")
    model = Map.get(params, :model)

    cond do
      is_nil(file) or file == "" ->
        resp =
          Response.tool() |> Response.error("`file` is required — provide a local path or URL")

        {:reply, resp, frame}

      true ->
        opts = [include_image: include_image, output_format: output_format]
        opts = if model, do: Keyword.put(opts, :model, model), else: opts

        case Client.extract(file, opts) do
          {:ok, text} ->
            resp = Response.tool() |> Response.text(text)
            {:reply, resp, frame}

          {:error, reason} ->
            resp = Response.tool() |> Response.error("Extraction failed: #{reason}")
            {:reply, resp, frame}
        end
    end
  end
end
