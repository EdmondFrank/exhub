defmodule Exhub.MCP.DocExtractServer do
  @moduledoc """
  MCP Server for document text extraction via Gitee AI.

  Exposes the `doc_extract` tool which extracts and recognizes text from
  documents (PDF, DOCX, images, etc.) using the Gitee AI Async Document
  Parse API powered by PaddleOCR-VL-1.5.

  The server uses HTTP transport and can be accessed at the /doc-extract/mcp endpoint.
  """

  use Anubis.Server,
    name: "exhub-doc-extract-server",
    version: "1.0.0",
    capabilities: [:tools]

  component(Exhub.MCP.Tools.DocExtract)

  @impl true
  def init(client_info, frame) do
    _ = client_info
    {:ok, frame}
  end
end
