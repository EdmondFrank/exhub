defmodule Exhub.MCP.Brain.RAG.Chunker do
  @moduledoc """
  Splits Obsidian markdown note content into chunks suitable for embedding.

  Chunking strategy (adapted from Mosaic's paragraph/sentence splitter):

    * A chunk is created for each top-level section, delimited by a markdown
      heading (`#`/`##`/`###`). The heading line is included in the chunk so
      the embedding carries section context.
    * Content before the first heading is treated as its own chunk.
    * Overly long chunks are further split on blank lines (paragraphs) and,
      if still too long, on sentence boundaries.

  Returns `[%{text: String.t(), index: pos_integer()}]`.

  ## Options

    * `:max_chars` - approximate max characters per chunk (default 2000)
    * `:min_chars` - chunk texts shorter than this are dropped (default 32)
  """

  @heading_pattern ~r/^\#{1,6}\s+(.+)$/
  @blank_line ~r/\n\s*\n/

  @doc "Chunk note content, returning a list of chunk maps."
  @spec chunk(String.t()) :: [%{text: String.t(), index: pos_integer()}]
  def chunk(content) when is_binary(content) do
    content
    |> strip_frontmatter()
    |> String.trim()
    |> split_sections()
    |> Enum.flat_map(&maybe_split_long/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.reject(&(String.length(String.trim(&1)) < min_chars()))
    |> Enum.with_index(1)
    |> Enum.map(fn {text, index} -> %{text: text, index: index} end)
  end

  def chunk(_), do: []

  # ── section splitting ────────────────────────────────────────────────

  defp split_sections(content) do
  lines = String.split(content, "\n")

  {sections, current} =
    Enum.reduce(lines, {[], []}, fn line, {sections, current} ->
      if heading?(line) and current != [] do
        {[Enum.reverse(current) | sections], [line]}
      else
        {sections, [line | current]}
      end
    end)

  sections =
    if current == [] do
      sections
    else
      [Enum.reverse(current) | sections]
    end

  sections
  |> Enum.reverse()
  |> Enum.map(fn lines -> Enum.join(lines, "\n") |> String.trim() end)
  |> Enum.reject(&(&1 == ""))
end

  defp heading?(line) do
    Regex.match?(@heading_pattern, String.trim(line))
  end

  # ── long chunk splitting ─────────────────────────────────────────────

  defp maybe_split_long(chunk) do
    if String.length(chunk) <= max_chars() do
      [chunk]
    else
      split_oversized(chunk)
    end
  end

  defp split_oversized(chunk) do
    paragraphs = split_paragraphs(chunk)

    if length(paragraphs) > 1 do
      # Recursively split any paragraph that is still too long, so no
      # returned chunk exceeds max_chars when a paragraph split is possible.
      Enum.flat_map(paragraphs, &maybe_split_long/1)
    else
      split_sentences(chunk)
    end
  end

  defp split_paragraphs(chunk) do
    chunk
    |> String.split(@blank_line)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp split_sentences(chunk) do
    # ASCII sentence boundaries (split on any letter or `#` so lowercase prose
    # is split too, not just capitalized sentences) plus CJK terminators, which
    # need no space. The `u` flag keeps character classes Unicode-aware
    # (byte-mode regexes would match on multibyte sub-bytes and split inside
    # CJK characters).
    Regex.split(~r/(?<=[.!?])\s+(?=[A-Za-z#])|(?<=[。！？])(?=\S)/u, chunk)
    |> Enum.map(&String.trim/1)
    |> Enum.reject(&(&1 == ""))
  end

  defp strip_frontmatter(content) do
    case Regex.run(~r/\A---\n.*?\n---\n?(.*)/s, content) do
      [_, rest] -> rest
      _ -> content
    end
  end

  defp max_chars, do: Application.get_env(:exhub, :brain_rag, %{})["max_chars"] || 2000
  defp min_chars, do: Application.get_env(:exhub, :brain_rag, %{})["min_chars"] || 32
end