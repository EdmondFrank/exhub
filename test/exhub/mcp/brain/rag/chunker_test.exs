defmodule Exhub.MCP.Brain.RAG.ChunkerTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.RAG.Chunker

  test "splits content into section-based chunks" do
    content = """
    # Intro
    Some intro text that is long enough to exceed the minimum chunk size.

    ## Details
    More details here.

    And a second paragraph.
    """

    chunks = Chunker.chunk(content)

    assert length(chunks) >= 2
    assert Enum.any?(chunks, &String.contains?(&1.text, "Intro"))
    assert Enum.any?(chunks, &String.contains?(&1.text, "Details"))
    assert Enum.all?(chunks, &(&1.index >= 1))
  end

  test "strips YAML frontmatter" do
    content = """
    ---
    tags: [project/active]
    aliases: [test]
    ---
    # Real Note
    Body content here that is long enough to exceed the minimum chunk size.
    """

    chunks = Chunker.chunk(content)

    refute Enum.any?(chunks, &String.contains?(&1.text, "aliases"))
    assert Enum.any?(chunks, &String.contains?(&1.text, "Real Note"))
    assert Enum.any?(chunks, &String.contains?(&1.text, "Body content"))
  end

  test "returns empty list for empty content" do
    assert Chunker.chunk("") == []
    assert Chunker.chunk(nil) == []
  end

  test "drops sub-minimal chunks" do
    chunks = Chunker.chunk("# H\n")
    assert chunks == []
  end

  test "heading lines are included in their chunk" do
    content = "# Meeting\nNotes about the meeting that are long enough to pass the minimum."
    chunks = Chunker.chunk(content)
    assert Enum.any?(chunks, &String.starts_with?(&1.text, "# Meeting"))
  end

  test "splits oversized single paragraphs on sentence boundaries" do
    paragraph = String.duplicate("This is a fairly long sentence used to exceed the maximum chunk size. ", 80)
    chunks = Chunker.chunk(paragraph)
    assert length(chunks) > 1
    # Content preserved across chunks.
    assert Enum.map_join(chunks, " ", &String.trim(&1.text)) == String.trim(paragraph)
  end

  test "splits lowercase prose on sentence boundaries" do
    paragraph =
      String.duplicate(
        "this is a lowercase sentence without capital letters after the period. ",
        200
      )

    chunks = Chunker.chunk(paragraph)
    assert length(chunks) > 1
    assert Enum.all?(chunks, &(String.length(&1.text) <= 2000))
  end

  test "splits CJK text on Chinese sentence terminators" do
    sentence = "这是用于测试中文分句功能的完整句子内容它足够长并且超过最小分块的阈值限制。"
    long = String.duplicate(sentence, 200)
    chunks = Chunker.chunk(long)
    assert length(chunks) > 1
    assert Enum.map_join(chunks, "", &String.trim(&1.text)) == String.trim(long)
  end

  test "recursively splits long paragraphs so each chunk respects max_chars" do
    # Two paragraphs, each well over max_chars, each containing sentences.
    p1 = String.duplicate("Alpha sentence with enough words that the resulting chunk exceeds the minimum length. ", 200)
    p2 = String.duplicate("Gamma sentence with enough words that the resulting chunk exceeds the minimum length. ", 200)
    content = p1 <> "\n\n" <> p2

    chunks = Chunker.chunk(content)
    assert length(chunks) > 2
    assert Enum.all?(chunks, &(String.length(&1.text) <= 2000))
  end
end