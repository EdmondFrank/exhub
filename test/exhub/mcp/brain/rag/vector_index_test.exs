defmodule Exhub.MCP.Brain.RAG.VectorIndexTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Brain.RAG.VectorIndex

  # Stub embedder that maps text to a deterministic 4-dim vector by hashing
  # the first token, so identical queries return identical embeddings.
  defmodule StubEmbedder do
    @dim 4

    def dimension, do: @dim

    def encode(text) when is_binary(text) do
      case encode_batch([text]) do
        {:ok, [e | _]} -> {:ok, e}
        _ -> {:error, "stub encode failed"}
      end
    end

    def encode_batch(texts) when is_list(texts) do
      vectors = Enum.map(texts, fn t -> vector(t) end)
      {:ok, vectors}
    end

    defp vector(text) do
      words = String.split(String.downcase(text), ~r/\W+/, trim: true)
      seed = if words == [], do: 0, else: :erlang.phash2(hd(words))
      for i <- 0..(@dim - 1), do: (:math.sin(seed + i) + 1) / 2
    end
  end

  setup do
    tmp = Path.join(System.tmp_dir!(), "brain_rag_#{System.unique_integer([:positive])}.db")

    Application.put_env(:exhub, :brain_rag,
      %{
        "index_path" => tmp,
        "embedder_module" => StubEmbedder,
        "dim" => StubEmbedder.dimension()
      }
    )

    {:ok, pid} = VectorIndex.start_link(name: :"vector_index_test_#{System.unique_integer([:positive])}")

    on_exit(fn ->
      if Process.alive?(pid), do: Process.exit(pid, :kill)
      Application.delete_env(:exhub, :brain_rag)
      File.rm_rf!(tmp)
    end)

    {:ok, server: pid}
  end

  test "rebuild indexes chunks and chunk_count reflects them", %{server: server} do
    file = write_note("alpha app note", "Alpha beta gamma delta apple banana cherry dog.")
    assert {:ok, %{indexed: n}} = VectorIndex.rebuild([file], server)
    assert n == 1
    assert VectorIndex.chunk_count(server) == 1
  end

  test "search returns nearest chunk by similarity", %{server: server} do
    a = write_note("alpha note", "apple banana cherry dog elephant fish")
    b = write_note("beta note", "xylophone yellow zebra apple orange mango peach plum")
    {:ok, _} = VectorIndex.rebuild([a, b], server)

    assert {:ok, results} = VectorIndex.search("apple", top_k: 2, server: server)
    assert [first | _] = results
    assert first.file == a
    assert first.similarity > 0.5
  end

  test "rebuild only re-embeds changed files", %{server: server} do
    file = write_note("alpha", "alpha beta gamma delta epsilon zeta eta theta")
    {:ok, %{indexed: 1}} = VectorIndex.rebuild([file], server)

    # Unchanged file -> 0 changed, 0 indexed
    assert {:ok, %{changed: 0, indexed: 0}} = VectorIndex.rebuild([file], server)

    # Changed content -> re-indexed
    File.write!(file, "alpha beta gamma delta epsilon zeta eta theta iota kappa")
    assert {:ok, %{changed: 1, indexed: 1}} = VectorIndex.rebuild([file], server)
  end

  test "prunes chunks for files deleted from disk", %{server: server} do
    file = write_note("gone", "content that is long enough to be indexed")
    {:ok, %{indexed: 1}} = VectorIndex.rebuild([file], server)
    assert VectorIndex.chunk_count(server) == 1

    File.rm!(file)
    {:ok, %{indexed: 0}} = VectorIndex.rebuild([file], server)
    assert VectorIndex.chunk_count(server) == 0
  end

  test "prunes stale chunks when a file's chunk count shrinks", %{server: server} do
    two_chunks =
      "# Alpha\n" <>
        String.duplicate("alpha word ", 100) <>
        "\n\n# Beta\n" <> String.duplicate("beta word ", 100)

    one_chunk = String.duplicate("omega word ", 100)
    file = write_note("shrink", two_chunks)

    {:ok, %{indexed: 1}} = VectorIndex.rebuild([file], server)
    assert VectorIndex.chunk_count(server) == 2

    File.write!(file, one_chunk)
    {:ok, %{indexed: 1}} = VectorIndex.rebuild([file], server)
    # The old higher-index chunk must be gone, leaving only the single chunk.
    assert VectorIndex.chunk_count(server) == 1
  end

  defp write_note(name, content) do
    dir = Path.join(System.tmp_dir!(), "brain_rag_vault_#{System.unique_integer([:positive])}")
    File.mkdir_p!(dir)
    path = Path.join(dir, "#{name}.md")
    File.write!(path, content)
    path
  end
end