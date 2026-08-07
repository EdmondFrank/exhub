defmodule Exhub.MCP.Tools.Brain.SearchVaultSemanticBlockersTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Brain.RAG.VectorIndex
  alias Exhub.MCP.Tools.Brain.SearchVault

  # Constant-vector stub embedder: every text maps to the same vector, so every
  # note semantically matches any query. This lets us isolate semantic-only
  # notes (no keyword match) and the scope filter from the embedder's behavior.
  defmodule ConstEmbedder do
    @dim 4
    @vec [1.0, 0.5, 0.25, 0.125]

    def dimension, do: @dim

    def encode(text) when is_binary(text) do
      case encode_batch([text]) do
        {:ok, [e | _]} -> {:ok, e}
        _ -> {:error, "stub"}
      end
    end

    def encode_batch(texts) when is_list(texts) do
      {:ok, Enum.map(texts, fn _ -> @vec end)}
    end
  end

  setup do
    vault =
      System.tmp_dir!()
      |> Path.join("brain_sem_blockers_#{System.unique_integer([:positive])}")

    File.mkdir_p!(vault)
    Application.put_env(:exhub, :obsidian_vault_path, vault)

    index_path =
      Path.join(
        System.tmp_dir!(),
        "brain_sem_blockers_#{System.unique_integer([:positive])}.db"
      )

    # Use a uniquely-named server so the test doesn't collide with the
    # app-started VectorIndex when `mix test` boots the application.
    server = :"vector_index_blockers_#{System.unique_integer([:positive])}"

    Application.put_env(:exhub, :brain_rag, %{
      "index_path" => index_path,
      "embedder_module" => ConstEmbedder,
      "dim" => ConstEmbedder.dimension(),
      "vector_index_server" => server
    })

    {:ok, pid} = VectorIndex.start_link(name: server)

    on_exit(fn ->
      if Process.alive?(pid), do: Process.exit(pid, :kill)
      Application.delete_env(:exhub, :obsidian_vault_path)
      Application.delete_env(:exhub, :brain_rag)
      File.rm_rf!(vault)
      File.rm_rf!(index_path)
    end)

    {:ok, vault: vault}
  end

  defp search(params) do
    {:reply, resp, _frame} = SearchVault.execute(params, %{})
    resp
  end

  defp text_of(resp) do
    Enum.find(resp.content, &(&1["type"] == "text"))["text"]
  end

  test "semantic-only notes get no fabricated bm25 score", %{vault: vault} do
    # The query term does not appear in this note's content, so it is a
    # semantic-only candidate (matches via the constant vector, not keywords).
    File.write!(Path.join(vault, "note.md"), "totally unrelated plain text content")

    resp = search(%{query: "zzznotfound", semantic: true})
    output = text_of(resp)

    assert output =~ "note.md"
    assert output =~ "bm25=0.000"
  end

  test "scoped semantic search does not leak out-of-scope results", %{vault: vault} do
    File.write!(Path.join(vault, "out.md"), "out of scope content that is long enough to embed")
    sub = Path.join(vault, "sub")
    File.mkdir_p!(sub)
    File.write!(Path.join(sub, "in.md"), "in scope content that is long enough to embed")

    # Full-vault search first so both files are in the index.
    _ = search(%{query: "anything", semantic: true})

    resp = search(%{query: "anything", semantic: true, path: "sub"})
    output = text_of(resp)

    assert output =~ "sub/in.md"
    refute output =~ "out.md"
  end
end