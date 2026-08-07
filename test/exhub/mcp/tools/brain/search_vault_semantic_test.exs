defmodule Exhub.MCP.Tools.Brain.SearchVaultSemanticTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Brain.RAG.VectorIndex
  alias Exhub.MCP.Tools.Brain.SearchVault

  # Deterministic stub embedder so no network is needed.
  defmodule StubEmbedder do
    @dim 4

    def dimension, do: @dim

    def encode(text) when is_binary(text) do
      case encode_batch([text]) do
        {:ok, [e | _]} -> {:ok, e}
        _ -> {:error, "stub"}
      end
    end

    def encode_batch(texts) when is_list(texts) do
      {:ok, Enum.map(texts, &vector/1)}
    end

    defp vector(text) do
      words = String.split(String.downcase(text), ~r/\W+/, trim: true)
      seed = if words == [], do: 0, else: :erlang.phash2(hd(words))
      for i <- 0..(@dim - 1), do: (:math.sin(seed + i) + 1) / 2
    end
  end

  setup do
    vault =
      System.tmp_dir!()
      |> Path.join("brain_semantic_#{System.unique_integer([:positive])}")

    File.mkdir_p!(vault)
    Application.put_env(:exhub, :obsidian_vault_path, vault)

    index_path =
      Path.join(System.tmp_dir!(), "brain_semantic_idx_#{System.unique_integer([:positive])}.db")

    # Use a uniquely-named server so the test doesn't collide with the
    # app-started VectorIndex when `mix test` boots the application.
    server = :"vector_index_semantic_#{System.unique_integer([:positive])}"

    Application.put_env(:exhub, :brain_rag,
      %{
        "index_path" => index_path,
        "embedder_module" => StubEmbedder,
        "dim" => StubEmbedder.dimension(),
        "vector_index_server" => server
      }
    )

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

  test "semantic search merges vector-discovered notes and adds semantic scorer", %{vault: vault} do
    File.write!(Path.join(vault, "auth.md"), "Authentication and login flow for the app.")
    File.write!(Path.join(vault, "unrelated.md"), "Groceries and shopping list.")

    resp = search(%{query: "authentication", semantic: true})
    output = text_of(resp)

    # auth.md should be surfaced (semantic match) and the semantic signal shown.
    assert output =~ "auth.md"
    assert output =~ "semantic="
  end

  test "semantic search degrades to keyword when no vector matches", %{vault: vault} do
    File.write!(Path.join(vault, "note.md"), "Some plain note content here.")

    resp = search(%{query: "note", semantic: true})
    assert text_of(resp) =~ "note.md"
  end
end