defmodule Exhub.MCP.Brain.RAG.VectorIndexPartialTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Brain.RAG.VectorIndex

  # Embedder that fails whenever any text in the batch contains "FAILME".
  # Used to simulate a partial embedding failure mid-file.
  defmodule FailingEmbedder do
    @dim 4

    def dimension, do: @dim

    def encode(text) when is_binary(text) do
      case encode_batch([text]) do
        {:ok, [e | _]} -> {:ok, e}
        _ -> {:error, "stub"}
      end
    end

    def encode_batch(texts) when is_list(texts) do
      if Enum.any?(texts, &String.contains?(&1, "FAILME")) do
        {:error, "injected failure"}
      else
        {:ok, Enum.map(texts, fn _ -> [1.0, 0.5, 0.25, 0.125] end)}
      end
    end
  end

  setup do
    tmp = Path.join(System.tmp_dir!(), "brain_partial_#{System.unique_integer([:positive])}.db")

    Application.put_env(:exhub, :brain_rag, %{
      "index_path" => tmp,
      "embedder_module" => FailingEmbedder,
      "dim" => FailingEmbedder.dimension(),
      "batch_size" => 1
    })

    {:ok, pid} =
      VectorIndex.start_link(name: :"vector_index_partial_#{System.unique_integer([:positive])}")

    on_exit(fn ->
      if Process.alive?(pid), do: Process.exit(pid, :kill)
      Application.delete_env(:exhub, :brain_rag)
      File.rm_rf!(tmp)
    end)

    {:ok, server: pid}
  end

  test "retries a file whose embedding failed partway (no signature recorded)", %{server: server} do
    content =
      "# One\n" <>
        String.duplicate("alpha word ", 60) <>
        "\n\n# Two\n" <> String.duplicate("beta word ", 60) <> " FAILME"

    file = write_note("partial", content)

    # First chunk embeds; second chunk fails (injected). Because the signature
    # is only recorded on full success, no marker is left behind.
    assert {:ok, %{changed: 1, indexed: 1, failed: 1}} = VectorIndex.rebuild([file], server)

    # The file is still considered changed, so the next rebuild retries it.
    assert {:ok, %{changed: 1, failed: 1}} = VectorIndex.rebuild([file], server)

    # Once the failing content is removed, the file indexes fully.
    File.write!(file, String.replace(content, " FAILME", ""))
    assert {:ok, %{changed: 1, indexed: 1, failed: 0}} = VectorIndex.rebuild([file], server)
  end

  defp write_note(name, content) do
    dir =
      Path.join(System.tmp_dir!(), "brain_partial_vault_#{System.unique_integer([:positive])}")

    File.mkdir_p!(dir)
    path = Path.join(dir, "#{name}.md")
    File.write!(path, content)
    path
  end
end