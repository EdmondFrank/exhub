defmodule Exhub.MCP.Tools.Brain.SearchVaultPolicyTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Brain.RAG.VectorIndex
  alias Exhub.MCP.Tools.Brain.SearchVault

  # Constant-vector stub embedder: every text maps to the same vector, so every
  # note semantically matches any query (isolates policy behavior from embedding).
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
      |> Path.join("brain_policy_#{System.unique_integer([:positive])}")

    File.mkdir_p!(vault)
    Application.put_env(:exhub, :obsidian_vault_path, vault)

    index_path =
      Path.join(System.tmp_dir!(), "brain_policy_#{System.unique_integer([:positive])}.db")

    server = :"vector_index_policy_#{System.unique_integer([:positive])}"

    Application.put_env(:exhub, :brain_rag, %{
      "index_path" => index_path,
      "embedder_module" => ConstEmbedder,
      "dim" => ConstEmbedder.dimension(),
      "vector_index_server" => server
    })

    # Isolated policy config; tests override as needed.
    Application.put_env(:exhub, :brain_search, %{})

    {:ok, pid} = VectorIndex.start_link(name: server)

    on_exit(fn ->
      if Process.alive?(pid), do: Process.exit(pid, :kill)
      Application.delete_env(:exhub, :obsidian_vault_path)
      Application.delete_env(:exhub, :brain_rag)
      Application.delete_env(:exhub, :brain_search)
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

  test "policy filename restricts retrieval to the filename channel", %{vault: vault} do
    # Body match only (agenda.md) vs filename match only (meeting.md).
    File.write!(Path.join(vault, "agenda.md"), "meeting notes are in the agenda")
    File.write!(Path.join(vault, "meeting.md"), "nothing relevant inside")

    output = search(%{query: "meeting", policy: "filename"}) |> text_of()

    assert output =~ "meeting.md"
    refute output =~ "agenda.md"
  end

  test "policy keyword returns matches without a semantic signal", %{vault: vault} do
    File.write!(Path.join(vault, "auth.md"), "authentication and login flow details")

    output = search(%{query: "authentication", policy: "keyword"}) |> text_of()

    assert output =~ "auth.md"
    refute output =~ "semantic="
  end

  test "inline policy map with semantic on runs the vector path", %{vault: vault} do
    File.write!(
      Path.join(vault, "auth.md"),
      "Authentication and login flow details for the application."
    )

    output =
      search(%{query: "zzznotfound", policy: %{"semantic" => "on"}})
      |> text_of()

    assert output =~ "semantic="
    assert output =~ "auth.md"
  end

  test "explicit semantic: true overrides a policy that disables it", %{vault: vault} do
    File.write!(
      Path.join(vault, "auth.md"),
      "Authentication and login flow details for the application."
    )

    output =
      search(%{query: "zzznotfound", policy: "keyword", semantic: true})
      |> text_of()

    assert output =~ "semantic="
    assert output =~ "auth.md"
  end

  test "policy auto enables semantic for conversational queries when autodetect is on", %{
    vault: vault
  } do
    Application.put_env(:exhub, :brain_search, %{
      "default_policy" => "auto",
      "semantic_autodetect" => true
    })

    File.write!(
      Path.join(vault, "auth.md"),
      "Authentication and login flow details for the application."
    )

    output =
      search(%{query: "how do we handle authentication and login flows", policy: "auto"})
      |> text_of()

    assert output =~ "semantic="
    assert output =~ "auth.md"
  end

  test "policy auto respects semantic_autodetect false", %{vault: vault} do
    Application.put_env(:exhub, :brain_search, %{
      "default_policy" => "auto",
      "semantic_autodetect" => false
    })

    File.write!(
      Path.join(vault, "auth.md"),
      "How authentication and login flow work inside the application layer."
    )

    output =
      search(%{query: "how do we handle authentication and login flows", policy: "auto"})
      |> text_of()

    refute output =~ "semantic="
    assert output =~ "auth.md"
  end

  test "unknown policy name falls back to default without raising", %{vault: vault} do
    File.write!(Path.join(vault, "note.md"), "some note content here")

    output = search(%{query: "note", policy: "does-not-exist"}) |> text_of()

    assert output =~ "note.md"
  end

  test "inline policy top_n caps the number of returned files", %{vault: vault} do
    File.write!(Path.join(vault, "a.md"), "project notes and ideas")
    File.write!(Path.join(vault, "b.md"), "project planning notes")
    File.write!(Path.join(vault, "c.md"), "project review notes")

    output =
      search(%{query: "project", policy: %{"top_n" => 1}})
      |> text_of()

    files = Regex.scan(~r/\.md/, output) |> length()
    assert files == 1
  end
end