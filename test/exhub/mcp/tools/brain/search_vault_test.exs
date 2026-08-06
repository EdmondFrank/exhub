defmodule Exhub.MCP.Tools.Brain.SearchVaultTest do
  # Sync to avoid racing on the shared :obsidian_vault_path config with other
  # async Brain tests (create_note, move_note).
  use ExUnit.Case, async: false

  alias Exhub.MCP.Tools.Brain.SearchVault

  setup do
    vault =
      System.tmp_dir!()
      |> Path.join("brain_search_#{System.unique_integer([:positive])}")

    File.mkdir_p!(vault)
    Application.put_env(:exhub, :obsidian_vault_path, vault)

    on_exit(fn ->
      Application.delete_env(:exhub, :obsidian_vault_path)
      File.rm_rf!(vault)
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

  test "content search returns ranked results with signal breakdown", %{vault: vault} do
    File.write!(Path.join(vault, "meeting.md"), "# Weekly Meeting\nLots of agenda items here.")
    File.write!(Path.join(vault, "agenda.md"), "agenda for sprint planning")
    File.write!(Path.join(vault, "unrelated.md"), "nothing relevant inside")

    resp = search(%{query: "agenda"})
    output = text_of(resp)

    assert output =~ "Found"
    assert output =~ "meeting.md" or output =~ "agenda.md"
    assert output =~ "score:"
    assert output =~ "bm25="
  end

  test "respects gitignore patterns", %{vault: vault} do
    File.write!(Path.join(vault, ".gitignore"), "secret/\n*.tmp\n")
    File.write!(Path.join(vault, "note.md"), "# Important Note")
    File.mkdir_p!(Path.join(vault, "secret"))
    File.write!(Path.join(vault, "secret/hidden.md"), "secret content")

    resp = search(%{query: "secret content"})
    output = text_of(resp)
    refute output =~ "hidden.md"
  end

  test "returns no results for non-matching query", %{vault: vault} do
    File.write!(Path.join(vault, "note.md"), "# Note")
    resp = search(%{query: "zzzz_nonexistent"})
    assert text_of(resp) =~ "No results found."
  end

  test "tag search finds notes by tag", %{vault: vault} do
    File.write!(Path.join(vault, "tagged.md"), "---\ntags: [project/active]\n---\nBody")
    File.write!(Path.join(vault, "plain.md"), "no tags here")

    resp = search(%{query: "tag:project/active"})
    output = text_of(resp)
    assert output =~ "tagged.md"
    refute output =~ "plain.md"
  end

  test "filename search works", %{vault: vault} do
    File.write!(Path.join(vault, "journal-2026.md"), "# Journal")
    File.write!(Path.join(vault, "notes.md"), "# Notes")

    resp = search(%{query: "journal", search_type: "filename"})
    output = text_of(resp)
    assert output =~ "journal-2026.md"
    refute output =~ "notes.md"
  end

  test "abs_path returns absolute paths", %{vault: vault} do
    File.write!(Path.join(vault, "note.md"), "# Note contents here")

    resp = search(%{query: "contents", abs_path: true})
    output = text_of(resp)
    assert output =~ Path.join(vault, "note.md")
  end

  test "fusion and weights params are accepted", %{vault: vault} do
    File.write!(Path.join(vault, "a.md"), "alpha beta gamma delta epsilon")
    File.write!(Path.join(vault, "b.md"), "beta gamma")

    resp = search(%{query: "beta", fusion: "rrf", weights: %{"bm25" => 0.5}})
    assert text_of(resp) =~ "Found"
  end

  test "title-match note ranks above body-only note", %{vault: vault} do
    # "meeting.md" matches via filename + heading; "notes.md" only in body
    File.write!(Path.join(vault, "meeting.md"), "# Weekly Meeting\nbody text")
    File.write!(Path.join(vault, "notes.md"), "some notes about meeting plans")

    resp = search(%{query: "meeting"})
    output = text_of(resp)

    assert rank_of(output, "meeting.md") < rank_of(output, "notes.md")
  end

  test "both-search retains content fields for merged notes", %{vault: vault} do
    # File matches both filename and content, and carries a matching tag.
    File.write!(Path.join(vault, "meeting.md"), "# Meeting\nbody with #meeting")

    resp = search(%{query: "meeting", search_type: "both"})
    output = text_of(resp)

    # tag_match = 1.0 requires the merged note to keep the content-search
    # fields (tags). A filename-only stub (tags: []) would yield tag_match=0.
    assert output =~ "meeting.md"
    assert output =~ "tag_match=1.000"
  end

  defp rank_of(output, file) do
    output
    |> String.split("\n")
    |> Enum.find_index(&(&1 =~ file))
  end
end