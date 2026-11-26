defmodule Exhub.MCP.Tools.Brain.ReadNoteTest do
  # Mutates the global :obsidian_vault_path config, so run serialized with the
  # other Brain tool tests.
  use ExUnit.Case, async: false

  alias Exhub.MCP.Tools.Brain.ReadNote

  setup do
    vault =
      System.tmp_dir!()
      |> Path.join("brain_read_note_#{System.unique_integer([:positive])}")

    File.mkdir_p!(vault)
    Application.put_env(:exhub, :obsidian_vault_path, vault)

    on_exit(fn ->
      Application.delete_env(:exhub, :obsidian_vault_path)
      File.rm_rf!(vault)
    end)

    {:ok, vault: vault}
  end

  defp read(params) do
    {:reply, resp, _frame} = ReadNote.execute(params, %{})
    resp
  end

  defp text_of(resp) do
    Enum.find(resp.content, &(&1["type"] == "text"))["text"]
  end

  defp json_of(resp), do: resp |> text_of() |> Jason.decode!()

  test "reads a note with line numbers and metadata", %{vault: vault} do
    File.write!(Path.join(vault, "note.md"), "---\ntags: [project/active]\n---\n# Title\nbody\n")

    output = text_of(read(%{path: "note"}))

    assert output =~ "Note: note.md"
    assert output =~ "Tags: project/active"
    assert output =~ "L4: # Title"
    assert output =~ "L5: body"
  end

  test "pages through a note with offset and limit", %{vault: vault} do
    File.write!(Path.join(vault, "long.md"), "one\ntwo\nthree\nfour\n")

    output = text_of(read(%{path: "long.md", offset: 2, limit: 2}))

    assert output =~ "L2: two"
    assert output =~ "L3: three"
    refute output =~ "L1: one"
    refute output =~ "L4: four"
    assert output =~ "truncated; continue with offset=4"
  end

  test "reads one Markdown section up to the next peer heading", %{vault: vault} do
    File.write!(Path.join(vault, "doc.md"), """
    # Title
    intro

    ## Endpoints
    GET /a

    ### Nested
    detail

    ## Other
    skip
    """)

    payload = json_of(read(%{path: "doc.md", heading: "Endpoints", format: "json"}))

    assert payload["body"] =~ "GET /a"
    assert payload["body"] =~ "### Nested"
    refute payload["body"] =~ "skip"
    assert payload["section"]["start_line"] == 4
    assert payload["first_line"] == 4
  end

  test "reports an unknown heading as an error", %{vault: vault} do
    File.write!(Path.join(vault, "doc.md"), "# Title\nbody\n")

    resp = read(%{path: "doc.md", heading: "Missing"})

    assert text_of(resp) =~ "Heading not found"
    assert resp.isError
  end

  test "missing notes and paths outside the vault are errors", %{vault: _vault} do
    missing = read(%{path: "ghost"})
    assert text_of(missing) =~ "Note not found"
    assert missing.isError

    outside = read(%{path: "../../etc/passwd"})
    assert text_of(outside) =~ "outside the vault"
    assert outside.isError
  end

  test "max_chars truncates the body on a line boundary", %{vault: vault} do
    File.write!(Path.join(vault, "long.md"), "aaaa\nbbbb\ncccc\n")

    payload = json_of(read(%{path: "long.md", max_chars: 6, format: "json"}))

    assert payload["body"] == "aaaa"
    assert payload["truncated"] == true
  end

  test "optionally counts backlinks", %{vault: vault} do
    File.write!(Path.join(vault, "target.md"), "# Target\n")
    File.write!(Path.join(vault, "source.md"), "see [[target]]\n")

    payload = json_of(read(%{path: "target.md", include_backlinks: true, format: "json"}))

    assert payload["backlinks"] == 1
  end
end
