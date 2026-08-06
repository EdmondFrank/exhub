defmodule Exhub.MCP.Brain.HelpersTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Brain.Helpers

  setup do
    vault =
      System.tmp_dir!()
      |> Path.join("brain_helpers_#{System.unique_integer([:positive])}")

    File.mkdir_p!(vault)

    on_exit(fn -> File.rm_rf!(vault) end)
    {:ok, vault: vault}
  end

  test "count_backlinks resolves folder-path targets and .md suffixes", %{vault: vault} do
    File.mkdir_p!(Path.join(vault, "projects"))
    File.write!(Path.join(vault, "index.md"), "See [[projects/meeting]] and [[other note.md]]")
    File.write!(Path.join(vault, "projects/meeting.md"), "content")
    File.write!(Path.join(vault, "other note.md"), "content")

    files = ["index.md", "projects/meeting.md", "other note.md"]
    backlinks = Helpers.count_backlinks(vault, files)

    assert backlinks["projects/meeting.md"] == 1
    assert backlinks["other note.md"] == 1
  end

  test "count_backlinks resolves folder-path targets via path index, not basename", %{vault: vault} do
    # Two notes share the basename "meeting"; a folder-path link must target
    # only the exact path, not both basename matches.
    File.mkdir_p!(Path.join(vault, "a"))
    File.mkdir_p!(Path.join(vault, "b"))
    File.write!(Path.join(vault, "a/meeting.md"), "# a")
    File.write!(Path.join(vault, "b/meeting.md"), "# b")
    File.write!(Path.join(vault, "index.md"), "See [[a/meeting]]")

    files = ["a/meeting.md", "b/meeting.md", "index.md"]
    backlinks = Helpers.count_backlinks(vault, files)

    assert backlinks["a/meeting.md"] == 1
    refute backlinks["b/meeting.md"]
  end

  test "count_backlinks counts all notes sharing a duplicate basename", %{vault: vault} do
    File.mkdir_p!(Path.join(vault, "a"))
    File.mkdir_p!(Path.join(vault, "b"))
    File.write!(Path.join(vault, "a/note.md"), "# a")
    File.write!(Path.join(vault, "b/note.md"), "# b")
    File.write!(Path.join(vault, "index.md"), "See [[note]], also [[note]], and [[note]] again")

    files = ["a/note.md", "b/note.md", "index.md"]
    backlinks = Helpers.count_backlinks(vault, files)

    assert backlinks["a/note.md"] == 3
    assert backlinks["b/note.md"] == 3
  end

  test "count_backlinks ignores targets that resolve to no note", %{vault: vault} do
    File.write!(Path.join(vault, "index.md"), "See [[missing]]")
    files = ["index.md"]
    backlinks = Helpers.count_backlinks(vault, files)
    assert backlinks == %{}
  end
end