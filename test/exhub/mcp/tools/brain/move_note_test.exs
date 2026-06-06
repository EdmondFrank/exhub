defmodule Exhub.MCP.Tools.Brain.MoveNoteTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Brain.MoveNote

  @test_vault Path.join(System.tmp_dir!(), "brain_move_note_test_#{System.unique_integer([:positive])}")

  setup do
    File.mkdir_p!(@test_vault)
    Application.put_env(:exhub, :obsidian_vault_path, @test_vault)

    on_exit(fn ->
      Application.delete_env(:exhub, :obsidian_vault_path)
      File.rm_rf!(@test_vault)
    end)

    :ok
  end

  describe "name/0" do
    test "returns brain_move_note" do
      assert MoveNote.name() == "brain_move_note"
    end
  end

  describe "execute/2" do
    test "renames a note in the same directory" do
      File.write!(Path.join(@test_vault, "old-name.md"), "# Content")

      params = %{source: "old-name", destination: "new-name", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = MoveNote.execute(params, frame)

      refute File.exists?(Path.join(@test_vault, "old-name.md"))
      assert File.exists?(Path.join(@test_vault, "new-name.md"))
      assert File.read!(Path.join(@test_vault, "new-name.md")) == "# Content"
    end

    test "moves a note to a different folder" do
      File.mkdir_p!(Path.join(@test_vault, "drafts"))
      File.write!(Path.join(Path.join(@test_vault, "drafts"), "idea.md"), "# Idea")

      params = %{source: "drafts/idea", destination: "projects/idea", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = MoveNote.execute(params, frame)

      refute File.exists?(Path.join(Path.join(@test_vault, "drafts"), "idea.md"))
      assert File.exists?(Path.join(Path.join(@test_vault, "projects"), "idea.md"))
      assert File.read!(Path.join(Path.join(@test_vault, "projects"), "idea.md")) == "# Idea"
    end

    test "appends .md extension automatically" do
      File.write!(Path.join(@test_vault, "note.md"), "content")

      params = %{source: "note", destination: "renamed", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = MoveNote.execute(params, frame)

      refute File.exists?(Path.join(@test_vault, "note.md"))
      assert File.exists?(Path.join(@test_vault, "renamed.md"))
    end

    test "does not double .md extension" do
      File.write!(Path.join(@test_vault, "a.md"), "content")

      params = %{source: "a.md", destination: "b.md", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = MoveNote.execute(params, frame)

      assert File.exists?(Path.join(@test_vault, "b.md"))
      refute File.exists?(Path.join(@test_vault, "b.md.md"))
    end

    test "creates parent directories for destination" do
      File.write!(Path.join(@test_vault, "loose.md"), "content")

      params = %{source: "loose", destination: "deep/nested/path/loose", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = MoveNote.execute(params, frame)

      nested = Path.join(Path.join(Path.join(Path.join(@test_vault, "deep"), "nested"), "path"), "loose.md")
      assert File.exists?(nested)
    end

    test "returns error when source does not exist" do
      params = %{source: "nonexistent", destination: "somewhere", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = MoveNote.execute(params, frame)

      refute File.exists?(Path.join(@test_vault, "somewhere.md"))
    end

    test "returns error when destination exists and overwrite is false" do
      File.write!(Path.join(@test_vault, "src.md"), "source content")
      File.write!(Path.join(@test_vault, "dst.md"), "dest content")

      params = %{source: "src", destination: "dst", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = MoveNote.execute(params, frame)

      # Both files should remain unchanged
      assert File.read!(Path.join(@test_vault, "src.md")) == "source content"
      assert File.read!(Path.join(@test_vault, "dst.md")) == "dest content"
    end

    test "overwrites destination when overwrite is true" do
      File.write!(Path.join(@test_vault, "src.md"), "source content")
      File.write!(Path.join(@test_vault, "dst.md"), "dest content")

      params = %{source: "src", destination: "dst", overwrite: true}
      frame = %{}

      {:reply, _resp, ^frame} = MoveNote.execute(params, frame)

      refute File.exists?(Path.join(@test_vault, "src.md"))
      assert File.read!(Path.join(@test_vault, "dst.md")) == "source content"
    end

    test "returns error for source path outside vault" do
      params = %{source: "../../../escape", destination: "safe", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = MoveNote.execute(params, frame)

      refute File.exists?(Path.join(@test_vault, "safe.md"))
    end

    test "returns error for destination path outside vault" do
      File.write!(Path.join(@test_vault, "note.md"), "content")

      params = %{source: "note", destination: "../../../escape", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = MoveNote.execute(params, frame)

      # Source should still exist (move was blocked)
      assert File.exists?(Path.join(@test_vault, "note.md"))
    end
  end
end
