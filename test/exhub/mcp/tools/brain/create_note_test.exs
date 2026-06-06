defmodule Exhub.MCP.Tools.Brain.CreateNoteTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Brain.CreateNote

  @test_vault Path.join(System.tmp_dir!(), "brain_create_note_test_#{System.unique_integer([:positive])}")

  setup do
    File.mkdir_p!(@test_vault)

    # Point the vault path to our temp dir
    Application.put_env(:exhub, :obsidian_vault_path, @test_vault)

    on_exit(fn ->
      Application.delete_env(:exhub, :obsidian_vault_path)
      File.rm_rf!(@test_vault)
    end)

    :ok
  end

  describe "name/0" do
    test "returns brain_create_note" do
      assert CreateNote.name() == "brain_create_note"
    end
  end

  describe "execute/2" do
    test "creates a simple note" do
      params = %{filename: "test-note", content: "# Hello", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = CreateNote.execute(params, frame)

      path = Path.join(@test_vault, "test-note.md")
      assert File.exists?(path)
      assert File.read!(path) == "# Hello"
    end

    test "appends .md extension automatically" do
      params = %{filename: "no-ext", content: "", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = CreateNote.execute(params, frame)

      assert File.exists?(Path.join(@test_vault, "no-ext.md"))
    end

    test "does not double .md extension" do
      params = %{filename: "already.md", content: "", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = CreateNote.execute(params, frame)

      assert File.exists?(Path.join(@test_vault, "already.md"))
      refute File.exists?(Path.join(@test_vault, "already.md.md"))
    end

    test "creates note in subfolder" do
      params = %{filename: "weekly", folder: "journal/2026", content: "## Week 1", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = CreateNote.execute(params, frame)

      path = Path.join([@test_vault, "journal", "2026", "weekly.md"])
      assert File.exists?(path)
      assert File.read!(path) == "## Week 1"
    end

    test "defaults content to empty string" do
      params = %{filename: "empty", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = CreateNote.execute(params, frame)

      path = Path.join(@test_vault, "empty.md")
      assert File.exists?(path)
      assert File.read!(path) == ""
    end

    test "returns error when note already exists and overwrite is false" do
      # Create the note first
      File.write!(Path.join(@test_vault, "existing.md"), "original")

      params = %{filename: "existing", content: "new", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = CreateNote.execute(params, frame)
      assert File.read!(Path.join(@test_vault, "existing.md")) == "original"
    end

    test "overwrites when overwrite is true" do
      # Create the note first
      File.write!(Path.join(@test_vault, "overwrite-me.md"), "old content")

      params = %{filename: "overwrite-me", content: "new content", overwrite: true}
      frame = %{}

      {:reply, _resp, ^frame} = CreateNote.execute(params, frame)

      assert File.read!(Path.join(@test_vault, "overwrite-me.md")) == "new content"
    end

    test "returns error for path outside vault" do
      params = %{filename: "../../../escape", content: "", overwrite: false}
      frame = %{}

      {:reply, _resp, ^frame} = CreateNote.execute(params, frame)

      # File should not be created outside vault
      refute File.exists?(Path.join(System.tmp_dir!(), "escape.md"))
    end
  end
end
