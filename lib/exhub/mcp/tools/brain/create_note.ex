defmodule Exhub.MCP.Tools.Brain.CreateNote do
  @moduledoc """
  MCP Tool: brain_create_note

  Create a new note in the Obsidian vault.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Brain.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "brain_create_note"

  @impl true
  def description do
    """
    Create a new note in the Obsidian brain vault.

    By default, returns an error if the note already exists.
    Set `overwrite: true` to replace an existing note.

    Examples:
    - Simple note:        { "filename": "ideas" }
    - In folder:          { "filename": "weekly-review", "folder": "journal/2026" }
    - With content:       { "filename": "todo", "folder": "projects", "content": "- [ ] Task 1" }
    - Overwrite existing: { "filename": "scratch", "overwrite": true, "content": "New content" }
    """
  end

  schema do
    field(:filename, {:required, :string},
      description: "Note filename (`.md` extension is added automatically if missing)"
    )

    field(:folder, :string, description: "Optional subfolder path relative to vault root")

    field(:content, :string,
      description: "Note content (default: empty)",
      default: ""
    )

    field(:overwrite, :boolean,
      description: "Allow overwriting an existing note (default: false)",
      default: false
    )
  end

  @impl true
  def execute(params, frame) do
    filename = Map.get(params, :filename)
    folder = Map.get(params, :folder)
    content = Map.get(params, :content, "")
    overwrite = Map.get(params, :overwrite, false)

    vault = Helpers.vault_path()
    full_path = Helpers.build_note_path(vault, filename, folder)

    with :ok <- Helpers.validate_in_vault(vault, full_path),
         :ok <- check_overwrite(full_path, overwrite),
         :ok <- ensure_parent_dir(full_path),
         :ok <- File.write(full_path, content) do
      rel_path = Path.relative_to(full_path, vault)

      resp =
        Response.tool()
        |> Response.text("Vault: #{vault}\n\nCreated: #{rel_path}")

      {:reply, resp, frame}
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp check_overwrite(_path, true), do: :ok

  defp check_overwrite(path, false) do
    if File.exists?(path) do
      {:error, "Note already exists: #{Path.basename(path)}. Use overwrite: true to replace it."}
    else
      :ok
    end
  end

  defp ensure_parent_dir(path) do
    dir = Path.dirname(path)

    case File.mkdir_p(dir) do
      :ok -> :ok
      {:error, reason} -> {:error, "Cannot create directory #{dir}: #{inspect(reason)}"}
    end
  end
end
