defmodule Exhub.MCP.Tools.Brain.MoveNote do
  @moduledoc """
  MCP Tool: brain_move_note

  Move or rename a note within the Obsidian vault.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Brain.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "brain_move_note"

  @impl true
  def description do
    """
    Move or rename a note within the Obsidian brain vault.

    Paths are relative to the vault root. The `.md` extension is added
    automatically if missing. Parent directories for the destination
    are created as needed.

    By default, returns an error if the destination already exists.
    Set `overwrite: true` to replace it.

    Examples:
    - Rename:      { "source": "drafts/idea", "destination": "projects/idea" }
    - Move folder: { "source": "old/meeting", "destination": "archive/2026/meeting" }
    - Overwrite:   { "source": "tmp/notes", "destination": "inbox/notes", "overwrite": true }
    """
  end

  schema do
    field(:source, {:required, :string},
      description: "Current note path relative to vault root (`.md` added if missing)"
    )

    field(:destination, {:required, :string},
      description: "New note path relative to vault root (`.md` added if missing)"
    )

    field(:overwrite, :boolean,
      description: "Allow replacing an existing note at the destination (default: false)",
      default: false
    )
  end

  @impl true
  def execute(params, frame) do
    source = Map.get(params, :source)
    destination = Map.get(params, :destination)
    overwrite = Map.get(params, :overwrite, false)

    vault = Helpers.vault_path()
    src_path = Path.join(vault, Helpers.ensure_md(source))
    dst_path = Path.join(vault, Helpers.ensure_md(destination))

    with :ok <- Helpers.validate_in_vault(vault, src_path),
         :ok <- Helpers.validate_in_vault(vault, dst_path),
         :ok <- check_source_exists(src_path),
         :ok <- check_overwrite(dst_path, overwrite),
         :ok <- ensure_parent_dir(dst_path),
         :ok <- File.rename(src_path, dst_path) do
      src_rel = Path.relative_to(src_path, vault)
      dst_rel = Path.relative_to(dst_path, vault)

      resp =
        Response.tool()
        |> Response.text("Vault: #{vault}\n\nMoved: #{src_rel} → #{dst_rel}")

      {:reply, resp, frame}
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp check_source_exists(path) do
    if File.exists?(path) do
      :ok
    else
      {:error, "Source note does not exist: #{Path.basename(path)}"}
    end
  end

  defp check_overwrite(_path, true), do: :ok

  defp check_overwrite(path, false) do
    if File.exists?(path) do
      {:error,
       "Destination already exists: #{Path.basename(path)}. Use overwrite: true to replace it."}
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
