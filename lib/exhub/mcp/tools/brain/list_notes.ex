defmodule Exhub.MCP.Tools.Brain.ListNotes do
  @moduledoc """
  MCP Tool: brain_list_notes

  List notes in the Obsidian vault, optionally scoped to a subfolder.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Brain.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "brain_list_notes"

  @impl true
  def description do
    """
    List entries in the Obsidian brain vault.

    Returns a list of note paths and directory entries relative to the vault root.
    Directories are indicated with a trailing `/`.
    Optionally scope to a subfolder and control recursion depth.

    Examples:
    - All notes:       { }
    - In folder:       { "folder": "journal/2024" }
    - Non-recursive:   { "folder": "projects", "recursive": false }
    - Absolute paths:  { "abs_path": true }
    """
  end

  schema do
    field(:folder, :string,
      description: "Optional subfolder path relative to vault root to list notes in"
    )

    field(:recursive, :boolean,
      description: "Whether to list notes recursively (default: true)",
      default: true
    )

    field(:abs_path, :boolean,
      description: "Return absolute paths instead of relative (default: false)",
      default: false
    )
  end

  @impl true
  def execute(params, frame) do
    folder = Map.get(params, :folder)
    recursive = Map.get(params, :recursive, true)
    abs_path = Map.get(params, :abs_path, false)

    vault = Helpers.vault_path()
    search_dir = if folder, do: Path.join(vault, folder), else: vault

    with :ok <- Helpers.validate_in_vault(vault, search_dir) do
      entries =
        if recursive do
          list_recursive_with_dirs(vault, search_dir)
        else
          list_flat(search_dir, vault)
        end

      entries = if abs_path do
        Enum.map(entries, fn e ->
          if String.ends_with?(e, "/") do
            Path.join(vault, String.trim_trailing(e, "/")) <> "/"
          else
            Path.join(vault, e)
          end
        end)
      else
        entries
      end

      if entries == [] do
        resp = Response.tool() |> Response.text("Vault: #{vault}\n\nNo entries found.")
        {:reply, resp, frame}
      else
        output = Enum.join(entries, "\n")
        resp = Response.tool() |> Response.text("Vault: #{vault}\n\n#{length(entries)} entries(s):\n\n#{output}")
        {:reply, resp, frame}
      end
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp list_flat(dir, vault) do
    case File.ls(dir) do
      {:ok, entries} ->
        dirs =
          entries
          |> Enum.filter(fn e -> File.dir?(Path.join(dir, e)) end)
          |> Enum.map(fn e -> Path.relative_to(Path.join(dir, e), vault) <> "/" end)
          |> Enum.sort()

        files =
          entries
          |> Enum.filter(&String.ends_with?(&1, ".md"))
          |> Enum.map(&Path.relative_to(Path.join(dir, &1), vault))
          |> Enum.sort()

        dirs ++ files

      {:error, _} ->
        []
    end
  end

  defp list_recursive_with_dirs(vault_path, dir_path) do
    case File.ls(dir_path) do
      {:ok, entries} ->
        {dirs, files} =
          Enum.reduce(entries, {[], []}, fn entry, {dirs, files} ->
            full = Path.join(dir_path, entry)

            cond do
              File.dir?(full) ->
                rel = Path.relative_to(full, vault_path) <> "/"
                child_entries = list_recursive_with_dirs(vault_path, full)
                {[{rel, child_entries} | dirs], files}

              String.ends_with?(entry, ".md") ->
                rel = Path.relative_to(full, vault_path)
                {dirs, [rel | files]}

              true ->
                {dirs, files}
            end
          end)

        sorted_dirs = dirs |> Enum.sort_by(fn {name, _} -> name end)
        sorted_files = Enum.sort(files)

        Enum.flat_map(sorted_dirs, fn {name, children} ->
          [name | children]
        end) ++ sorted_files

      {:error, _} ->
        []
    end
  end
end
