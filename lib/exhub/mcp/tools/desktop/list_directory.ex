defmodule Exhub.MCP.Tools.Desktop.ListDirectory do
  @moduledoc """
  MCP Tool: list_directory

  List the contents of a directory on the filesystem.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "list_directory"

  @impl true
  def description do
    """
    List the contents of a directory on the filesystem.

    Returns a list of entries with path and size. Directory entries are suffixed
    with "/" to distinguish them from files. Optionally recurse into subdirectories
    up to a given depth. Entry paths are relative to the requested directory.

    Parameters:
    - path: Absolute path to the directory to list
    - depth: How many levels deep to recurse (0 = immediate children only, default 0)
    - show_modified: Include last modified time in entries (default false)
    - pattern: Glob pattern to filter entries (e.g. "*.rb", "**/*.ex"). Directories are
      always recursed into, but only entries matching the pattern are included in results.
      Default nil (no filter).
    """
  end

  schema do
    field(:path, {:required, :string}, description: "Absolute path to the directory to list")
    field(:depth, :integer, description: "Recursion depth (0 = immediate children only, default 0)", default: 0)
    field(:show_modified, :boolean, description: "Include last modified time in entries (default false)", default: false)
    field(:pattern, :string, description: "Glob pattern to filter entries (e.g. *.rb, **/*.ex)", default: nil)
  end

  @impl true
  def execute(params, frame) do
    path = Map.get(params, :path) |> Helpers.expand_path()
    depth = Map.get(params, :depth, 0)
    show_modified = Map.get(params, :show_modified, false)
    pattern = Map.get(params, :pattern)

    case list_directory(path, depth, show_modified, pattern) do
      {:ok, entries} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{"entries" => entries})

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to list directory: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp list_directory(path, depth, show_modified, pattern) do
    case File.stat(path) do
      {:ok, %File.Stat{type: :directory}} ->
        entries = collect_entries(path, path, depth, 0, show_modified, pattern)
        {:ok, entries}

      {:ok, _} ->
        {:error, "Not a directory: #{path}"}

      {:error, :enoent} ->
        {:error, "Directory not found: #{path}"}

      {:error, :eacces} ->
        {:error, "Permission denied: #{path}"}

      {:error, reason} ->
        {:error, inspect(reason)}
    end
  end

  defp collect_entries(root, dir, max_depth, current_depth, show_modified, pattern) do
    case File.ls(dir) do
      {:ok, names} ->
        names
        |> Enum.sort()
        |> Enum.flat_map(fn name ->
          full_path = Path.join(dir, name)
          relative_path = Path.relative_to(full_path, root)

          case File.stat(full_path) do
            {:ok, stat} ->
              is_dir = stat.type == :directory

              children =
                if is_dir and current_depth < max_depth do
                  collect_entries(root, full_path, max_depth, current_depth + 1, show_modified, pattern)
                else
                  []
                end

              # Determine if this entry should be included
              include_entry = matches_pattern?(name, pattern)

              if include_entry do
                display_path = if is_dir, do: relative_path <> "/", else: relative_path
                entry = %{
                  "path" => display_path,
                  "size" => humanize_size(stat.size)
                }

                entry = if show_modified, do: Map.put(entry, "modified", format_time(stat.mtime)), else: entry
                [entry | children]
              else
                children
              end

            {:error, _} ->
              []
          end
        end)

      {:error, _} ->
        []
    end
  end

defp format_time({{y, mo, d}, {h, mi, s}}) do
    :io_lib.format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B", [y, mo, d, h, mi, s])
    |> IO.iodata_to_binary()
  end

  defp format_time(_), do: "unknown"

  defp humanize_size(bytes) when bytes < 1024 do
    "#{bytes} B"
  end

  defp humanize_size(bytes) when bytes < 1024 * 1024 do
    kb = bytes / 1024
    "#{:erlang.float_to_binary(kb, decimals: 1)} KB"
  end

  defp humanize_size(bytes) when bytes < 1024 * 1024 * 1024 do
    mb = bytes / (1024 * 1024)
    "#{:erlang.float_to_binary(mb, decimals: 1)} MB"
  end

  defp humanize_size(bytes) do
    gb = bytes / (1024 * 1024 * 1024)
    "#{:erlang.float_to_binary(gb, decimals: 1)} GB"
  end

  defp matches_pattern?(_name, nil), do: true
  defp matches_pattern?(name, pattern) do
    regex =
      pattern
      |> Regex.escape()
      |> String.replace("\\*\\*", ".*")
      |> String.replace("\\*", "[^/]*")
      |> String.replace("\\?", "[^/]")
      |> then(&("^" <> &1 <> "$"))

    case Regex.compile(regex) do
      {:ok, re} -> Regex.match?(re, name)
      _ -> false
    end
  end
end
