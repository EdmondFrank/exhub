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

    Returns a list of entries with their type (file or directory), name, size,
    and last modification time. Optionally recurse into subdirectories up to
    a given depth.

    Parameters:
    - path: Absolute path to the directory to list
    - depth: How many levels deep to recurse (1 = immediate children only, default 1)
    - show_modified: Include last modified time in entries (default false)
    """
  end

  schema do
    field(:path, {:required, :string}, description: "Absolute path to the directory to list")
    field(:depth, :integer, description: "Recursion depth (1 = immediate children only, default 1)", default: 1)
    field(:show_modified, :boolean, description: "Include last modified time in entries (default false)", default: false)
  end

  @impl true
  def execute(params, frame) do
    path = Map.get(params, :path) |> Helpers.expand_path()
    depth = Map.get(params, :depth, 1)
    show_modified = Map.get(params, :show_modified, false)

    case list_directory(path, depth, show_modified) do
      {:ok, entries} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "path" => path,
            "count" => length(entries),
            "entries" => entries
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to list directory: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp list_directory(path, depth, show_modified) do
    case File.stat(path) do
      {:ok, %File.Stat{type: :directory}} ->
        entries = collect_entries(path, depth, 0, show_modified)
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

  defp collect_entries(dir, max_depth, current_depth, show_modified) do
    case File.ls(dir) do
      {:ok, names} ->
        names
        |> Enum.sort()
        |> Enum.flat_map(fn name ->
          full_path = Path.join(dir, name)

          case File.stat(full_path) do
            {:ok, stat} ->
              entry = %{
                "depth" => current_depth,
                "name" => name,
                "path" => full_path,
                "type" => stat_type(stat.type),
                "size" => humanize_size(stat.size)
              }

              entry = if show_modified, do: Map.put(entry, "modified", format_time(stat.mtime)), else: entry

              children =
                if stat.type == :directory and current_depth < max_depth do
                  collect_entries(full_path, max_depth, current_depth + 1, show_modified)
                else
                  []
                end

              [entry | children]

            {:error, _} ->
              []
          end
        end)

      {:error, _} ->
        []
    end
  end

  defp stat_type(:directory), do: "directory"
  defp stat_type(:regular), do: "file"
  defp stat_type(:symlink), do: "symlink"
  defp stat_type(other), do: to_string(other)

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
end
