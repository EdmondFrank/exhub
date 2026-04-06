defmodule Exhub.MCP.Tools.Desktop.GetFileInfo do
  @moduledoc """
  MCP Tool: get_file_info

  Get metadata about a file or directory on the filesystem.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "get_file_info"

  @impl true
  def description do
    """
    Get metadata about a file or directory on the filesystem.

    Returns information including type, size, permissions, and timestamps.

    Parameters:
    - path: Absolute path to the file or directory
    """
  end

  schema do
    field(:path, {:required, :string}, description: "Absolute path to the file or directory")
  end

  @impl true
  def execute(params, frame) do
    path = Map.get(params, :path) |> Helpers.expand_path()

    case File.stat(path) do
      {:ok, stat} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "path" => path,
            "type" => stat_type(stat.type),
            "size" => stat.size,
            "access" => stat.access,
            "atime" => format_time(stat.atime),
            "mtime" => format_time(stat.mtime),
            "ctime" => format_time(stat.ctime),
            "mode" => stat.mode,
            "links" => stat.links,
            "uid" => stat.uid,
            "gid" => stat.gid
          })

        {:reply, resp, frame}

      {:error, :enoent} ->
        resp = Response.tool() |> Response.error("Path not found: #{path}")
        {:reply, resp, frame}

      {:error, :eacces} ->
        resp = Response.tool() |> Response.error("Permission denied: #{path}")
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to get file info: #{inspect(reason)}")
        {:reply, resp, frame}
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
end
