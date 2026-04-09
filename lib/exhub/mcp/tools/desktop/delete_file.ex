defmodule Exhub.MCP.Tools.Desktop.DeleteFile do
  @moduledoc """
  MCP Tool: delete_file

  Delete a file or directory from the filesystem.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "delete_file"

  @impl true
  def description do
    """
    Delete a file or directory from the filesystem.

    For directories, set recursive to true to delete the directory and all its
    contents. By default, only empty directories can be deleted.

    Parameters:
    - path: Absolute path or ~ shorthand to the file or directory to delete
    - recursive: If true, delete directories and their contents recursively (default false)
    """
  end

  schema do
    field(:path, {:required, :string}, description: "Absolute path or ~ shorthand to the file or directory to delete")
    field(:recursive, :boolean, description: "If true, delete directories recursively (default false)", default: false)
  end

  @impl true
  def execute(params, frame) do
    path = Map.get(params, :path) |> Helpers.expand_path()
    recursive = Map.get(params, :recursive, false)

    case delete_path(path, recursive) do
      :ok ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "path" => path,
            "message" => "Deleted successfully."
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to delete: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp delete_path(path, recursive) do
    case File.stat(path) do
      {:ok, %File.Stat{type: :directory}} ->
        if recursive do
          case File.rm_rf(path) do
            {:ok, _} -> :ok
            {:error, reason, _} -> {:error, inspect(reason)}
          end
        else
          case File.rmdir(path) do
            :ok -> :ok
            {:error, :enotempty} -> {:error, "Directory is not empty. Use recursive: true to delete non-empty directories."}
            {:error, :eacces} -> {:error, "Permission denied: #{path}"}
            {:error, reason} -> {:error, inspect(reason)}
          end
        end

      {:ok, _} ->
        case File.rm(path) do
          :ok -> :ok
          {:error, :eacces} -> {:error, "Permission denied: #{path}"}
          {:error, reason} -> {:error, inspect(reason)}
        end

      {:error, :enoent} ->
        {:error, "Path not found: #{path}"}

      {:error, :eacces} ->
        {:error, "Permission denied: #{path}"}

      {:error, reason} ->
        {:error, inspect(reason)}
    end
  end
end
