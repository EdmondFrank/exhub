defmodule Exhub.MCP.Tools.Desktop.MoveFile do
  @moduledoc """
  MCP Tool: move_file

  Move or rename a file or directory on the filesystem.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "move_file"

  @impl true
  def description do
    """
    Move or rename a file or directory on the filesystem.

    Moves the source path to the destination path. If the destination's parent
    directory does not exist, it will be created automatically.

    Parameters:
    - source: Absolute path or ~ shorthand of the file or directory to move
    - destination: Absolute path or ~ shorthand of the new location
    """
  end

  schema do
    field(:source, {:required, :string}, description: "Absolute path or ~ shorthand of the file or directory to move")
    field(:destination, {:required, :string}, description: "Absolute path or ~ shorthand of the new location")
  end

  @impl true
  def execute(params, frame) do
    source = Map.get(params, :source) |> Helpers.expand_path()
    destination = Map.get(params, :destination) |> Helpers.expand_path()

    case move_file(source, destination) do
      :ok ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "source" => source,
            "destination" => destination,
            "message" => "Moved successfully."
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to move file: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp move_file(source, destination) do
    with :ok <- check_exists(source),
         :ok <- ensure_parent_dir(destination) do
      case File.rename(source, destination) do
        :ok ->
          :ok

        {:error, :exdev} ->
          # Cross-device move: copy then delete
          copy_and_delete(source, destination)

        {:error, :eacces} ->
          {:error, "Permission denied"}

        {:error, reason} ->
          {:error, inspect(reason)}
      end
    end
  end

  defp check_exists(path) do
    case File.exists?(path) do
      true -> :ok
      false -> {:error, "Source not found: #{path}"}
    end
  end

  defp ensure_parent_dir(path) do
    dir = Path.dirname(path)

    case File.mkdir_p(dir) do
      :ok -> :ok
      {:error, reason} -> {:error, "Cannot create destination directory: #{inspect(reason)}"}
    end
  end

  defp copy_and_delete(source, destination) do
    case File.cp_r(source, destination) do
      {:ok, _} ->
        File.rm_rf(source)
        :ok

      {:error, reason, _} ->
        {:error, "Copy failed: #{inspect(reason)}"}
    end
  end
end
