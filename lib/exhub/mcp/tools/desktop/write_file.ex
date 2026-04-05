defmodule Exhub.MCP.Tools.Desktop.WriteFile do
  @moduledoc """
  MCP Tool: write_file

  Write or append content to a file on the filesystem.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  def name, do: "write_file"

  @impl true
  def description do
    """
    Write content to a file on the filesystem.

    Supports two modes:
    - rewrite: Overwrite the entire file with new content (default)
    - append: Append content to the end of an existing file

    Creates the file and any missing parent directories if they do not exist.

    Parameters:
    - path: Absolute path to the file to write
    - content: The text content to write
    - mode: "rewrite" (default) or "append"
    """
  end

  schema do
    field(:path, {:required, :string}, description: "Absolute path to the file to write")
    field(:content, {:required, :string}, description: "The text content to write to the file")
    field(:mode, :string, description: "Write mode: \"rewrite\" (default) or \"append\"", default: "rewrite")
  end

  @impl true
  def execute(params, frame) do
    path = Map.get(params, :path)
    content = Map.get(params, :content)
    mode = Map.get(params, :mode, "rewrite")

    case write_file(path, content, mode) do
      :ok ->
        resp =
          Response.tool()
          |> Response.structured(%{
            "success" => true,
            "path" => path,
            "mode" => mode,
            "bytes_written" => byte_size(content)
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to write file: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp write_file(path, content, mode) do
    with :ok <- validate_path(path),
         :ok <- ensure_parent_dir(path) do
      case mode do
        "append" -> File.write(path, content, [:append])
        _ -> File.write(path, content)
      end
      |> case do
        :ok -> :ok
        {:error, :eacces} -> {:error, "Permission denied: #{path}"}
        {:error, reason} -> {:error, inspect(reason)}
      end
    end
  end

  defp ensure_parent_dir(path) do
    dir = Path.dirname(path)

    case File.mkdir_p(dir) do
      :ok -> :ok
      {:error, reason} -> {:error, "Cannot create directory #{dir}: #{inspect(reason)}"}
    end
  end

  defp validate_path(path) when is_binary(path) and path != "", do: :ok
  defp validate_path(_), do: {:error, "Invalid path"}
end
