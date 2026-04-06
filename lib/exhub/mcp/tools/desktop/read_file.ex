defmodule Exhub.MCP.Tools.Desktop.ReadFile do
  @moduledoc """
  MCP Tool: read_file

  Read the contents of a file from the filesystem with optional line offset and limit.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "read_file"

  @impl true
  def description do
    """
    Read the contents of a file from the filesystem.

    Supports reading plain text files with optional line offset and length limits.
    Returns the file content as text. For large files, use offset and length to
    read specific portions.

    Parameters:
    - path: Absolute path to the file to read
    - offset: Line number to start reading from (0-based, default 0)
    - length: Maximum number of lines to read (default 1000)
    """
  end

  schema do
    field(:path, {:required, :string}, description: "Absolute path to the file to read")
    field(:offset, :integer, description: "Line number to start reading from (0-based, default 0)", default: 0)
    field(:length, :integer, description: "Maximum number of lines to read (default 1000)", default: 1000)
  end

  @impl true
  def execute(params, frame) do
    path = Map.get(params, :path) |> Helpers.expand_path()
    offset = Map.get(params, :offset, 0)
    length = Map.get(params, :length, 1000)

    case read_file(path, offset, length) do
      {:ok, content, lines_read, total_lines} ->
        resp =
          Response.tool()
          |> Helpers.toon_response(%{
            "path" => path,
            "offset" => offset,
            "lines_read" => lines_read,
            "total_lines" => total_lines,
            "content" => content
          })

        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to read file: #{reason}")
        {:reply, resp, frame}
    end
  end

  defp read_file(path, offset, max_lines) do
    with :ok <- validate_path(path),
         {:ok, content} <- File.read(path) do
      lines = String.split(content, "\n")
      total_lines = length(lines)
      sliced = lines |> Enum.drop(offset) |> Enum.take(max_lines)
      lines_read = Enum.count(sliced)
      {:ok, Enum.join(sliced, "\n"), lines_read, total_lines}
    else
      {:error, :enoent} -> {:error, "File not found: #{path}"}
      {:error, :eacces} -> {:error, "Permission denied: #{path}"}
      {:error, :eisdir} -> {:error, "Path is a directory: #{path}"}
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp validate_path(path) when is_binary(path) and path != "", do: :ok
  defp validate_path(_), do: {:error, "Invalid path"}
end
