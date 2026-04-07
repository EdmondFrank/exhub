defmodule Exhub.MCP.Tools.Desktop.ReadMultipleFiles do
  use Anubis.Server.Component, type: :tool

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Tools.DocExtract.Client

  def name, do: "read_multiple_files"

  @impl true
  def description do
    """
    Read multiple files in a single call. Returns results for each file with
    success status and content or error message. Supports optional offset/length
    for text files and document extraction for supported file types.
    """
  end

  schema do
    field(:paths, {:required, {:list, :string}}, description: "List of file paths to read")
    field(:offset, :integer, description: "Line number to start reading from (0-based, default 0)", default: 0)
    field(:length, :integer, description: "Maximum number of lines to read (default 2000)", default: 2000)
    field(:extract, :boolean, description: "Whether to extract text from document files (default: false)", default: false)
  end

  @impl true
  def execute(args, frame) do
    paths = Map.get(args, :paths, [])
    offset = Map.get(args, :offset, 0)
    length = Map.get(args, :length, 2000)
    extract = Map.get(args, :extract, false)

    results =
      paths
      |> Task.async_stream(
        fn path -> read_single_file(path, offset, length, extract) end,
        max_concurrency: 10,
        ordered: true
      )
      |> Enum.map(fn {:ok, result} -> result end)

    resp =
      Response.tool()
      |> Helpers.toon_response(%{"results" => results})

    {:reply, resp, frame}
  end

  defp read_single_file(path, offset, length, extract) do
    expanded_path = Helpers.expand_path(path)

    cond do
      extract and Client.document_type?(expanded_path) ->
        read_document(expanded_path, path)

      true ->
        read_text_file(expanded_path, path, offset, length)
    end
  end

  defp read_document(expanded_path, original_path) do
    case Client.extract(expanded_path) do
      {:ok, content} ->
        %{
          path: original_path,
          success: true,
          content: sanitize_utf8(content)
        }

      {:error, reason} ->
        %{
          path: original_path,
          success: false,
          error: reason
        }
    end
  end

  defp read_text_file(expanded_path, original_path, offset, length) do
    case read_file(expanded_path, offset, length) do
      {:ok, content, lines_read, total_lines} ->
        # Sanitize content to ensure valid UTF-8 for JSON encoding
        sanitized_content = sanitize_utf8(content)

        %{
          path: original_path,
          success: true,
          content: sanitized_content,
          lines_read: lines_read,
          total_lines: total_lines
        }

      {:error, reason} ->
        %{
          path: original_path,
          success: false,
          error: reason
        }
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
      {:error, reason} -> {:error, inspect(reason)}
    end
  end

  defp validate_path(path) do
    expanded = Path.expand(path)

    if String.starts_with?(expanded, "/") or String.starts_with?(expanded, System.user_home!()) do
      :ok
    else
      {:error, "Invalid path: #{path}"}
    end
  end

  # Sanitize string to ensure valid UTF-8 for JSON encoding
  defp sanitize_utf8(string) when is_binary(string) do
    string
    |> :unicode.characters_to_binary(:utf8, :utf8)
    |> case do
      {:error, good, bad} -> good <> replace_invalid(bad)
      {:incomplete, good, rest} -> good <> replace_invalid(rest)
      result when is_binary(result) -> result
    end
  end

  defp sanitize_utf8(other), do: other

  defp replace_invalid(<<_byte, rest::binary>>) do
    <<0xEF, 0xBF, 0xBD>> <> replace_invalid(rest)
  end

  defp replace_invalid(<<>>), do: <<>>
end
