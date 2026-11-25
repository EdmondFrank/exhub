defmodule Exhub.MCP.Tools.Desktop.ReadFile do
  @moduledoc """
  MCP Tool: read_file

  Read the contents of a file from the filesystem with optional line offset and limit.
  Supports both plain text files and document files (PDF, DOCX, images, etc.) via
  automatic text extraction.

  Targeted at agents rather than humans:

    * `with_lines: true` prefixes every returned line with its 1-based file line
      number, so an edit or a follow-up read can aim at a line instead of
      guessing an offset (`numbered/2`).
    * every response carries a short content digest (`sha256`), and passing it
      back as `since_hash` short-circuits an unchanged file to metadata only
      (`unchanged?/2`) — a re-read costs a few tokens instead of the file.

  See `docs/modules/desktop.md` for the payload shapes.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Desktop.Helpers
  alias Exhub.MCP.Desktop.Nanoxml
  alias Exhub.MCP.Tools.DocExtract.Client

  use Anubis.Server.Component, type: :tool

  # Files above this size are still read for their content but not hashed: a
  # digest is only worth computing when the caller can pass it back. Document
  # extraction reads the file a second time, so it skips the digest entirely
  # above this bound.
  @hash_max_bytes 8_000_000
  @hash_chars 12

  def name, do: "read_file"

  @impl true
  def description do
    """
    Read the contents of a file from the filesystem.

    Supports both plain text files and document files (PDF, DOCX, images, etc.).

    For plain text files: use offset and length to read specific portions.
    Set with_lines to prefix each line with its 1-based file line number
    (`42|text`) and target edits or re-reads at a line. Do not copy the prefix
    into edit_block strings.

    Every response carries sha256 (12 hex chars of the file digest) and mtime.
    Pass sha256 back as since_hash and an unchanged file returns
    `unchanged: true` instead of the content.

    For document files (PDF, DOCX, DOC, PNG, JPG, JPEG, TIFF, BMP, GIF, WEBP):
    automatically extracts text content using OCR/document parsing. The extract
    parameter controls whether extraction is attempted (default: true).

    """
  end

  schema do
    field(:path, {:required, :string},
      description: "Absolute path or ~ shorthand to the file to read"
    )

    field(:offset, :integer,
      description:
        "Line number to start reading from (0-based, e.g. 100 skips first 100 lines, default 0) - only for text files",
      default: 0
    )

    field(:length, :integer,
      description: "Maximum number of lines to read (default 1000) - only for text files",
      default: 1000
    )

    field(:with_lines, :boolean,
      description:
        "Prefix each line with its 1-based file line number (`12|text`). Default false; the prefix is not part of the file",
      default: false
    )

    field(:since_hash, :string,
      description:
        "A `sha256` from an earlier read; when the file still hashes to it, only `unchanged: true` plus metadata is returned"
    )

    field(:extract, :boolean,
      description: "Whether to extract text from document files (default: true)",
      default: true
    )
  end

  @impl true
  def execute(params, frame) do
    with {:ok, path} <- Helpers.resolve_path(Map.get(params, :path), frame) do
      cond do
        is_nil(path) or path == "" -> error(frame, "`path` is required")
        true -> read(path, params, frame)
      end
    else
      {:error, reason} -> error(frame, reason)
    end
  end

  # ---------------------------------------------------------------------------
  # Reading
  # ---------------------------------------------------------------------------

  defp read(path, params, frame) do
    case File.stat(path, time: :posix) do
      {:ok, %File.Stat{type: :directory}} ->
        error(frame, "Failed to read file: Path is a directory: #{path}")

      {:ok, %File.Stat{} = stat} ->
        case extraction_kind(path, params) do
          :none -> read_text(path, params, stat, frame)
          :office -> read_extracted(path, :office, stat, params, frame)
          :document -> read_extracted(path, :document, stat, params, frame)
        end

      {:error, reason} ->
        error(frame, "Failed to read file: #{describe_fs_error(path, reason)}")
    end
  end

  defp read_text(path, params, stat, frame) do
    case File.read(path) do
      {:ok, binary} ->
        sha = digest(binary)
        stamp = stamp(sha, stat)

        if unchanged?(Map.get(params, :since_hash), sha) do
          reply(frame, Map.merge(%{"path" => path, "unchanged" => true}, stamp))
        else
          text(path, binary, params, stamp, frame)
        end

      {:error, reason} ->
        error(frame, "Failed to read file: #{describe_fs_error(path, reason)}")
    end
  end

  defp read_extracted(path, kind, stat, params, frame) do
    sha = if stat.size <= @hash_max_bytes, do: digest_of(path)
    stamp = stamp(sha, stat)

    if unchanged?(Map.get(params, :since_hash), sha) do
      reply(frame, Map.merge(%{"path" => path, "unchanged" => true}, stamp))
    else
      case kind do
        :office -> office(path, stamp, frame)
        :document -> document(path, stamp, frame)
      end
    end
  end

  defp office(path, stamp, frame) do
    case Nanoxml.extract_text(path) do
      {:ok, content} ->
        reply(
          frame,
          Map.merge(%{"path" => path, "extraction" => true, "content" => content}, stamp)
        )

      {:error, reason} ->
        error(frame, "Office file extraction failed: #{reason}")
    end
  end

  defp document(path, stamp, frame) do
    case Client.extract(path) do
      {:ok, content} ->
        reply(
          frame,
          Map.merge(%{"path" => path, "extraction" => true, "content" => content}, stamp)
        )

      {:error, reason} ->
        error(frame, "Document extraction failed: #{reason}")
    end
  end

  defp text(path, binary, params, stamp, frame) do
    offset = params |> Map.get(:offset, 0) |> max(0)
    length = params |> Map.get(:length, 1000) |> max(1)

    lines = split_lines(binary)
    sliced = lines |> Enum.drop(offset) |> Enum.take(length)

    content =
      if Map.get(params, :with_lines, false) do
        numbered(sliced, offset)
      else
        Enum.join(sliced, "\n")
      end

    reply(
      frame,
      Map.merge(
        %{
          "path" => path,
          "offset" => offset,
          "lines_read" => length(sliced),
          "total_lines" => length(lines),
          "content" => content
        },
        stamp
      )
    )
  end

  # A trailing newline terminates the last line, it does not start an empty one:
  # otherwise a numbered read shows a phantom `N|` row and total_lines is off by
  # one for every file a shell wrote.
  defp split_lines(""), do: []

  defp split_lines(binary) do
    binary =
      if String.ends_with?(binary, "\n") do
        binary_part(binary, 0, byte_size(binary) - 1)
      else
        binary
      end

    String.split(binary, "\n")
  end

  # `42|line` — 1-based, unpadded to keep the payload small. `offset` stays
  # 0-based, so line N is read with `offset: N - 1`.
  defp numbered(lines, offset) do
    lines
    |> Enum.with_index(offset + 1)
    |> Enum.map_join("\n", fn {line, number} -> "#{number}|#{line}" end)
  end

  defp extraction_kind(path, params) do
    if Map.get(params, :extract, true) == false do
      :none
    else
      cond do
        Client.office_type?(path) -> :office
        Client.document_type?(path) -> :document
        true -> :none
      end
    end
  end

  # ---------------------------------------------------------------------------
  # Digest
  # ---------------------------------------------------------------------------

  defp digest(binary) when is_binary(binary) and byte_size(binary) <= @hash_max_bytes do
    :crypto.hash(:sha256, binary)
    |> Base.encode16(case: :lower)
    |> binary_part(0, @hash_chars)
  end

  defp digest(_binary), do: nil

  defp digest_of(path) do
    case File.read(path) do
      {:ok, binary} -> digest(binary)
      {:error, _reason} -> nil
    end
  end

  defp stamp(sha, stat) do
    %{"sha256" => sha, "mtime" => stat.mtime, "size" => stat.size}
  end

  defp unchanged?(since, sha) when is_binary(since) and since != "" and is_binary(sha),
    do: since == sha

  defp unchanged?(_since, _sha), do: false

  defp describe_fs_error(path, :enoent), do: "File not found: #{path}"
  defp describe_fs_error(path, :eacces), do: "Permission denied: #{path}"
  defp describe_fs_error(path, :eisdir), do: "Path is a directory: #{path}"
  defp describe_fs_error(_path, reason), do: inspect(reason)

  defp reply(frame, payload) do
    resp = Response.tool() |> Helpers.toon_response(payload)
    {:reply, resp, frame}
  end

  defp error(frame, message) do
    resp = Response.tool() |> Response.error(message)
    {:reply, resp, frame}
  end
end
