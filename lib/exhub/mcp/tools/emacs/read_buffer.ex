defmodule Exhub.MCP.Tools.Emacs.ReadBuffer do
  @moduledoc """
  MCP Tool: emacs_read_buffer

  Read the content of a specific Emacs buffer.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Emacs.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "emacs_read_buffer"

  @impl true
  def description do
    """
    Read the content of a specific Emacs buffer.

    Use this tool to examine the current state of an Emacs buffer, inspect file
    contents loaded in the editor, or extract text for analysis. Returns the
    buffer content as a string.

    Parameters:
    - buffer_name: Name of the buffer to read (required). Use emacs_list_buffers
      to discover available buffer names. Common names: "*scratch*", "*Messages*",
      or the filename for file-visiting buffers.
    - start_line: Optional 1-based line number to start reading from
    - end_line: Optional 1-based line number to stop reading at (inclusive)
    - reverse: Optional boolean (default false). When true, start_line/end_line
      are counted from the END of the buffer (1 = last line). Ideal for log
      buffers where new content is appended at the bottom — fetch the latest
      lines directly without scanning from the top.

    When start_line/end_line are omitted, returns the entire buffer content.
    Character count is included in the response header.

    Examples:
    - Read entire buffer: {"buffer_name": "*scratch*"}
    - Read file buffer: {"buffer_name": "myfile.el"}
    - Read specific lines: {"buffer_name": "myfile.el", "start_line": 10, "end_line": 20}
    - Read from line to end: {"buffer_name": "config.el", "start_line": 50}
    - Read last 100 lines of a log: {"buffer_name": "*exhub*", "reverse": true, "end_line": 100}
    """
  end

  schema do
    field(:buffer_name, {:required, :string}, description: "Name of the buffer to read")

    field(:start_line, :integer, description: "Start line number (1-based, optional)")

    field(:end_line, :integer, description: "End line number (1-based, optional)")

    field(:reverse, :boolean,
      description:
        "Count start_line/end_line from the END of the buffer (1 = last line). e.g. {reverse: true, end_line: N} returns the last N lines",
      default: false
    )
  end

  @impl true
  def execute(params, frame) do
    buffer_name = Map.get(params, :buffer_name)
    start_line = Map.get(params, :start_line)
    end_line = Map.get(params, :end_line)
    reverse? = Map.get(params, :reverse, false) == true

    command = build_read_command(buffer_name, start_line, end_line, reverse?)

    case Helpers.send_command(command) do
      {:ok, response} ->
        content = parse_buffer_content(response)

        output =
          cond do
            reverse? && (start_line || end_line) ->
              range =
                case {start_line, end_line} do
                  {s, e} when s != nil and e != nil -> "bottom lines #{s}-#{e}"
                  {nil, e} -> "last #{e} lines"
                  {s, nil} -> "bottom lines #{s}-end"
                end

              "Buffer '#{buffer_name}' (reverse, #{range}):\n\n#{content}"

            start_line || end_line ->
              "Buffer '#{buffer_name}' (lines #{start_line || 1}-#{end_line || "end"}):\n\n#{content}"

            true ->
              "Buffer '#{buffer_name}' (#{String.length(content)} chars):\n\n#{content}"
          end

        resp = Response.tool() |> Response.text(output)
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to read buffer: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  @doc false
  def build_read_command(buffer_name, start_line, end_line, reverse \\ false) do
    name = Helpers.escape_elisp(buffer_name)

    if reverse do
      build_reverse_command(name, start_line, end_line)
    else
      build_forward_command(name, start_line, end_line)
    end
  end

  defp build_forward_command(name, nil, nil) do
    """
    (with-current-buffer "#{name}"
      (buffer-substring-no-properties (point-min) (point-max)))
    """
  end

  defp build_forward_command(name, start_line, end_line) do
    start = start_line || 1
    end_val = end_line || "nil"

    """
    (with-current-buffer "#{name}"
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- #{start}))
        (let ((start-pos (point)))
          (if (eq #{end_val} nil)
              (buffer-substring-no-properties start-pos (point-max))
            (forward-line (1- (- #{end_val} #{start})))
            (buffer-substring-no-properties start-pos (point))))))
    """
  end

  # Reverse mode: start_line/end_line count from the bottom of the buffer.
  # Total line count is resolved inside Emacs so a single round trip suffices;
  # reverse bounds are translated into forward bounds before slicing.
  defp build_reverse_command(name, start_line, end_line) do
    rev_s = start_line || "nil"
    rev_e = end_line || "nil"

    """
    (with-current-buffer "#{name}"
      (save-excursion
        (goto-char (point-max))
        (let* ((total (line-number-at-pos (point)))
               (rev-s (or #{rev_s} 1))
               (rev-e (or #{rev_e} total))
               (fwd-start (max 1 (+ 1 (- total rev-e))))
               (fwd-end (min total (+ 1 (- total rev-s)))))
          (goto-char (point-min))
          (forward-line (1- fwd-start))
          (let ((start-pos (point)))
            (if (>= fwd-end total)
                (buffer-substring-no-properties start-pos (point-max))
              (forward-line (1- (- fwd-end fwd-start)))
              (buffer-substring-no-properties start-pos (point)))))))
    """
  end

  defp parse_buffer_content(response) do
    # Remove surrounding quotes if present
    cleaned =
      response
      |> String.trim()
      |> String.trim("\"")

    # Unescape Elisp string escaping
    cleaned
    |> String.replace("\\n", "\n")
    |> String.replace("\\t", "\t")
    |> String.replace("\\\\", "\\")
    |> String.replace("\\\"", "\"")
  end
end
