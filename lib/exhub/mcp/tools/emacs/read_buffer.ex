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

    Returns the entire content of the specified buffer as a string.
    This is useful for agents that need to examine the current state
    of an Emacs buffer.

    Examples:
    - Read buffer: {"buffer_name": "*scratch*"}
    - Read file buffer: {"buffer_name": "myfile.el"}
    - Read with line range: {"buffer_name": "myfile.el", "start_line": 10, "end_line": 20}
    """
  end

  schema do
    field(:buffer_name, {:required, :string},
      description: "Name of the buffer to read"
    )

    field(:start_line, :integer,
      description: "Start line number (1-based, optional)"
    )

    field(:end_line, :integer,
      description: "End line number (1-based, optional)"
    )
  end

  @impl true
  def execute(params, frame) do
    buffer_name = Map.get(params, :buffer_name)
    start_line = Map.get(params, :start_line)
    end_line = Map.get(params, :end_line)

    command = build_read_command(buffer_name, start_line, end_line)

    case Helpers.send_command(command) do
      {:ok, response} ->
        content = parse_buffer_content(response)

        output =
          if start_line || end_line do
            "Buffer '#{buffer_name}' (lines #{start_line || 1}-#{end_line || "end"}):\n\n#{content}"
          else
            "Buffer '#{buffer_name}' (#{String.length(content)} chars):\n\n#{content}"
          end

        resp = Response.tool() |> Response.text(output)
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to read buffer: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  defp build_read_command(buffer_name, nil, nil) do
    """
    (with-current-buffer "#{Helpers.escape_elisp(buffer_name)}"
      (buffer-substring-no-properties (point-min) (point-max)))
    """
  end

  defp build_read_command(buffer_name, start_line, end_line) do
    start = start_line || 1
    end_val = end_line || "nil"

    """
    (with-current-buffer "#{Helpers.escape_elisp(buffer_name)}"
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