defmodule Exhub.MCP.Tools.Emacs.WriteBuffer do
  @moduledoc """
  MCP Tool: emacs_write_buffer

  Write content to a specific Emacs buffer.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Emacs.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "emacs_write_buffer"

  @impl true
  def description do
    """
    Write content to a specific Emacs buffer.

    Use this tool to insert, replace, or append text in an Emacs buffer.
    This modifies the editor state directly — use with caution on important buffers.

    Parameters:
    - buffer_name: Name of the target buffer (required). The buffer must already exist.
    - content: The text content to write (required)
    - mode: Write operation mode (default: "replace"):
      - "replace": Erases entire buffer content, then inserts new content
      - "insert": Inserts content at a specific line position (preserves existing content)
      - "append": Adds content at the end of the buffer
    - position: Line number for insert mode (1-based, default: 1). Ignored for other modes.

    Examples:
    - Replace buffer content: {"buffer_name": "*scratch*", "content": "New content"}
    - Insert at line 5: {"buffer_name": "notes.txt", "content": "Inserted text", "mode": "insert", "position": 5}
    - Append to buffer: {"buffer_name": "*Messages*", "content": "\nNew log entry", "mode": "append"}
    """
  end

  schema do
    field(:buffer_name, {:required, :string},
      description: "Name of the buffer to write to"
    )

    field(:content, {:required, :string},
      description: "Content to write to the buffer"
    )

    field(:mode, :string,
      description: "Write mode: 'replace' (default), 'insert', or 'append'",
      default: "replace"
    )

    field(:position, :integer,
      description: "Position to insert at (only for 'insert' mode, 1-based)"
    )
  end

  @impl true
  def execute(params, frame) do
    buffer_name = Map.get(params, :buffer_name)
    content = Map.get(params, :content)
    mode = Map.get(params, :mode, "replace")
    position = Map.get(params, :position)

    command = build_write_command(buffer_name, content, mode, position)

    case Helpers.send_command(command) do
      {:ok, _response} ->
        output = "Successfully wrote to buffer '#{buffer_name}' (#{mode} mode)"
        resp = Response.tool() |> Response.text(output)
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to write to buffer: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  defp build_write_command(buffer_name, content, "replace", _position) do
    """
    (with-current-buffer "#{Helpers.escape_elisp(buffer_name)}"
      (erase-buffer)
      (insert "#{Helpers.escape_elisp(content)}")
      "OK")
    """
  end

  defp build_write_command(buffer_name, content, "insert", position) do
    pos = position || 1

    """
    (with-current-buffer "#{Helpers.escape_elisp(buffer_name)}"
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- #{pos}))
        (insert "#{Helpers.escape_elisp(content)}")
        "OK"))
    """
  end

  defp build_write_command(buffer_name, content, "append", _position) do
    """
    (with-current-buffer "#{Helpers.escape_elisp(buffer_name)}"
      (goto-char (point-max))
      (insert "#{Helpers.escape_elisp(content)}")
      "OK")
    """
  end

  defp build_write_command(_buffer_name, _content, mode, _position) do
    raise "Invalid write mode: #{mode}. Use 'replace', 'insert', or 'append'."
  end
end