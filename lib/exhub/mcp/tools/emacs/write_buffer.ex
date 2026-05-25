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

    This tool can replace the entire buffer content or insert at a specific position.
    Use with caution as it modifies the editor state.

    Examples:
    - Replace entire buffer: {"buffer_name": "*scratch*", "content": "New content"}
    - Insert at position: {"buffer_name": "myfile.el", "content": "inserted text", "position": 100}
    - Append to buffer: {"buffer_name": "*scratch*", "content": "appended text", "mode": "append"}
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