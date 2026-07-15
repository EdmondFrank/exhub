defmodule Exhub.MCP.Tools.Emacs.CloseBuffer do
  @moduledoc """
  MCP Tool: emacs_close_buffer

  Close an Emacs buffer with option to save or discard changes.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Emacs.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "emacs_close_buffer"

  @impl true
  def description do
    """
    Close an Emacs buffer with control over unsaved changes.

    Use this tool to clean up buffers, dismiss temporary buffers, or manage
    the editor's buffer list. Handles unsaved changes according to the specified action.

    Parameters:
    - buffer_name: Name of the buffer to close (required)
    - action: How to handle unsaved changes (required):
      - "save": Save the buffer if modified, then close it
      - "discard": Close without saving, discarding any unsaved changes
      - "close": Close only if the buffer has no unsaved changes; returns an error if modified

    The "close" action is safe for buffers you expect to be clean. If the buffer
    has unsaved changes, the tool returns a message asking you to use "save" or "discard".

    Examples:
    - Save and close a file: {"buffer_name": "myfile.el", "action": "save"}
    - Discard scratch work: {"buffer_name": "*scratch*", "action": "discard"}
    - Close clean buffer: {"buffer_name": "*Help*", "action": "close"}
    """
  end

  schema do
    field(:buffer_name, {:required, :string}, description: "Name of the buffer to close")

    field(:action, {:required, :string},
      description:
        "Action to take: 'save' (save then close), 'discard' (close without saving), or 'close' (close only if not modified)"
    )
  end

  @impl true
  def execute(params, frame) do
    buffer_name = Map.get(params, :buffer_name)
    action = Map.get(params, :action)

    command = build_close_command(buffer_name, action)

    case Helpers.send_command(command) do
      {:ok, response} ->
        output = parse_close_response(response, buffer_name, action)
        resp = Response.tool() |> Response.text(output)
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to close buffer: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  defp build_close_command(buffer_name, "save") do
    """
    (with-current-buffer "#{Helpers.escape_elisp(buffer_name)}"
      (when (buffer-modified-p)
        (save-buffer))
      (kill-buffer)
      "OK")
    """
  end

  defp build_close_command(buffer_name, "discard") do
    """
    (with-current-buffer "#{Helpers.escape_elisp(buffer_name)}"
      (set-buffer-modified-p nil)
      (kill-buffer)
      "OK")
    """
  end

  defp build_close_command(buffer_name, "close") do
    """
    (with-current-buffer "#{Helpers.escape_elisp(buffer_name)}"
      (if (buffer-modified-p)
          "Buffer has unsaved changes. Use 'save' or 'discard' action."
        (progn
          (kill-buffer)
          "OK")))
    """
  end

  defp build_close_command(_buffer_name, action) do
    raise "Invalid action: #{action}. Use 'save', 'discard', or 'close'."
  end

  defp parse_close_response(response, buffer_name, action) do
    cleaned = String.trim(response)

    case cleaned do
      "\"OK\"" ->
        case action do
          "save" -> "Buffer '#{buffer_name}' saved and closed."
          "discard" -> "Buffer '#{buffer_name}' closed (changes discarded)."
          "close" -> "Buffer '#{buffer_name}' closed."
        end

      "\"Buffer has unsaved changes. Use 'save' or 'discard' action.\"" ->
        "Buffer '#{buffer_name}' has unsaved changes. Use action 'save' or 'discard'."

      _ ->
        "Buffer '#{buffer_name}' operation completed: #{cleaned}"
    end
  end
end
