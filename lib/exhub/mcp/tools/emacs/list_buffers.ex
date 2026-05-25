defmodule Exhub.MCP.Tools.Emacs.ListBuffers do
  @moduledoc """
  MCP Tool: emacs_list_buffers

  List all open buffers in Emacs.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.Emacs.Helpers

  use Anubis.Server.Component, type: :tool

  def name, do: "emacs_list_buffers"

  @impl true
  def description do
    """
    List all open buffers in Emacs.

    Returns a list of buffer names currently open in the Emacs editor.
    This includes both file-visiting buffers and non-file buffers like
    *scratch*, *Messages*, etc.

    Examples:
    - List all buffers: {}
    - List buffers with details: {"include_details": true}
    """
  end

  schema do
    field(:include_details, :boolean,
      description: "Whether to include buffer details like size and mode (default: false)",
      default: false
    )
  end

  @impl true
  def execute(params, frame) do
    include_details = Map.get(params, :include_details, false)

    command =
      if include_details do
        "(mapcar (lambda (buf) (list (buffer-name buf) (buffer-size buf) (with-current-buffer buf major-mode))) (buffer-list))"
      else
        "(mapcar 'buffer-name (buffer-list))"
      end

    case Helpers.send_command(command) do
      {:ok, response} ->
        output =
          if include_details do
            buffers = Helpers.parse_detailed_buffer_list(response)
            format_detailed_buffer_list(buffers)
          else
            buffers = Helpers.parse_buffer_list(response)
            Helpers.format_buffer_list(buffers)
          end

        resp = Response.tool() |> Response.text(output)
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to list buffers: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  defp format_detailed_buffer_list(buffers) do
    if buffers == [] do
      "No buffers found."
    else
      header = "#{length(buffers)} buffer(s):\n\n"

      body =
        Enum.map_join(buffers, "\n", fn
          {name, size, mode} ->
            "  - #{name} (#{size} bytes, #{mode})"
          name when is_binary(name) ->
            "  - #{name}"
        end)

      header <> body
    end
  end
end