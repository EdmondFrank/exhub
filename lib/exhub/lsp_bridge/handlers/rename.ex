defmodule Exhub.LspBridge.Handlers.Rename do
  @moduledoc """
  `textDocument/rename` — port of `core/handler/rename.py`.

  Returns a `WorkspaceEdit`; the elisp front end applies it to the affected
  buffers (multifile edits, sorted descending). A `nil` result means the server
  found nothing to rename.
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "rename"

  @impl true
  def method, do: "textDocument/rename"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "rename"

  @impl true
  def request_params(%{"position" => position, "newName" => new_name}, _ctx) do
    %{"position" => position, "newName" => new_name}
  end

  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(nil, _ctx), do: {:message, "No rename found."}

  def process_response(%{} = edit, _ctx), do: {:workspace_edit, edit, "Rename done."}

  def process_response(_other, _ctx), do: {:message, "No rename found."}
end
