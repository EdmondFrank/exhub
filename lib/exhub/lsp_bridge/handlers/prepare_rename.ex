defmodule Exhub.LspBridge.Handlers.PrepareRename do
  @moduledoc """
  `textDocument/prepareRename` — port of `core/handler/prepare_rename.py`.

  Asks the server for the range the rename applies to (used to highlight and
  to learn the placeholder). gopls answers `{range, placeholder}`; most servers
  answer a bare `Range`. Both are normalised to a range.
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "prepare-rename"

  @impl true
  def method, do: "textDocument/prepareRename"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "prepare_rename"

  @impl true
  def request_params(%{"position" => position}, _ctx), do: %{"position" => position}
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(nil, _ctx), do: {:message, "No rename range."}

  # gopls: {range, placeholder}
  def process_response(%{"range" => range}, ctx) when is_map(range),
    do: {:rename_range, ctx.path, range}

  # standard: a bare Range
  def process_response(%{"start" => _} = range, ctx), do: {:rename_range, ctx.path, range}

  def process_response(_other, _ctx), do: {:message, "No rename range."}
end
