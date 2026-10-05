defmodule Exhub.LspBridge.Handlers.RangeFormatting do
  @moduledoc """
  `textDocument/rangeFormatting` — port of `core/handler/range_formatting.py`.

  Like `Formatting`, but scoped to a range; the elisp front end sends the
  active region. Results are applied the same way (descending `TextEdit`s).
  """

  @behaviour Exhub.LspBridge.Handler

  alias Exhub.LspBridge.Handlers.Formatting

  @impl true
  def name, do: "range-format"

  @impl true
  def method, do: "textDocument/rangeFormatting"

  @impl true
  def cancel_on_change?, do: true

  @impl true
  def provider, do: "range_formatting"

  @impl true
  def request_params(%{"range" => range} = args, _ctx) do
    %{"range" => range, "options" => Formatting.options(args)}
  end

  def request_params(args, _ctx), do: %{"options" => Formatting.options(args)}

  @impl true
  def process_response(edits, ctx) when is_list(edits) and edits != [] do
    {:format, ctx.path, edits}
  end

  def process_response(_other, _ctx), do: {:message, "Nothing to format."}
end
