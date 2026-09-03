defmodule Exhub.ResponseHandlers.ExhubFim do
  @moduledoc """
  WebSocket response handler for exhub-fim commands from Emacs.

  Dispatches `["func", ["exhub-fim", action, ...args]]` messages to
  `Exhub.Fim.Server`, which runs the LLM completion requests asynchronously and
  pushes results back to Emacs over the WebSocket.

  ## Supported actions

  - `"complete"` — run a batch of FIM completions for a provider
    (`request_id`, `provider`, `context`, `opts`)
  - `"cancel"` — cancel in-flight completions for a `request_id`
  """

  alias Exhub.Fim.Server

  @doc false
  def call(["exhub-fim", "complete", request_id, provider, context, opts]) do
    context = if is_map(context), do: context, else: %{}
    opts = if is_map(opts), do: opts, else: %{}
    Server.complete(normalize_request_id(request_id), to_string(provider), context, opts)
    nil
  end

  def call(["exhub-fim", "cancel", request_id]) do
    Server.cancel(normalize_request_id(request_id))
    nil
  end

  def call(["exhub-fim" | rest]) do
    require Logger
    Logger.warning("Unknown exhub-fim action: #{inspect(rest)}")
    nil
  end

  defp normalize_request_id(id) when is_integer(id), do: id
  defp normalize_request_id(id) when is_binary(id) do
    case Integer.parse(id) do
      {int, ""} -> int
      _ -> id
    end
  end
  defp normalize_request_id(id), do: id
end