defmodule Exhub.Sagents.Markdown do
  @moduledoc """
  Renders message text as HTML for the Agent Hub transcript.

  The hub sends rows both as server-rendered HTML (first paint) and as raw
  content over SSE (`display_message`), so both paths go through `to_html/1`
  and produce the same markup. Raw HTML in the source is never passed through:
  `unsafe` stays off, so an agent quoting `<script>` cannot inject it.
  """

  @options [
    extension: [table: true, strikethrough: true, autolink: true, tasklist: true],
    render: [unsafe: false, escape: false]
  ]

  @doc """
  Converts Markdown to HTML. Blank input yields an empty string; a parse failure
  degrades to escaped preformatted text rather than dropping the message.
  """
  @spec to_html(term()) :: String.t()
  def to_html(text) when is_binary(text) do
    if String.trim(text) == "" do
      ""
    else
      case MDEx.to_html(text, @options) do
        {:ok, html} -> html
        {:error, reason} -> fallback(text, reason)
      end
    end
  rescue
    error -> fallback(text, error)
  end

  def to_html(nil), do: ""
  def to_html(text), do: text |> to_string() |> to_html()

  defp fallback(text, reason) do
    require Logger
    Logger.warning("Agent Hub markdown render failed: #{inspect(reason)}")
    "<pre>#{escape(text)}</pre>"
  end

  defp escape(text) do
    text
    |> String.replace("&", "&amp;")
    |> String.replace("<", "&lt;")
    |> String.replace(">", "&gt;")
    |> String.replace("\"", "&quot;")
  end
end
