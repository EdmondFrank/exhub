defmodule Exhub.LspBridge.Handlers.Hover do
  @moduledoc """
  `textDocument/hover` — port of `core/handler/hover.py`.

  Normalises the various `Hover.contents` shapes (string, `MarkupContent`,
  `MarkedString`, or an array of either) into a single markdown string the
  elisp front end renders.
  """

  @behaviour Exhub.LspBridge.Handler

  @impl true
  def name, do: "hover"

  @impl true
  def method, do: "textDocument/hover"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "hover"

  @impl true
  def request_params(%{"position" => position}, _ctx), do: %{"position" => position}
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(nil, _ctx), do: {:message, "No documentation available."}

  def process_response(%{"contents" => contents}, ctx) do
    if empty_contents?(contents) do
      {:message, "No documentation available."}
    else
      case render(contents, ctx.language_id) do
        "" -> {:message, "No documentation available."}
        markdown -> {:hover, ctx.path, markdown}
      end
    end
  end

  def process_response(_other, _ctx), do: {:message, "No documentation available."}

  defp empty_contents?(contents), do: contents in [nil, "", []]

  # -- contents normalisation (parse_hover_contents) -------------------------

  defp render(contents, language_id) do
    contents |> collect(language_id, []) |> Enum.join("\n")
  end

  # A bare string: already-markdown stays, anything else becomes a text block.
  defp collect(str, _language_id, acc) when is_binary(str) do
    item = if String.starts_with?(str, "```"), do: str, else: code_block("text", str)
    acc ++ [item]
  end

  # MarkupContent ({kind, value}) or MarkedString ({language, value}).
  defp collect(%{} = map, language_id, acc) do
    cond do
      Map.has_key?(map, "kind") ->
        kind = map["kind"]
        value = Map.get(map, "value", "")

        item =
          if kind in ["markdown", "plaintext"], do: value, else: code_block(language_id, value)

        acc ++ [item]

      Map.has_key?(map, "language") ->
        acc ++ [code_block(map["language"], Map.get(map, "value", ""))]

      true ->
        acc
    end
  end

  # An array of strings/objects; a `language` carried by an object applies to
  # the bare strings that follow it (the java special case in the original).
  defp collect(list, language_id, acc) when is_list(list) do
    {acc, _language} =
      Enum.reduce(list, {acc, ""}, fn item, {acc, language} ->
        cond do
          is_map(item) ->
            language = Map.get(item, "language", language)
            {collect(item, language_id, acc), language}

          is_binary(item) and item != "" ->
            if language == "java" do
              {acc ++ [item], language}
            else
              {collect(item, language_id, acc), language}
            end

          true ->
            {acc, language}
        end
      end)

    acc
  end

  defp collect(_other, _language_id, acc), do: acc

  defp code_block(language, string), do: "```#{language}\n#{string}\n```"
end
