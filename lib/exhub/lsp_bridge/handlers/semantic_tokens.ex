defmodule Exhub.LspBridge.Handlers.SemanticTokens do
  @moduledoc """
  `textDocument/semanticTokens/full` — the Elixir port of the decode half of
  `core/handler/semantic_tokens.py`.

  LSP sends a flat, delta-encoded `data` array of 5-tuples
  (`deltaLine`, `deltaStartCharacter`, `length`, `tokenType`, `tokenModifiers`).
  This handler expands it into absolute tokens and resolves the `tokenType` /
  `tokenModifiers` indices to names via the server's advertised legend (passed
  in the handler context), so the elisp front end only needs a type-name → face
  table. Only the `full` request is implemented (no range/incremental + cache).
  """

  @behaviour Exhub.LspBridge.Handler

  import Bitwise

  @impl true
  def name, do: "semantic-tokens"

  @impl true
  def method, do: "textDocument/semanticTokens/full"

  @impl true
  def cancel_on_change?, do: false

  @impl true
  def provider, do: "semantic_tokens"

  @impl true
  def request_params(_args, _ctx), do: %{}

  @impl true
  def process_response(%{"data" => data}, ctx) when is_list(data) do
    {:semantic_tokens, ctx.path, decode(data, Map.get(ctx, :semantic_tokens_legend))}
  end

  def process_response(_other, ctx), do: {:semantic_tokens, ctx.path, []}

  @doc "Expand LSP delta-encoded `data` into absolute token maps using `legend`."
  @spec decode([integer()], map() | nil) :: [map()]
  def decode(data, legend) do
    types = map_get(legend, "tokenTypes")
    modifiers = map_get(legend, "tokenModifiers")

    {tokens, _line, _character} =
      data
      |> Enum.chunk_every(5)
      |> Enum.reduce({[], 0, 0}, fn
        [delta_line, delta_start, length, type_index, modifier_mask], {acc, line, character} ->
          {line, character} = advance(delta_line, delta_start, line, character)

          token = %{
            "line" => line,
            "character" => character,
            "length" => length,
            "type" => Enum.at(types, type_index),
            "modifiers" => modifier_names(modifier_mask, modifiers)
          }

          {[token | acc], line, character}

        _other, acc ->
          acc
      end)

    Enum.reverse(tokens)
  end

  # A non-zero deltaLine resets the character to the (absolute) deltaStart;
  # otherwise the character is relative to the previous token on the same line.
  defp advance(0, delta_start, line, character), do: {line, character + delta_start}
  defp advance(delta_line, delta_start, line, _character), do: {line + delta_line, delta_start}

  defp modifier_names(mask, names) when is_integer(mask) and mask > 0 do
    names
    |> Enum.with_index()
    |> Enum.filter(fn {_name, index} -> band(mask, bsl(1, index)) != 0 end)
    |> Enum.map(&elem(&1, 0))
  end

  defp modifier_names(_mask, _names), do: []

  defp map_get(nil, _key), do: []
  defp map_get(map, key) when is_map(map), do: List.wrap(Map.get(map, key, []))
end
