defmodule Exhub.Lua.Value do
  @moduledoc """
  Lua ↔ Elixir value conversion and rendering for the Lua sandbox.

  A decoded Lua table is a list of `{key, value}` pairs (see `Lua.VM.Value`):
  array-shaped tables (keys `1..n`) become lists, everything else a
  string-keyed map — the same heuristic Lua uses to distinguish arrays from
  objects.

  Rendering helpers (`render_value/1`, `stringify/1`) sanitize UTF-8 before
  encoding. A Lua `string.sub` — or any byte-offset slice — can end inside a
  multibyte codepoint, and a single invalid byte makes Jason reject the whole
  tree; repairing first keeps the JSON path and leaves the result valid UTF-8.
  """

  alias Exhub.MCP.Encoding

  @doc """
  Converts the first sandbox argument to MCP arguments.

  MCP tools take a JSON object; use the first argument when it is one. A table
  with positional keys (`1..n`) decodes to a list — reject it instead of
  silently dropping it to `%{}`, since MCP arguments must be named objects.
  """
  @spec to_args(list()) :: {:ok, map()} | {:error, String.t()}
  def to_args([]), do: {:ok, %{}}

  def to_args([first | _rest]) do
    case lua_to_elixir(first) do
      map when is_map(map) -> {:ok, map}
      list when is_list(list) -> {:error, "arguments must be a table with named keys"}
      _ -> {:ok, %{}}
    end
  end

  @spec lua_to_elixir(term()) :: term()
  def lua_to_elixir(list) when is_list(list) do
    cond do
      list == [] -> %{}
      array_like?(list) -> Enum.map(list, fn {_key, value} -> lua_to_elixir(value) end)
      true -> Map.new(list, fn {key, value} -> {key_to_string(key), lua_to_elixir(value)} end)
    end
  end

  def lua_to_elixir(map) when is_map(map) do
    Map.new(map, fn {key, value} -> {key_to_string(key), lua_to_elixir(value)} end)
  end

  def lua_to_elixir(other), do: other

  defp array_like?(list) do
    list
    |> Enum.with_index(1)
    |> Enum.all?(fn {{key, _value}, index} -> key == index end)
  end

  defp key_to_string(key) when is_binary(key), do: key
  defp key_to_string(key), do: to_string(key)

  @doc """
  Formats a decoded result value for the caller: a bare string is returned
  as-is, `nil` becomes `"nil"`, anything else is JSON-encoded **compactly**
  (single line — no indentation or spaces after `:`/`,`, so the response is
  cheaper in tokens) with an `inspect/2` fallback for terms JSON cannot hold.
  """
  @spec render_value(term()) :: String.t()
  def render_value(value) do
    # Sanitize before encoding. A Lua `string.sub` — or any byte-offset slice —
    # can end inside a multibyte codepoint, and a single invalid byte makes Jason
    # reject the whole tree. Repairing first keeps the JSON path (one U+FFFD per
    # dangling byte) and leaves the result valid UTF-8 for truncation and spill.
    case lua_to_elixir(value) |> Encoding.sanitize_utf8() do
      value when is_binary(value) ->
        value

      nil ->
        "nil"

      value ->
        case Jason.encode(value) do
          {:ok, json} -> json
          {:error, reason} -> inspect_unencodable(value, reason)
        end
    end
  end

  @doc """
  Renders several return values as a JSON array.

  A Lua `return a, b, …` yields **several** values, so `render_value/1` cannot be
  used: the list is a list of independent values, not a single Lua table's
  `{key, value}` pair-list, and decoding it as one crashed in `array_like?/1`.
  Each value is decoded on its own and the batch is JSON-encoded compactly, so
  every returned value survives.
  """
  @spec render_values([term()]) :: String.t()
  def render_values(values) do
    decoded = values |> Enum.map(&lua_to_elixir/1) |> Encoding.sanitize_utf8()

    case Jason.encode(decoded) do
      {:ok, json} -> json
      {:error, reason} -> inspect_unencodable(decoded, reason)
    end
  end

  @doc "Renders one `print` argument: text stays text, anything else is inspected."
  @spec stringify(term()) :: String.t()
  def stringify(value) do
    case lua_to_elixir(value) |> Encoding.sanitize_utf8() do
      value when is_binary(value) -> value
      value -> inspect(value, limit: :infinity)
    end
  end

  @doc "JSON-encodes a raw Elixir term, falling back to `inspect/2`."
  @spec encode_fallback(term()) :: String.t()
  def encode_fallback(result) do
    result = Encoding.sanitize_utf8(result)

    case Jason.encode(result) do
      {:ok, json} -> json
      {:error, reason} -> inspect_unencodable(result, reason)
    end
  end

  @doc """
  Last resort for terms JSON cannot hold at all.

  `value` is already sanitized, so `inspect/2` sees valid UTF-8 and prints text
  as text instead of a numeric byte dump; the encoder's complaint is appended so
  the fallback is diagnosable rather than a silent change of format.
  """
  @spec inspect_unencodable(term(), term()) :: String.t()
  def inspect_unencodable(value, reason) do
    inspect(value, limit: :infinity) <>
      "\n(not JSON-encodable: #{inspect(reason, limit: :infinity)})"
  end
end
