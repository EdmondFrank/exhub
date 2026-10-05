defmodule Exhub.MCP.Encoding do
  @moduledoc """
  Sanitization helpers for MCP response payloads.

  External data — shell output, raw file contents, process output — may contain
  byte sequences that are not valid UTF-8 (e.g. latin1 bytes). The JSON encoders
  used when serializing MCP responses (`:elixir_json` in Elixir >= 1.18, Jason,
  Toon) raise on such input, crashing the whole request with `{:invalid_byte, N}`.

  `sanitize_utf8/1` recursively rewrites invalid byte sequences to U+FFFD (the
  Unicode replacement character) so that responses always encode successfully.
  Valid UTF-8 binaries pass through untouched.
  """

  @replacement <<0xEF, 0xBF, 0xBD>>

  # How many un-decodable bytes `salvage/2` drops one at a time, re-decoding the
  # tail after each. A stray byte is usually isolated (e.g. a slice cut in the
  # middle of a multibyte codepoint), so the valid UTF-8 that follows it is
  # kept. A whole latin1/GBK payload would need one repair per byte, so past
  # this budget the tail goes through the linear `replace_non_ascii/1` instead.
  @salvage_repairs 32

  @doc """
  Recursively ensures every string in `data` is valid UTF-8.

  - Binaries: returned unchanged when already valid (fast path); otherwise
    invalid sequences are replaced with U+FFFD, preserving ASCII and any valid
    UTF-8 sequences that follow the invalid bytes.
  - Maps: keys and values are recursed into.
  - Lists: elements are recursed into (charlists pass through unchanged).
  - All other terms: returned as-is.

  ## Examples

      iex> Exhub.MCP.Encoding.sanitize_utf8("hello")
      "hello"

      iex> Exhub.MCP.Encoding.sanitize_utf8(<<104, 105, 186, 77>>)
      "hi\uFFFD" <> "M"
  """
  @spec sanitize_utf8(term()) :: term()
  def sanitize_utf8(data) when is_binary(data) do
    if String.valid?(data) do
      data
    else
      sanitize_string(data)
    end
  end

  def sanitize_utf8(data) when is_map(data) do
    Map.new(data, fn {key, value} -> {sanitize_utf8(key), sanitize_utf8(value)} end)
  end

  def sanitize_utf8(data) when is_list(data) do
    Enum.map(data, &sanitize_utf8/1)
  end

  def sanitize_utf8(data), do: data

  defp sanitize_string(binary), do: sanitize_string(binary, @salvage_repairs)

  defp sanitize_string(binary, repairs) do
    case :unicode.characters_to_binary(binary, :utf8, :utf8) do
      {:ok, result} -> result
      result when is_binary(result) -> result
      {:error, good, bad} -> good <> salvage(bad, repairs)
      {:incomplete, good, rest} -> good <> salvage(rest, repairs)
    end
  end

  # `bad`/`rest` starts at the first byte that could not be decoded. Drop that
  # byte and re-decode from the next one so ASCII *and* valid multibyte
  # sequences following it are preserved — a single stray byte must not discard
  # the rest of an otherwise well-formed payload. Re-decoding is bounded by
  # `repairs`; once that budget runs out the tail is treated as non-UTF-8
  # throughout and salvaged byte by byte, which stays linear.
  defp salvage(<<>>, _repairs), do: <<>>

  defp salvage(<<_byte, rest::binary>>, repairs) when repairs > 0 do
    @replacement <> sanitize_string(rest, repairs - 1)
  end

  defp salvage(<<_byte, rest::binary>>, _repairs) do
    @replacement <> replace_non_ascii(rest)
  end

  # Accumulates an iolist rather than folding `<>` on the right, which would
  # copy the whole remaining tail at every byte.
  defp replace_non_ascii(binary), do: binary |> replace_non_ascii([]) |> IO.iodata_to_binary()

  defp replace_non_ascii(<<byte, rest::binary>>, acc) when byte < 0x80 do
    replace_non_ascii(rest, [<<byte>> | acc])
  end

  defp replace_non_ascii(<<_byte, rest::binary>>, acc) do
    replace_non_ascii(rest, [@replacement | acc])
  end

  defp replace_non_ascii(<<>>, acc), do: Enum.reverse(acc)
end
