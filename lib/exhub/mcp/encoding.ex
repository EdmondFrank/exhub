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

  # Tails longer than this are assumed to be non-UTF-8 overall (e.g. a whole
  # latin1/GBK file); replacing every non-ASCII byte there is linear and cheap,
  # whereas byte-by-byte re-decoding would be quadratic.
  @salvage_window 256

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

  defp sanitize_string(binary) do
    case :unicode.characters_to_binary(binary, :utf8, :utf8) do
      {:ok, result} -> result
      result when is_binary(result) -> result
      {:error, good, bad} -> good <> salvage(bad)
      {:incomplete, good, rest} -> good <> salvage(rest)
    end
  end

  # `bad`/`rest` starts at the first byte that could not be decoded. Drop that
  # byte and re-decode from the next one so ASCII and valid UTF-8 sequences
  # following it are preserved. Long non-UTF-8 tails fall back to linear
  # per-byte replacement (preserving ASCII) to avoid quadratic re-decoding.
  defp salvage(<<_byte, rest::binary>>) when byte_size(rest) > @salvage_window do
    @replacement <> replace_non_ascii(rest)
  end

  defp salvage(<<_byte, rest::binary>>) do
    @replacement <> sanitize_string(rest)
  end

  defp salvage(<<>>), do: <<>>

  defp replace_non_ascii(<<byte, rest::binary>>) when byte < 0x80 do
    <<byte>> <> replace_non_ascii(rest)
  end

  defp replace_non_ascii(<<_byte, rest::binary>>) do
    @replacement <> replace_non_ascii(rest)
  end

  defp replace_non_ascii(<<>>), do: <<>>
end