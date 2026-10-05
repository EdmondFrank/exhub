defmodule Exhub.LspBridge.Protocol do
  @moduledoc """
  JSON-RPC 2.0 wire framing for the Language Server Protocol.

  This is the Elixir port of the `Content-Length` header framing that
  lsp-bridge's Python `MessageSender` / `MessageReceiver` threads implement in
  `core/lspserver.py`. Every LSP message on stdio is:

      Content-Length: <byte-length>\r
      \r
      <json payload>

  The module is pure (no process state) so it can be unit-tested with
  `mix test --no-start` and reused by both the encoder (server -> child) and
  the decoder (child -> server, fed a continuous byte stream).

  ## Framing vs. transport

  This module only turns bytes into messages and messages into bytes. Owning
  the OS `Port`, buffering partial reads, and dispatching decoded frames is the
  job of `Exhub.LspBridge.Server`.
  """

  @crlf "\r\n"
  @header_sep @crlf <> @crlf

  @typedoc "A decoded JSON-RPC message (request, response, or notification)."
  @type message :: map()

  @doc """
  Encode one message into a framed iodata chunk ready to write to the child's
  stdin.

  Adds the `"jsonrpc": "2.0"` field when the caller omitted it.

      iex> {:ok, iodata} = Exhub.LspBridge.Protocol.encode(%{"method" => "exit"})
      iex> IO.iodata_to_binary(iodata)
      "Content-Length: 26\\r\\n\\r\\n{\\"jsonrpc\\":\\"2.0\\",\\"method\\":\\"exit\\"}"
  """
  @spec encode(message()) :: {:ok, iodata()} | {:error, term()}
  def encode(%{} = msg) do
    body =
      msg
      |> Map.put_new("jsonrpc", "2.0")
      |> Jason.encode!()

    len = byte_size(body)
    {:ok, ["Content-Length: ", Integer.to_string(len), @header_sep, body]}
  rescue
    e -> {:error, Exception.message(e)}
  end

  @doc """
  Decode a continuous byte stream into `{messages, rest}`.

  `rest` is any trailing incomplete frame to be carried over to the next
  `feed/1`. Handles multiple back-to-back frames and split headers/bodies.

      iex> frame = "Content-Length: 5\\r\\n\\r\\n{\\\"a\\\":1}"
      iex> {[msg], ""} = Exhub.LspBridge.Protocol.decode(frame)
      iex> msg
      %{"a" => 1}
  """
  @spec decode(binary()) :: {[message()], binary()}
  def decode(buffer) when is_binary(buffer), do: do_decode(buffer, [])

  @doc false
  defp do_decode(buffer, acc) do
    case extract_frame(buffer) do
      {:ok, body, rest} ->
        case Jason.decode(body) do
          {:ok, msg} -> do_decode(rest, [msg | acc])
          # Malformed JSON: drop this frame and keep scanning.
          {:error, _} -> do_decode(rest, acc)
        end

      :more ->
        {Enum.reverse(acc), buffer}

      :error ->
        # Unparseable header — resync by skipping one byte and retrying.
        <<_::utf8, rest::binary>> = buffer
        do_decode(rest, acc)
    end
  end

  # Returns {:ok, body, rest} | :more (need more bytes) | :error (bad header).
  defp extract_frame(buffer) do
    case :binary.split(buffer, @header_sep) do
      [_only] ->
        :more

      [headers, rest] ->
        case content_length(headers) do
          {:ok, len} ->
            if byte_size(rest) >= len do
              <<body::binary-size(len), tail::binary>> = rest
              {:ok, body, tail}
            else
              :more
            end

          :error ->
            :error
        end
    end
  end

  defp content_length(headers) do
    headers
    |> String.split("\r\n")
    |> Enum.find_value(:error, fn line ->
      case Regex.run(~r/^Content-Length:\s*(\d+)$/i, line) do
        [_, num] -> {:ok, String.to_integer(num)}
        _ -> nil
      end
    end)
  end
end
