defmodule Exhub.LspBridge.ProtocolTest do
  use ExUnit.Case, async: true

  alias Exhub.LspBridge.Protocol

  describe "encode/1" do
    test "adds jsonrpc and frames with content-length" do
      {:ok, iodata} = Protocol.encode(%{"method" => "exit"})
      bin = IO.iodata_to_binary(iodata)

      body = Jason.encode!(%{"jsonrpc" => "2.0", "method" => "exit"})
      assert bin == "Content-Length: #{byte_size(body)}\r\n\r\n#{body}"
    end

    test "preserves an existing jsonrpc field" do
      {:ok, bin} = Protocol.encode(%{"jsonrpc" => "2.0", "id" => 1})
      assert IO.iodata_to_binary(bin) =~ "\"id\":1"
    end

    test "content-length counts bytes, not codepoints" do
      {:ok, iodata} = Protocol.encode(%{"text" => "héllo"})
      bin = IO.iodata_to_binary(iodata)

      body = Jason.encode!(%{"text" => "héllo", "jsonrpc" => "2.0"})
      assert bin == "Content-Length: #{byte_size(body)}\r\n\r\n#{body}"
      # "é" is 2 UTF-8 bytes; the header must reflect that, not the char count.
      assert byte_size(body) > String.length(body)
    end
  end

  describe "decode/1" do
    test "decodes a single complete frame" do
      body = ~s({"a":1})
      frame = "Content-Length: #{byte_size(body)}\r\n\r\n#{body}"
      assert {[msg], ""} = Protocol.decode(frame)
      assert msg == %{"a" => 1}
    end

    test "decodes multiple back-to-back frames" do
      b1 = ~s({"id":1})
      b2 = ~s({"id":2})

      buffer =
        "Content-Length: #{byte_size(b1)}\r\n\r\n#{b1}" <>
          "Content-Length: #{byte_size(b2)}\r\n\r\n#{b2}"

      assert {msgs, ""} = Protocol.decode(buffer)
      assert msgs == [%{"id" => 1}, %{"id" => 2}]
    end

    test "leaves an incomplete trailing frame in rest" do
      body = ~s({"id":1})
      full = "Content-Length: #{byte_size(body)}\r\n\r\n#{body}"
      partial = full <> "Content-Length: 50\r\n\r\n{\"incomple"

      assert {[msg], rest} = Protocol.decode(partial)
      assert msg == %{"id" => 1}
      assert rest == "Content-Length: 50\r\n\r\n{\"incomple"
    end

    test "returns empty when only a partial header is present" do
      assert {[], rest} = Protocol.decode("Content-Len")
      assert rest == "Content-Len"
    end

    test "round-trips encode -> decode" do
      msg = %{"jsonrpc" => "2.0", "id" => 7, "method" => "textDocument/hover"}
      {:ok, iodata} = Protocol.encode(msg)
      assert {[decoded], ""} = Protocol.decode(IO.iodata_to_binary(iodata))
      assert decoded["id"] == 7
      assert decoded["method"] == "textDocument/hover"
    end
  end
end
