defmodule Exhub.MCP.EncodingTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Encoding

  doctest Exhub.MCP.Encoding

  @replacement "\uFFFD"

  describe "sanitize_utf8/1" do
    test "passes valid UTF-8 binaries through unchanged" do
      assert Encoding.sanitize_utf8("hello") == "hello"
      assert Encoding.sanitize_utf8("café — 中文") == "café — 中文"
      assert Encoding.sanitize_utf8("") == ""
    end

    test "replaces invalid bytes with the U+FFFD replacement character" do
      # <<186>> (0xBA) is a latin1 byte, invalid on its own in UTF-8
      assert Encoding.sanitize_utf8(<<104, 105, 186, 77>>) == "hi" <> @replacement <> "M"

      # 0xFF can never appear in valid UTF-8
      assert Encoding.sanitize_utf8(<<255, 65>>) == @replacement <> "A"

      # Valid prefix is preserved
      assert Encoding.sanitize_utf8(<<"ok", 186>>) == "ok" <> @replacement
    end

    test "salvages ASCII and valid UTF-8 sequences after an invalid byte" do
      # ASCII following an invalid byte is kept
      assert Encoding.sanitize_utf8(<<186, 195, 169, 77>>) == @replacement <> "éM"

      # A whole valid sequence following an invalid byte is kept
      assert Encoding.sanitize_utf8(<<255, "中文", 77>>) == @replacement <> "中文M"

      # Runs of consecutive invalid bytes each become one replacement char
      assert Encoding.sanitize_utf8(<<255, 186, "ok">>) ==
               @replacement <> @replacement <> "ok"
    end

    test "handles long non-UTF-8 tails linearly, preserving ASCII" do
      long = :binary.copy(<<186>>, 1_000) <> "END"

      result = Encoding.sanitize_utf8(long)

      assert String.valid?(result)
      assert String.ends_with?(result, "END")
      assert byte_size(result) == 1_000 * 3 + 3
    end

    test "recurses into maps, sanitizing keys and values" do
      data = %{"stdout" => <<186, 77>>, "nested" => %{"stderr" => <<255>>}}

      assert Encoding.sanitize_utf8(data) == %{
               "stdout" => @replacement <> "M",
               "nested" => %{"stderr" => @replacement}
             }
    end

    test "recurses into lists" do
      data = [%{"name" => <<186>>}, "maybe\xFF", 42, nil]

      assert Encoding.sanitize_utf8(data) == [
               %{"name" => @replacement},
               "maybe" <> @replacement,
               42,
               nil
             ]
    end

    test "passes non-binary terms through unchanged" do
      assert Encoding.sanitize_utf8(nil) == nil
      assert Encoding.sanitize_utf8(42) == 42
      assert Encoding.sanitize_utf8(:atom) == :atom
      assert Encoding.sanitize_utf8(true) == true
    end

    test "leaves charlists (lists of integers) untouched" do
      assert Encoding.sanitize_utf8([77, 186]) == [77, 186]
    end

    test "result is always valid UTF-8" do
      assert String.valid?(Encoding.sanitize_utf8(<<1, 2, 186, 255, 3>>))
      assert String.valid?(Encoding.sanitize_utf8("ütf-8 ok"))
    end
  end
end