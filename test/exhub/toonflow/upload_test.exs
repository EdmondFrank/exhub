defmodule Exhub.Toonflow.UploadTest do
  use ExUnit.Case, async: true

  alias Exhub.Toonflow.Upload

  describe "ext_for/1" do
    test "detects png, jpeg and webp from magic bytes" do
      assert {:ok, "png"} = Upload.ext_for(<<0x89, "PNG\r\n", 0x1A, "\n", 0, 0, 0, 0>>)
      assert {:ok, "jpg"} = Upload.ext_for(<<0xFF, 0xD8, 0xFF, 0xE0, 0, 0, 0, 0>>)
      assert {:ok, "webp"} = Upload.ext_for(<<"RIFF", 0, 0, 0, 0, "WEBPVP8 ", 0>>)
    end

    test "refuses non-images and truncated headers" do
      assert {:error, :unsupported_image} = Upload.ext_for("GIF89a....")
      assert {:error, :unsupported_image} = Upload.ext_for(<<0x89, "PNG">>)
      assert {:error, :unsupported_image} = Upload.ext_for("")
    end
  end
end
