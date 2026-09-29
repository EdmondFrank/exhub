defmodule Exhub.MCP.ImageSourceTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.ImageSource

  # Minimal 1x1 transparent PNG.
  @png <<137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 1, 0, 0, 0, 1, 8,
         6, 0, 0, 0, 31, 21, 196, 137, 0, 0, 0, 10, 73, 68, 65, 84, 120, 156, 99, 0, 1, 0, 0, 5,
         0, 1, 13, 10, 45, 180, 0, 0, 0, 0, 73, 69, 78, 68, 174, 66, 96, 130>>

  describe "classify/1" do
    test "classifies every accepted source kind" do
      assert ImageSource.classify("https://example.com/a.png") == :url
      assert ImageSource.classify("http://example.com/a.png") == :url
      assert ImageSource.classify("data:image/png;base64,AAAA") == :data_uri
      assert ImageSource.classify("/tmp/a.png") == :path
      assert ImageSource.classify("~/a.png") == :path
      assert ImageSource.classify("a.png") == :relative
      assert ImageSource.classify("") == :empty
      assert ImageSource.classify(nil) == :invalid
      assert ImageSource.classify(42) == :invalid
    end
  end

  describe "supported_ext?/1 and mime_type/1" do
    test "accepts the documented extensions" do
      for ext <- ~w(.png .jpg .jpeg .gif .webp .bmp) do
        assert ImageSource.supported_ext?(ext)
      end

      assert ImageSource.supported_ext?(".PNG")
      refute ImageSource.supported_ext?(".tiff")
      refute ImageSource.supported_ext?(nil)
    end

    test "maps extensions to MIME types" do
      assert ImageSource.mime_type(".png") == "image/png"
      assert ImageSource.mime_type(".JPG") == "image/jpeg"
      assert ImageSource.mime_type(".jpeg") == "image/jpeg"
      assert ImageSource.mime_type(".webp") == "image/webp"
      assert ImageSource.mime_type(".unknown") == "application/octet-stream"
    end

    test "exposes the supported extension list" do
      assert ".png" in ImageSource.supported_extensions()
    end
  end

  describe "data_uri/2" do
    test "prefixes base64 bytes with the MIME type" do
      uri = ImageSource.data_uri("image/png", "abc")

      assert uri == "data:image/png;base64,YWJj"
      assert ImageSource.data_uri?(uri)
      refute ImageSource.data_uri?("https://example.com/a.png")
    end
  end

  describe "resolve/2" do
    test "passes URLs and data URIs through unchanged" do
      assert ImageSource.resolve("https://example.com/a.png") ==
               {:ok, "https://example.com/a.png"}

      assert ImageSource.resolve("data:image/png;base64,AAAA") ==
               {:ok, "data:image/png;base64,AAAA"}
    end

    test "encodes a local file as a data URI" do
      path = write_tmp!(@png, ".png")
      on_exit(fn -> File.rm(path) end)

      assert {:ok, uri} = ImageSource.resolve(path)
      assert uri == "data:image/png;base64," <> Base.encode64(@png)
    end

    test "expands ~ shorthand" do
      missing = "/definitely-missing-#{System.unique_integer([:positive])}.png"

      assert {:error, message} = ImageSource.resolve("~" <> missing)
      assert message =~ "File not found"
      assert message =~ Path.join(System.user_home!(), missing)
    end

    test "rejects relative paths with guidance" do
      assert {:error, message} = ImageSource.resolve("images/a.png")
      assert message =~ "Relative paths are not supported"
      assert message =~ "data URI"
    end

    test "rejects an unsupported extension" do
      path = write_tmp!(@png, ".tiff")
      on_exit(fn -> File.rm(path) end)

      assert {:error, message} = ImageSource.resolve(path)
      assert message =~ "Unsupported image format: .tiff"
    end

    test "reports a missing file" do
      path = "/tmp/exhub-missing-#{System.unique_integer([:positive])}.png"

      assert {:error, message} = ImageSource.resolve(path)
      assert message =~ "File not found"
    end

    test "reports a directory as not found" do
      assert {:error, message} = ImageSource.resolve(System.tmp_dir!())
      assert message =~ "File not found"
    end

    test "reports empty and invalid sources" do
      assert {:error, message} = ImageSource.resolve("")
      assert message =~ "empty"

      assert {:error, message} = ImageSource.resolve(nil)
      assert message =~ "Invalid image source"

      assert {:error, message} = ImageSource.resolve(42)
      assert message =~ "Invalid image source"
    end
  end

  describe "resolve/2 downscaling" do
    test "encodes small files without downscaling" do
      path = write_tmp!(@png, ".png")
      on_exit(fn -> File.rm(path) end)

      assert {:ok, uri} = ImageSource.resolve(path)
      assert String.starts_with?(uri, "data:image/png;base64,")
    end

    test "downscales an oversized file when a downscaler is available" do
      path = write_tmp!(large_bmp(), ".bmp")
      on_exit(fn -> File.rm(path) end)

      original_size = File.stat!(path).size
      assert original_size > 2_000_000

      assert {:ok, uri} = ImageSource.resolve(path)

      if System.find_executable("sips") do
        assert String.starts_with?(uri, "data:image/jpeg;base64,")
        assert byte_size(uri) < original_size
      else
        assert String.starts_with?(uri, "data:image/bmp;base64,")
      end
    end

    test "downscaling can be disabled with :infinity" do
      path = write_tmp!(large_bmp(), ".bmp")
      on_exit(fn -> File.rm(path) end)

      assert {:ok, uri} = ImageSource.resolve(path, downscale_threshold: :infinity)
      assert uri == "data:image/bmp;base64," <> Base.encode64(File.read!(path))
    end
  end

  # A 1000x1000 24-bit BMP filled with random bytes: valid input for the
  # downscaler, and comfortably above the 2 MB threshold.
  defp large_bmp(width \\ 1000, height \\ 1000) do
    pixels = :crypto.strong_rand_bytes(width * 3 * height)
    file_size = 54 + byte_size(pixels)

    <<"BM", file_size::little-32, 0::little-32, 54::little-32, 40::little-32, width::little-32,
      height::little-32, 1::little-16, 24::little-16, 0::little-32, byte_size(pixels)::little-32,
      2835::little-32, 2835::little-32, 0::little-32, 0::little-32, pixels::binary>>
  end

  defp write_tmp!(bytes, ext) do
    path =
      Path.join(
        System.tmp_dir!(),
        "exhub_image_source_test_#{System.unique_integer([:positive])}#{ext}"
      )

    File.write!(path, bytes)
    path
  end
end
