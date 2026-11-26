defmodule Exhub.Toonflow.Upload do
  @moduledoc """
  Validation for reference images uploaded from the canvas' cast tab.

  The browser posts the file as the raw request body, so its `content-type`
  header is only a hint — a mislabelled PNG stored as `.jpg` would later be
  served (and sent to the image model) with the wrong MIME type. The extension
  is therefore derived from the file's magic bytes instead of the header, and
  anything that is not a real PNG / JPEG / WebP is refused before it reaches
  the workspace.

  `ext_for/1` is pure and unit-tested.
  """

  @max_bytes 20_000_000

  @doc "Largest accepted upload, in bytes."
  @spec max_bytes() :: pos_integer()
  def max_bytes, do: @max_bytes

  @doc """
  Derive the file extension from an image's magic bytes. Pure.

  Returns `{:ok, "png" | "jpg" | "webp"}` or `{:error, :unsupported_image}`.
  """
  @spec ext_for(binary()) :: {:ok, String.t()} | {:error, :unsupported_image}
  def ext_for(<<0x89, "PNG\r\n", 0x1A, "\n", _rest::binary>>), do: {:ok, "png"}
  def ext_for(<<0xFF, 0xD8, 0xFF, _rest::binary>>), do: {:ok, "jpg"}
  def ext_for(<<"RIFF", _size::binary-size(4), "WEBP", _rest::binary>>), do: {:ok, "webp"}
  def ext_for(_bytes), do: {:error, :unsupported_image}
end
