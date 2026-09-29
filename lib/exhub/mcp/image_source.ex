defmodule Exhub.MCP.ImageSource do
  @moduledoc """
  Resolves an image reference into a value a remote model API can fetch.

  MoArk / Gitee AI endpoints (video `first_frame` / `last_frame`, vision
  messages, …) take images as URLs. Local files are therefore read and encoded
  as `data:<mime>;base64,…` URIs, which the platform accepts; oversized files
  are downscaled first so the request body stays small.

  Accepted sources:

    * `https://…` / `http://…` — passed through unchanged
    * `data:image/…;base64,…` — passed through unchanged
    * an absolute path or `~/…` shorthand — read and encoded as a data URI

  Path handling mirrors `Exhub.MCP.Desktop.Helpers.validate_absolute_path/1`,
  the same validator the `look` and desktop tools use.
  """

  alias Exhub.MCP.Desktop.Helpers

  # Local files above this size are downscaled before being encoded.
  @downscale_threshold_bytes 2_000_000
  @downscale_max_dimension 1280
  @downscale_quality 85

  @supported_exts ~w(.png .jpg .jpeg .gif .webp .bmp)

  @type source :: String.t()
  @type kind :: :url | :data_uri | :path | :empty | :relative | :invalid
  @type result :: {:ok, String.t()} | {:error, String.t()}

  @doc """
  File extensions accepted for local paths.
  """
  @spec supported_extensions() :: [String.t()]
  def supported_extensions, do: @supported_exts

  @doc """
  Classifies a source string.

  Returns `:url`, `:data_uri`, `:path` (absolute or `~` shorthand), `:empty`,
  `:relative` or `:invalid`.
  """
  @spec classify(term()) :: kind()
  def classify(source) when is_binary(source) do
    cond do
      source == "" -> :empty
      String.starts_with?(source, ["http://", "https://"]) -> :url
      String.starts_with?(source, "data:") -> :data_uri
      String.starts_with?(source, ["/", "~"]) -> :path
      true -> :relative
    end
  end

  def classify(_source), do: :invalid

  @doc """
  True when `source` is a base64 `data:` URI.
  """
  @spec data_uri?(term()) :: boolean()
  def data_uri?(source) when is_binary(source), do: String.starts_with?(source, "data:")
  def data_uri?(_source), do: false

  @doc """
  True when `ext` (e.g. `".png"`) is an accepted image extension.
  """
  @spec supported_ext?(term()) :: boolean()
  def supported_ext?(ext) when is_binary(ext), do: String.downcase(ext) in @supported_exts
  def supported_ext?(_ext), do: false

  @doc """
  Maps a file extension to its MIME type.
  """
  @spec mime_type(term()) :: String.t()
  def mime_type(ext) when is_binary(ext) do
    case String.downcase(ext) do
      ".png" -> "image/png"
      ".jpg" -> "image/jpeg"
      ".jpeg" -> "image/jpeg"
      ".gif" -> "image/gif"
      ".webp" -> "image/webp"
      ".bmp" -> "image/bmp"
      _ -> "application/octet-stream"
    end
  end

  def mime_type(_ext), do: "application/octet-stream"

  @doc """
  Builds a base64 `data:` URI for `bytes`.
  """
  @spec data_uri(String.t(), binary()) :: String.t()
  def data_uri(mime, bytes) when is_binary(mime) and is_binary(bytes) do
    "data:" <> mime <> ";base64," <> Base.encode64(bytes)
  end

  @doc """
  Resolves `source` into a string the remote API can fetch.

  ## Options

    * `:downscale_threshold` — byte size above which local files are downscaled
      (default `#{@downscale_threshold_bytes}`); pass `:infinity` to never downscale
    * `:max_dimension` — longest edge used when downscaling
      (default `#{@downscale_max_dimension}`)
  """
  @spec resolve(source(), keyword()) :: result()
  def resolve(source, opts \\ [])

  def resolve(source, opts) when is_binary(source) do
    case classify(source) do
      :url -> {:ok, source}
      :data_uri -> {:ok, source}
      :path -> encode_local_file(source, opts)
      :empty -> {:error, "Image source is empty."}
      :relative -> {:error, relative_path_message(source)}
      :invalid -> {:error, invalid_source_message(source)}
    end
  end

  def resolve(source, _opts), do: {:error, invalid_source_message(source)}

  @doc """
  Message returned for a relative path.
  """
  @spec relative_path_message(String.t()) :: String.t()
  def relative_path_message(path) do
    "Relative paths are not supported: '#{path}'. Use an absolute path " <>
      "(e.g. /path/to/image.png), ~ shorthand (e.g. ~/path/to/image.png), " <>
      "a URL (https://...), or a data URI (data:image/png;base64,...)."
  end

  defp invalid_source_message(source) do
    "Invalid image source: #{inspect(source)}. Expected a URL, a data URI, or a file path."
  end

  # ---------------------------------------------------------------------------
  # Local files
  # ---------------------------------------------------------------------------

  defp encode_local_file(source, opts) do
    with {:ok, path} <- Helpers.validate_absolute_path(source),
         :ok <- ensure_regular_file(path),
         {:ok, ext} <- ensure_supported_ext(path),
         {:ok, bytes} <- read_bytes(path) do
      {:ok, encode_bytes(path, bytes, mime_type(ext), opts)}
    end
  end

  defp ensure_regular_file(path) do
    if File.regular?(path) do
      :ok
    else
      {:error, "File not found: #{path}"}
    end
  end

  defp ensure_supported_ext(path) do
    ext = Path.extname(path)

    if supported_ext?(ext) do
      {:ok, ext}
    else
      {:error, "Unsupported image format: #{ext}. Supported: #{Enum.join(@supported_exts, ", ")}"}
    end
  end

  defp read_bytes(path) do
    case File.read(path) do
      {:ok, bytes} -> {:ok, bytes}
      {:error, reason} -> {:error, "Cannot read file #{path}: #{inspect(reason)}"}
    end
  end

  defp encode_bytes(path, bytes, mime, opts) do
    threshold = Keyword.get(opts, :downscale_threshold, @downscale_threshold_bytes)
    max_dimension = Keyword.get(opts, :max_dimension, @downscale_max_dimension)

    if downsizable?(bytes, threshold) do
      case downscale(path, byte_size(bytes), max_dimension) do
        {:ok, scaled} -> data_uri("image/jpeg", scaled)
        :error -> data_uri(mime, bytes)
      end
    else
      data_uri(mime, bytes)
    end
  end

  defp downsizable?(_bytes, :infinity), do: false
  defp downsizable?(bytes, threshold) when is_integer(threshold), do: byte_size(bytes) > threshold
  defp downsizable?(_bytes, _threshold), do: false

  # Best-effort downscale through macOS `sips`. Falls back to the original bytes
  # when the binary is unavailable (non-macOS) or does not produce a smaller
  # file, so a failure here never breaks the request.
  defp downscale(path, original_size, max_dimension) do
    case System.find_executable("sips") do
      nil ->
        :error

      sips ->
        out = tmp_jpeg_path()

        args = [
          "-Z",
          Integer.to_string(max_dimension),
          "-s",
          "format",
          "jpeg",
          "-s",
          "formatOptions",
          Integer.to_string(@downscale_quality),
          path,
          "--out",
          out
        ]

        try do
          case System.cmd(sips, args, stderr_to_stdout: true) do
            {_output, 0} ->
              case File.read(out) do
                {:ok, scaled} when byte_size(scaled) < original_size -> {:ok, scaled}
                _ -> :error
              end

            _ ->
              :error
          end
        rescue
          _ -> :error
        after
          File.rm(out)
        end
    end
  end

  defp tmp_jpeg_path do
    Path.join(System.tmp_dir!(), "exhub_image_source_#{System.unique_integer([:positive])}.jpg")
  end
end
