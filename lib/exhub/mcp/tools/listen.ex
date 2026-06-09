defmodule Exhub.MCP.Tools.Listen do
  @moduledoc """
  MCP Tool for audio transcription via Gitee AI / moark.com speech-to-text API.

  Accepts local audio file paths and sends them to the OpenAI-compatible
  `/v1/audio/transcriptions` endpoint. Returns the transcribed text.
  """

  alias Anubis.Server.Response

  use Anubis.Server.Component, type: :tool

  @api_url "https://ai.gitee.com/v1/audio/transcriptions"
  @default_model "whisper-large-v3-turbo"
  @request_timeout 120_000

  @supported_audio_exts ~w(.mp3 .wav .m4a .flac .ogg .opus .webm .mp4 .mpeg .mpga .oga .aac)

  @valid_models ~w(
    whisper-large-v3-turbo
    whisper-large-v3
  )

  def name, do: "listen"

  @impl true
  def description do
    """
    Transcribe audio to text using AI speech recognition via Gitee AI / moark.com.

    Accepts local audio file paths. Uses OpenAI-compatible `/v1/audio/transcriptions` endpoint.

    **Supported models:**
    - `whisper-large-v3-turbo` (default) — OpenAI Whisper 多语种实时识别
    - `whisper-large-v3` — Whisper 标准版

    **Supported audio formats:** MP3, WAV, M4A, FLAC, OGG, OPUS, WebM, MP4, AAC

    Returns the transcribed text.
    """
  end

  schema do
    field(:file, {:required, :string},
      description: "Absolute path to the audio file (e.g. /path/to/audio.mp3) or ~ shorthand"
    )

    field(:model, :string,
      description: "Speech recognition model. Default: whisper-large-v3-turbo"
    )

    field(:language, :string,
      description: "Language hint (e.g. 'zh', 'en'). Optional."
    )
  end

  @impl true
  def execute(params, frame) do
    file = Map.get(params, :file)
    model = (Map.get(params, :model, @default_model) || @default_model) |> normalize_model()
    language = Map.get(params, :language)

    cond do
      is_nil(file) or file == "" ->
        resp = Response.tool() |> Response.error("`file` is required — provide a local path")
        {:reply, resp, frame}

      model not in @valid_models ->
        resp = Response.tool() |> Response.error("Invalid model: #{model}. Valid models: #{Enum.join(@valid_models, ", ")}")
        {:reply, resp, frame}

      true ->
        api_key = Application.get_env(:exhub, :giteeai_api_key, "")

        if api_key == "" do
          resp = Response.tool() |> Response.error("Gitee AI API key not configured. Run: mix scr.insert dev giteeai_api_key \"your-key\"")
          {:reply, resp, frame}
        else
          do_transcribe(file, model, language, api_key, frame)
        end
    end
  end

  # --- Private helpers ---

  defp normalize_model(model) do
    model
    |> String.trim()
    |> String.downcase()
    |> case do
      "whisper-large-v3-turbo" -> "whisper-large-v3-turbo"
      "whisper-large-v3" -> "whisper-large-v3"
      other -> other
    end
  end

  defp do_transcribe(file, model, language, api_key, frame) do
    with {:ok, expanded_path} <- validate_and_expand_path(file),
         {:ok, binary} <- read_audio_file(expanded_path) do
      filename = Path.basename(expanded_path)
      mime = ext_to_mime(Path.extname(expanded_path))
      boundary = generate_boundary()

      fields =
        [
          {"model", model},
          {"file", {filename, binary, mime}}
        ]
        |> maybe_add_language(language)

      {body, content_type} = build_multipart(fields, boundary)

      headers = [
        {"Authorization", "Bearer #{api_key}"},
        {"X-Failover-Enabled", "true"},
        {"Content-Type", content_type}
      ]

      case HTTPoison.post(@api_url, body, headers, recv_timeout: @request_timeout, timeout: @request_timeout) do
        {:ok, %HTTPoison.Response{status_code: 200, body: resp_body}} ->
          handle_success(resp_body, frame)

        {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
          resp = Response.tool() |> Response.error("Transcription API error (HTTP #{status}): #{resp_body}")
          {:reply, resp, frame}

        {:error, %HTTPoison.Error{reason: reason}} ->
          resp = Response.tool() |> Response.error("HTTP request failed: #{inspect(reason)}")
          {:reply, resp, frame}
      end
    else
      {:error, reason} ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp validate_and_expand_path(file) do
    cond do
      String.starts_with?(file, ["/", "~"]) ->
        case Exhub.MCP.Desktop.Helpers.validate_absolute_path(file) do
          {:ok, expanded_path} ->
            if File.exists?(expanded_path) do
              ext = Path.extname(expanded_path) |> String.downcase()

              if ext in @supported_audio_exts do
                {:ok, expanded_path}
              else
                {:error, "Unsupported audio format: #{ext}. Supported: #{Enum.join(@supported_audio_exts, ", ")}"}
              end
            else
              {:error, "File not found: #{expanded_path}"}
            end

          {:error, reason} ->
            {:error, reason}
        end

      true ->
        {:error, "Relative paths are not supported: '#{file}'. Use an absolute path (e.g. /path/to/audio.mp3) or ~ shorthand (e.g. ~/path/to/audio.mp3)."}
    end
  end

  defp read_audio_file(path) do
    case File.read(path) do
      {:ok, binary} -> {:ok, binary}
      {:error, reason} -> {:error, "Cannot read file #{path}: #{inspect(reason)}"}
    end
  end

  defp maybe_add_language(fields, nil), do: fields
  defp maybe_add_language(fields, ""), do: fields

  defp maybe_add_language(fields, language) do
    fields ++ [{"language", language}]
  end

  defp generate_boundary do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end

  defp build_multipart(fields, boundary) do
    parts =
      Enum.map(fields, fn
        {name, {filename, content, content_type}} ->
          [
            "--#{boundary}\r\n",
            "Content-Disposition: form-data; name=\"#{name}\"; filename=\"#{filename}\"\r\n",
            "Content-Type: #{content_type}\r\n",
            "\r\n",
            content,
            "\r\n"
          ]

        {name, value} ->
          [
            "--#{boundary}\r\n",
            "Content-Disposition: form-data; name=\"#{name}\"\r\n",
            "\r\n",
            value,
            "\r\n"
          ]
      end)

    body = IO.iodata_to_binary([parts, "--#{boundary}--\r\n"])
    content_type = "multipart/form-data; boundary=#{boundary}"
    {body, content_type}
  end

  defp ext_to_mime(".mp3"), do: "audio/mpeg"
  defp ext_to_mime(".wav"), do: "audio/wav"
  defp ext_to_mime(".m4a"), do: "audio/mp4"
  defp ext_to_mime(".flac"), do: "audio/flac"
  defp ext_to_mime(".ogg"), do: "audio/ogg"
  defp ext_to_mime(".opus"), do: "audio/opus"
  defp ext_to_mime(".webm"), do: "audio/webm"
  defp ext_to_mime(".mp4"), do: "video/mp4"
  defp ext_to_mime(".mpeg"), do: "audio/mpeg"
  defp ext_to_mime(".mpga"), do: "audio/mpeg"
  defp ext_to_mime(".oga"), do: "audio/ogg"
  defp ext_to_mime(".aac"), do: "audio/aac"
  defp ext_to_mime(_), do: "application/octet-stream"

  defp handle_success(resp_body, frame) do
    case Jason.decode(resp_body) do
      {:ok, %{"text" => text}} ->
        resp = Response.tool() |> Response.text(text)
        {:reply, resp, frame}

      {:ok, decoded} ->
        resp = Response.tool() |> Response.error("Unexpected API response: #{inspect(decoded)}")
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to decode API response: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end
end
