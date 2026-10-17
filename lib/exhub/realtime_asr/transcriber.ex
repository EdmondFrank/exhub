defmodule Exhub.RealtimeASR.Transcriber do
  @moduledoc """
  Batch transcription for MoArk's WebSocket-only ASR models
  (e.g. `TeleASR-MultiDialect`).

  MoArk serves non-streaming models at the OpenAI-compatible HTTP
  `/v1/audio/transcriptions` endpoint, but `TeleASR-MultiDialect` is only
  reachable over the WebSocket shown in `Exhub.RealtimeASR.Client`. This module
  bridges the two: the uploaded audio is decoded to raw PCM with ffmpeg, streamed
  over the WebSocket, and the final transcript is returned.
  """

  alias Exhub.RealtimeASR.Client

  @sample_rate 16_000
  @channels 1

  @typedoc "Audio properties of the PCM fed to the ASR WebSocket."
  @type pcm_opts :: [channels: pos_integer(), sample_rate: pos_integer()]

  @doc """
  Transcribes the audio file at `path` with `model`.

  Options: `:token` (required), plus any option accepted by
  `Exhub.RealtimeASR.Client.transcribe/2` (`:options`, `:timeout`, `:host`, `:path`).
  """
  @spec transcribe(Path.t(), String.t(), keyword()) :: {:ok, String.t()} | {:error, term()}
  def transcribe(path, model, opts \\ []) do
    token = Keyword.fetch!(opts, :token)

    with {:ok, pcm} <- to_pcm(path),
         {:ok, text} <- Client.transcribe(pcm, [model: model, token: token] ++ opts) do
      {:ok, text}
    end
  end

  @doc """
  Decodes `path` to raw `s16le` PCM (16 kHz, mono by default) using ffmpeg.

  Returns `{:error, :ffmpeg_not_found}` when ffmpeg is unavailable, or
  `{:error, {:ffmpeg_failed, status, stderr}}` on a decode failure.
  """
  @spec to_pcm(Path.t(), pcm_opts()) :: {:ok, binary()} | {:error, term()}
  def to_pcm(path, opts \\ []) do
    case System.find_executable("ffmpeg") do
      nil ->
        {:error, :ffmpeg_not_found}

      ffmpeg ->
        decode(ffmpeg, path, opts)
    end
  end

  defp decode(ffmpeg, path, opts) do
    out =
      Path.join(
        System.tmp_dir!(),
        "exhub_asr_#{System.unique_integer([:positive, :monotonic])}.pcm"
      )

    args = [
      "-hide_banner",
      "-loglevel",
      "error",
      "-y",
      "-i",
      path,
      "-f",
      "s16le",
      "-acodec",
      "pcm_s16le",
      "-ac",
      Integer.to_string(Keyword.get(opts, :channels, @channels)),
      "-ar",
      Integer.to_string(Keyword.get(opts, :sample_rate, @sample_rate)),
      out
    ]

    try do
      case System.cmd(ffmpeg, args, stderr_to_stdout: true) do
        {_output, 0} -> read_pcm(out)
        {error, status} -> {:error, {:ffmpeg_failed, status, String.slice(error, 0, 500)}}
      end
    after
      File.rm(out)
    end
  end

  defp read_pcm(path) do
    case File.read(path) do
      {:ok, ""} -> {:error, :empty_audio}
      {:ok, pcm} -> {:ok, pcm}
      {:error, reason} -> {:error, {:read_pcm_failed, reason}}
    end
  end
end
