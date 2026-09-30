defmodule Exhub.TTS.Sync do
  @moduledoc """
  Synchronous text-to-speech client for the Gitee AI "模力方舟" serverless API.

  Unlike the MoArk async speech API (submit → poll a task), this posts to the
  OpenAI-compatible `POST /v1/audio/speech` endpoint, which streams the audio
  bytes straight back in the response body.

  The API key is read from `:exhub, :giteeai_api_key` (or `:api_key` in the
  options).

  ## Models

  Verified against the live endpoint (2026-09):

    * `CosyVoice2` (default) — Chinese/English, returns WAV bytes
    * `ChatTTS` — returns WAV bytes
    * `Step-Audio-TTS-3B` — returns MP3 bytes
    * `IndexTTS-2`, `GLM-TTS` — voice-cloning only: they require a
      `prompt_audio_url` reference clip

  `CosyVoice3` and `Qwen3-TTS` are **not** served by this endpoint (it answers
  `暂不支持该接口`); use the async path in `Exhub.MCP.Tools.Speak` for those.

  The response `content-type` header is unreliable — `CosyVoice2` advertises
  `audio/mp3` while emitting a RIFF/WAVE stream — so the real container is
  detected from the payload's magic bytes (`detect_format/1`) and callers
  should name saved files accordingly (`with_format/2`).
  """

  alias Exhub.TLSCompat

  @url "https://ai.gitee.com/v1/audio/speech"
  @default_model "CosyVoice2"
  @default_voice "alloy"

  @valid_models ~w(CosyVoice2 ChatTTS Step-Audio-TTS-3B IndexTTS-2 GLM-TTS TeleTTS-Mandarin)

  # These models cannot synthesize from text alone: they need a reference clip.
  @clone_models ~w(IndexTTS-2 GLM-TTS)

  # Sync requests return the whole clip in one response; allow a generous
  # timeout since long text produces long audio.
  @timeout_ms 180_000

  @doc "The OpenAI-compatible speech endpoint."
  @spec url() :: String.t()
  def url, do: @url

  @doc "The default model (`CosyVoice2`)."
  @spec default_model() :: String.t()
  def default_model, do: @default_model

  @doc "The default voice (`alloy`)."
  @spec default_voice() :: String.t()
  def default_voice, do: @default_voice

  @doc "Models accepted by the sync endpoint."
  @spec valid_models() :: [String.t()]
  def valid_models, do: @valid_models

  @doc "Models that require a `prompt_audio_url` reference clip."
  @spec clone_models() :: [String.t()]
  def clone_models, do: @clone_models

  @doc "Whether `model` is a supported sync model."
  @spec model?(String.t() | nil) :: boolean()
  def model?(model), do: model in @valid_models

  @doc "Whether `model` is a clone-only (reference-audio) model."
  @spec clone_model?(String.t() | nil) :: boolean()
  def clone_model?(model), do: model in @clone_models

  @doc """
  Builds the JSON request body (pure).

  `opts`: `:model`, `:voice`, `:prompt_audio_url`, `:prompt_text`. The clone
  fields are omitted when blank.
  """
  @spec build_body(String.t(), keyword()) :: map()
  def build_body(text, opts \\ []) do
    %{
      "model" => Keyword.get(opts, :model) || @default_model,
      "input" => text,
      "voice" => Keyword.get(opts, :voice) || @default_voice
    }
    |> maybe_put("prompt_audio_url", blank(Keyword.get(opts, :prompt_audio_url)))
    |> maybe_put("prompt_text", blank(Keyword.get(opts, :prompt_text)))
  end

  @doc """
  Detects an audio container from its magic bytes (pure).

  Returns `"wav"`, `"mp3"`, `"ogg"`, `"flac"`, or `nil` when unrecognized.
  """
  @spec detect_format(binary()) :: String.t() | nil
  def detect_format(<<"RIFF", _size::binary-size(4), "WAVE", _::binary>>), do: "wav"
  def detect_format(<<"RIFF", _::binary>>), do: "wav"
  def detect_format(<<"OggS", _::binary>>), do: "ogg"
  def detect_format(<<"fLaC", _::binary>>), do: "flac"
  def detect_format(<<"ID3", _::binary>>), do: "mp3"
  def detect_format(<<0xFF, second, _::binary>>) when second >= 0xE0, do: "mp3"
  def detect_format(_), do: nil

  @doc """
  Returns `path` with its extension set to `format` (pure).

  Leaves `path` untouched when the extension already matches or `format` is
  unknown (`nil`/`""`/`"bin"`).
  """
  @spec with_format(String.t(), String.t() | nil) :: String.t()
  def with_format(path, format) when is_binary(path) do
    current = path |> Path.extname() |> String.trim_leading(".") |> String.downcase()

    if format in [nil, "", "bin"] or current == String.downcase(format) do
      path
    else
      Path.rootname(path) <> "." <> format
    end
  end

  @doc """
  Synthesize `text` and return the raw audio.

  Returns `{:ok, %{body: binary, format: binary, content_type: binary | nil,
  model: binary, voice: binary}}` or `{:error, reason}`.

  `opts`: `:api_key`, `:model`, `:voice`, `:prompt_audio_url`, `:prompt_text`,
  `:timeout`.
  """
  @spec synthesize(String.t(), keyword()) :: {:ok, map()} | {:error, term()}
  def synthesize(text, opts \\ []) do
    body = build_body(text, opts)

    with {:ok, key} <- api_key(opts) do
      request =
        [
          json: body,
          headers: [{"Authorization", "Bearer #{key}"}],
          receive_timeout: Keyword.get(opts, :timeout) || @timeout_ms,
          decode_body: false
        ] ++ TLSCompat.req_opts()

      case Req.post(@url, request) do
        {:ok, %Req.Response{status: status, body: bin, headers: headers}}
        when status in 200..299 ->
          content_type = content_type(headers)

          {:ok,
           %{
             body: bin,
             format: format(bin, content_type),
             content_type: content_type,
             model: body["model"],
             voice: body["voice"]
           }}

        {:ok, %Req.Response{status: status, body: bin}} ->
          {:error, {:http, status, error_message(bin)}}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  # --- helpers ---

  defp api_key(opts) do
    case Keyword.get(opts, :api_key) || Application.get_env(:exhub, :giteeai_api_key, "") do
      key when is_binary(key) and key != "" -> {:ok, key}
      _ -> {:error, :missing_api_key}
    end
  end

  defp format(bin, content_type),
    do: detect_format(bin) || from_content_type(content_type) || "bin"

  defp from_content_type(content_type) when is_binary(content_type) do
    mime =
      content_type
      |> String.downcase()
      |> String.split(";")
      |> hd()
      |> String.trim()

    cond do
      mime in ["audio/mpeg", "audio/mp3", "audio/mpeg3"] -> "mp3"
      mime in ["audio/wav", "audio/x-wav", "audio/wave"] -> "wav"
      mime in ["audio/ogg", "application/ogg"] -> "ogg"
      mime == "audio/flac" -> "flac"
      true -> nil
    end
  end

  defp from_content_type(_), do: nil

  defp content_type(headers) when is_map(headers) do
    Enum.find_value(headers, fn {k, v} ->
      if String.downcase(to_string(k)) == "content-type", do: v
    end)
  end

  defp content_type(_), do: nil

  defp error_message(bin) when is_binary(bin) do
    case Jason.decode(bin) do
      {:ok, %{"error" => %{"message" => msg}}} when is_binary(msg) -> msg
      {:ok, %{"error" => msg}} when is_binary(msg) -> msg
      {:ok, %{"message" => msg}} when is_binary(msg) -> msg
      _ -> String.slice(bin, 0, 500)
    end
  end

  defp error_message(other), do: inspect(other)

  defp blank(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      trimmed -> trimmed
    end
  end

  defp blank(_), do: nil

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)
end
