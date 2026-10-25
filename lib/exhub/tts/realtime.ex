defmodule Exhub.TTS.Realtime do
  @moduledoc """
  Streaming text-to-speech client for MoArk's WebSocket speech endpoint
  (`wss://api.moark.com/v1/audio/speech/ws`).

  MoArk reuses OpenAI's *Realtime* WebSocket subprotocol names (`realtime`,
  `openai-beta.realtime-v1`, `openai-insecure-api-key.<TOKEN>`) but its message
  protocol is its own:

      client → {"type":"start","model":…,"payload":{input,output,options}}
      server → {"type":"started"}
      server → {"type":"output","payload":{"audio":<base64 PCM>,"is_final":bool}} (repeated)
      server → {"type":"completed"} | {"type":"error","payload":{code,message}}

  The audio is raw `s16le` PCM (24 kHz mono by default), not a container, so
  `synthesize/2` wraps it in a RIFF/WAVE header (`wav/1`) and returns the same
  shape as `Exhub.TTS.Sync.synthesize/2` — callers can treat both backends
  uniformly.

  The API key is read from `:exhub, :giteeai_api_key` (or `:token` in the
  options). The connection is opened and closed within the calling process, so
  callers should run it inside a `Task` when the socket could outlive an HTTP
  request (the MCP `speak` tool already does).

  ## Concurrency

  MoArk's TeleTTS backend serves **one session at a time**: a second concurrent
  connection answers `TELEAI_WS_CLOSED` (protocol error 1002) instead of audio
  (verified 2026-10-25 — 1 of 3 parallel calls succeeded, the other 2 failed;
  6 sequential calls all succeeded, including across models). `synthesize/2`
  therefore serialises sessions behind a `:global` lock, so parallel callers
  (a ToonFlow voice batch, several agents hitting `speak`) queue rather than
  fail.

  ## Models

  Verified against the live endpoint (2026-10-25):

    * `TeleTTS-Bidirectional-Mandarin` (default) — honours every preset
      voice, and synthesizes voiceless too
    * `TeleTTS-MultiDialect` — honours every preset voice, but answered
      `TELEAI_WS_CLOSED` (protocol error 1002) throughout the verification
      window; switch back with `media.tts_model` once it recovers
    * `TeleTTS-Mandarin` — rejects the `voice` option (`Parameter error`);
      use it voiceless

  ## Voices

  `TeleTTS-MultiDialect` / `TeleTTS-Bidirectional-Mandarin` accept the preset
  catalogue in `voices/0` (`guangzhounv`, `shanghainv`, `chengdunv`,
  `zhengzhounv`, `dongbeinv`, `changshanv`, `xiannv`, `xiamennv`, `joyce`,
  `lucas`) plus an unvoiced default; `TeleTTS-Mandarin` accepts none
  (`voiceless_model?/1`). An unknown voice comes back as a
  `{"code":"10301","message":"Parameter error"}` frame — there is no local
  voice list to check against, so validation is best-effort.
  """

  require Logger

  @host "api.moark.com"
  @path "/v1/audio/speech/ws"

  @models ~w(TeleTTS-MultiDialect TeleTTS-Bidirectional-Mandarin TeleTTS-Mandarin)
  @default_model "TeleTTS-Bidirectional-Mandarin"

  @voices ~w(guangzhounv shanghainv chengdunv zhengzhounv dongbeinv changshanv xiannv xiamennv joyce lucas)
  @default_voice "guangzhounv"

  # These models reject the `voice` option entirely.
  @voiceless_models ~w(TeleTTS-Mandarin)

  @default_sample_rate 24_000
  @channels 1
  @bits_per_sample 16

  @default_volume 50
  @default_speech_rate 1

  @default_timeout 120_000

  @type opt ::
          {:model, String.t()}
          | {:voice, String.t() | nil}
          | {:volume, number()}
          | {:speech_rate, number()}
          | {:sample_rate, pos_integer()}
          | {:token, String.t()}
          | {:timeout, pos_integer()}
          | {:host, String.t()}
          | {:path, String.t()}

  @doc "The WebSocket speech endpoint (without the `?model=` query)."
  @spec url() :: String.t()
  def url, do: "wss://#{@host}#{@path}"

  @doc "The default model (`TeleTTS-Bidirectional-Mandarin`)."
  @spec default_model() :: String.t()
  def default_model, do: @default_model

  @doc "The default voice (`guangzhounv`)."
  @spec default_voice() :: String.t()
  def default_voice, do: @default_voice

  @doc "The default output sample rate (24 000 Hz)."
  @spec default_sample_rate() :: pos_integer()
  def default_sample_rate, do: @default_sample_rate

  @doc "The default volume (50)."
  @spec default_volume() :: number()
  def default_volume, do: @default_volume

  @doc "The default speech rate (1)."
  @spec default_speech_rate() :: number()
  def default_speech_rate, do: @default_speech_rate

  @doc "Models served by the WebSocket endpoint."
  @spec valid_models() :: [String.t()]
  def valid_models, do: @models

  @doc "Whether `model` is a supported WebSocket model."
  @spec model?(String.t() | nil) :: boolean()
  def model?(model), do: model in @models

  @doc "The preset voice catalogue (for the voice-accepting models)."
  @spec voices() :: [String.t()]
  def voices, do: @voices

  @doc "Whether `voice` is one of the preset `voices/0`."
  @spec voice?(String.t() | nil) :: boolean()
  def voice?(voice), do: voice in @voices

  @doc "Whether `model` rejects the `voice` option."
  @spec voiceless_model?(String.t() | nil) :: boolean()
  def voiceless_model?(model), do: model in @voiceless_models

  @doc """
  Builds the upstream `wss://…?model=…` URL for `model` (pure).
  """
  @spec websocket_url(String.t(), keyword()) :: String.t()
  def websocket_url(model, opts \\ []) do
    host = Keyword.get(opts, :host, @host)
    path = Keyword.get(opts, :path, @path)
    "wss://#{host}#{path}?" <> URI.encode_query(%{"model" => model})
  end

  @doc """
  The `Sec-WebSocket-Protocol` subprotocols MoArk expects, the access token
  carried in the last one (pure).
  """
  @spec subprotocols(String.t()) :: [String.t()]
  def subprotocols(token) when is_binary(token) do
    ["realtime", "openai-beta.realtime-v1", "openai-insecure-api-key.#{token}"]
  end

  @doc """
  Builds the JSON `start` message (pure).

  `opts`: `:model`, `:voice`, `:volume`, `:speech_rate`, `:sample_rate`. The
  `voice` option is omitted for a `voiceless_model?/1` model and defaults to
  `default_voice/0` otherwise; blank options are omitted.
  """
  @spec build_start_message(String.t(), keyword()) :: map()
  def build_start_message(text, opts \\ []) do
    model = Keyword.get(opts, :model) || @default_model
    sample_rate = Keyword.get(opts, :sample_rate) || @default_sample_rate

    %{
      "type" => "start",
      "model" => model,
      "payload" => %{
        "input" => %{"type" => "text", "text" => text},
        "output" => %{"type" => "audio", "format" => "PCM", "sample_rate" => sample_rate},
        "options" => start_options(model, opts)
      }
    }
  end

  @doc """
  Wraps raw `s16le` PCM in a RIFF/WAVE header (pure).
  """
  @spec wav(binary(), pos_integer(), pos_integer()) :: binary()
  def wav(pcm, sample_rate \\ @default_sample_rate, channels \\ @channels)
      when is_binary(pcm) do
    byte_rate = sample_rate * channels * div(@bits_per_sample, 8)
    block_align = channels * div(@bits_per_sample, 8)
    data_size = byte_size(pcm)

    "RIFF" <>
      <<data_size + 36::little-32>> <>
      "WAVE" <>
      "fmt " <>
      <<16::little-32, 1::little-16, channels::little-16, sample_rate::little-32,
        byte_rate::little-32, block_align::little-16,
        @bits_per_sample::little-16>> <>
      "data" <> <<data_size::little-32>> <> pcm
  end

  @doc """
  Synthesizes `text` and returns the audio as a WAV.

  Returns `{:ok, %{body: binary, format: "wav", model: binary, voice: binary | nil,
  sample_rate: pos_integer, channels: pos_integer, bytes: non_neg_integer}}` or
  `{:error, reason}` (`{:server_error, message}` for a server `error` frame).

  `opts`: `:token`, `:model`, `:voice`, `:volume`, `:speech_rate`,
  `:sample_rate`, `:timeout`, `:host`, `:path`.
  """
  @spec synthesize(String.t(), [opt()]) :: {:ok, map()} | {:error, term()}
  def synthesize(text, opts \\ []) when is_binary(text) do
    # The TeleTTS backend serves one session at a time (see the moduledoc), so
    # hold a global lock for the whole exchange.
    case :global.trans({{__MODULE__, :session}, self()}, fn -> do_synthesize(text, opts) end, [
           node()
         ]) do
      {:aborted, reason} -> {:error, {:lock_failed, reason}}
      result -> result
    end
  end

  defp do_synthesize(text, opts) do
    model = Keyword.get(opts, :model) || @default_model
    timeout = Keyword.get(opts, :timeout) || @default_timeout
    sample_rate = Keyword.get(opts, :sample_rate) || @default_sample_rate

    with {:ok, token} <- api_key(opts),
         {:ok, state} <- connect(websocket_url(model, opts), token, timeout) do
      try do
        run(state, text, model, opts, timeout, sample_rate)
      after
        _ = Mint.HTTP.close(state.conn)
      end
    end
  end

  # --- Options ---

  defp start_options(model, opts) do
    %{}
    |> maybe_put("voice", voice_option(model, Keyword.get(opts, :voice)))
    |> maybe_put("volume", Keyword.get(opts, :volume))
    |> maybe_put("speech_rate", Keyword.get(opts, :speech_rate))
  end

  defp voice_option(model, _voice) when model in @voiceless_models, do: nil
  defp voice_option(_model, voice), do: blank(voice) || @default_voice

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp blank(value) when is_binary(value) do
    case String.trim(value) do
      "" -> nil
      trimmed -> trimmed
    end
  end

  defp blank(_), do: nil

  defp api_key(opts) do
    case Keyword.get(opts, :token) || Application.get_env(:exhub, :giteeai_api_key, "") do
      key when is_binary(key) and key != "" -> {:ok, key}
      _ -> {:error, :missing_api_key}
    end
  end

  # --- Connection ---

  defp connect(url, token, timeout) do
    uri = URI.parse(url)

    with :ok <- validate_scheme(uri),
         {:ok, conn} <- do_connect(uri),
         {:ok, conn, ref} <-
           Mint.WebSocket.upgrade(
             scheme(uri),
             conn,
             request_path(uri),
             subprotocol_headers(token)
           ),
         {:ok, conn, status, headers} <- recv_upgrade(conn, ref, timeout),
         {:ok, conn, websocket} <- Mint.WebSocket.new(conn, ref, status, headers, mode: :passive) do
      {:ok, %{conn: conn, websocket: websocket, ref: ref}}
    else
      {:error, reason} -> {:error, reason}
      {:error, _conn, error} -> {:error, {:upgrade_failed, error}}
      {:error, _conn, error, _responses} -> {:error, {:recv_failed, error}}
    end
  end

  defp validate_scheme(%URI{scheme: scheme}) when scheme in ["ws", "wss"], do: :ok
  defp validate_scheme(%URI{scheme: scheme}), do: {:error, {:invalid_scheme, scheme}}

  defp scheme(%URI{scheme: "wss"}), do: :wss
  defp scheme(%URI{scheme: "ws"}), do: :ws

  defp do_connect(%URI{host: host, port: port, scheme: scheme}) do
    transport_opts =
      case scheme do
        "wss" -> [verify_fun: Exhub.TLSCompat.verify_fun()]
        "ws" -> []
      end

    Mint.HTTP.connect(transport(scheme), host, port,
      transport_opts: transport_opts,
      mode: :passive
    )
  end

  defp transport("wss"), do: :https
  defp transport("ws"), do: :http

  defp request_path(%URI{path: path, query: query}) do
    path = path || "/"
    if query, do: path <> "?" <> query, else: path
  end

  defp subprotocol_headers(token) do
    [{"sec-websocket-protocol", Enum.join(subprotocols(token), ", ")}]
  end

  # Passive-mode handshake read: pull responses until the `101` is seen.
  defp recv_upgrade(conn, ref, timeout) do
    case Mint.HTTP.recv(conn, 0, timeout) do
      {:ok, conn, responses} ->
        with {:ok, status} <- fetch(responses, :status, ref),
             {:ok, headers} <- fetch(responses, :headers, ref) do
          if status == 101 do
            {:ok, conn, status, headers}
          else
            {:error, {:unexpected_status, status}}
          end
        end

      {:error, _conn, error, _responses} ->
        {:error, {:recv_failed, error}}
    end
  end

  defp fetch(responses, key, ref) do
    Enum.find_value(responses, {:error, {:missing, key}}, fn
      {^key, ^ref, value} -> {:ok, value}
      _ -> nil
    end)
  end

  # --- Protocol ---

  defp run(state, text, model, opts, timeout, sample_rate) do
    with {:ok, state} <- send_json(state, build_start_message(text, opts)) do
      case collect(state, timeout, <<>>) do
        {:ok, _state, pcm} ->
          voice = if voiceless_model?(model), do: nil, else: voice_option(model, opts[:voice])

          {:ok,
           %{
             body: wav(pcm, sample_rate, @channels),
             format: "wav",
             model: model,
             voice: voice,
             sample_rate: sample_rate,
             channels: @channels,
             bytes: byte_size(pcm)
           }}

        {:error, _reason} = error ->
          error
      end
    end
  end

  defp collect(state, timeout, pcm) do
    case recv_events(state, timeout) do
      {:ok, state, events} -> handle_events(state, events, timeout, pcm)
      {:error, _reason} = error -> error
    end
  end

  defp handle_events(state, [], timeout, pcm), do: collect(state, timeout, pcm)

  defp handle_events(state, [event | rest], timeout, pcm) do
    case event do
      {:audio, chunk} -> handle_events(state, rest, timeout, pcm <> chunk)
      :completed -> {:ok, state, pcm}
      {:error, message} -> {:error, {:server_error, message}}
      :started -> handle_events(state, rest, timeout, pcm)
      _other -> handle_events(state, rest, timeout, pcm)
    end
  end

  defp send_json(state, message), do: send_frame(state, {:text, Jason.encode!(message)})

  defp send_frame(%{conn: conn, websocket: websocket, ref: ref} = state, frame) do
    case Mint.WebSocket.encode(websocket, frame) do
      {:ok, websocket, data} ->
        case Mint.WebSocket.stream_request_body(conn, ref, data) do
          {:ok, conn} -> {:ok, %{state | conn: conn, websocket: websocket}}
          {:error, _conn, error} -> {:error, {:send_failed, error}}
        end

      {:error, _websocket, error} ->
        {:error, {:encode_failed, error}}
    end
  end

  defp recv_events(%{conn: conn, websocket: websocket} = state, timeout) do
    case Mint.WebSocket.recv(conn, 0, timeout) do
      {:ok, conn, responses} ->
        {websocket, events} = decode_responses(websocket, responses)
        {:ok, %{state | conn: conn, websocket: websocket}, events}

      {:error, _conn, error, _responses} ->
        {:error, {:recv_failed, error}}
    end
  end

  defp decode_responses(websocket, responses) do
    Enum.reduce(responses, {websocket, []}, fn
      {:data, _ref, data}, {websocket, acc} ->
        case Mint.WebSocket.decode(websocket, data) do
          {:ok, websocket, frames} -> {websocket, acc ++ Enum.map(frames, &classify/1)}
          {:error, websocket, _reason} -> {websocket, acc}
        end

      _other, acc ->
        acc
    end)
  end

  defp classify({:text, json}) do
    case Jason.decode(json) do
      {:ok, message} -> classify(message)
      {:error, _reason} -> :ignore
    end
  end

  defp classify({:binary, _bytes}), do: :ignore
  defp classify({:ping, _data}), do: :ignore
  defp classify({:pong, _data}), do: :ignore
  # A clean close after `completed` is a normal end of stream.
  defp classify({:close, _code, _reason}), do: :completed

  defp classify(%{"type" => "started"}), do: :started

  defp classify(%{"type" => "output"} = message) do
    audio = get_in(message, ["payload", "audio"]) || ""
    {:audio, decode_audio(audio)}
  end

  defp classify(%{"type" => "completed"}), do: :completed

  defp classify(%{"type" => "error"} = message) do
    payload = Map.get(message, "payload", %{})
    code = Map.get(payload, "code")
    text = Map.get(payload, "message") || "unknown TTS error"
    {:error, if(code, do: "#{text} (#{code})", else: text)}
  end

  defp classify(_other), do: :ignore

  defp decode_audio(audio) when is_binary(audio) do
    case Base.decode64(audio) do
      {:ok, pcm} -> pcm
      :error -> <<>>
    end
  end

  defp decode_audio(_audio), do: <<>>
end
