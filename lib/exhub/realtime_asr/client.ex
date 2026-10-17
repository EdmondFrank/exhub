defmodule Exhub.RealtimeASR.Client do
  @moduledoc """
  Client for MoArk's WebSocket real-time ASR endpoint
  (`wss://api.moark.com/v1/audio/transcriptions/ws`).

  MoArk reuses OpenAI's *Realtime* WebSocket subprotocol names (`realtime`,
  `openai-beta.realtime-v1`, `openai-insecure-api-key.<TOKEN>`) but its message
  protocol is its own:

      client → {"type":"start","model":…,"payload":{input,options}}
      server → {"type":"started"}
      client → raw binary PCM frames (s16le / 16 kHz / mono)
      client → {"type":"finish"}
      server → {"type":"output","payload":{"text":…,"is_final":bool}} (repeated)
      server → {"type":"completed"} | {"type":"error","payload":{"message":…}}

  `transcribe/2` drives that exchange synchronously over an HTTP/1 WebSocket
  (Mint + `Mint.WebSocket`, passive mode) and returns the final transcript.

  The connection is opened and closed within the calling process, so callers
  should run it inside a `Task` when the socket could outlive an HTTP request.
  """

  require Logger

  @default_host "api.moark.com"
  @default_path "/v1/audio/transcriptions/ws"
  @default_model "TeleASR-MultiDialect"
  @default_timeout 120_000
  # 3200 bytes = 100 ms of s16le / 16 kHz / mono audio.
  @chunk_bytes 3_200
  # MoArk's real-time ASR rejects bursts with `4007 Non-real-time audio data`,
  # so frames are metered out at the audio's own rate unless `pace: false`.
  @chunk_pace_ms 100

  @type opt ::
          {:model, String.t()}
          | {:token, String.t()}
          | {:options, map()}
          | {:timeout, pos_integer()}
          | {:host, String.t()}
          | {:path, String.t()}
          | {:pace, boolean()}

  @doc "The MoArk model this client is built around."
  @spec default_model() :: String.t()
  def default_model, do: @default_model

  @doc """
  Builds the upstream `wss://…` URL for `model`.
  """
  @spec websocket_url(String.t(), keyword()) :: String.t()
  def websocket_url(model, opts \\ []) do
    host = Keyword.get(opts, :host, @default_host)
    path = Keyword.get(opts, :path, @default_path)
    "wss://#{host}#{path}?" <> URI.encode_query(%{"model" => model})
  end

  @doc """
  The `Sec-WebSocket-Protocol` subprotocols MoArk expects, the access token
  carried in the last one.
  """
  @spec subprotocols(String.t()) :: [String.t()]
  def subprotocols(token) when is_binary(token) do
    ["realtime", "openai-beta.realtime-v1", "openai-insecure-api-key.#{token}"]
  end

  @doc """
  Builds the JSON `start` message. `:options` overrides individual ASR options.
  """
  @spec start_message(String.t(), keyword()) :: map()
  def start_message(model, opts \\ []) do
    %{
      "type" => "start",
      "model" => model,
      "payload" => %{
        "input" => %{
          "type" => "audio",
          "format" => "pcm_s16le",
          "sample_rate" => 16_000,
          "channels" => 1
        },
        "options" => start_options(opts)
      }
    }
  end

  @doc """
  Transcribes `pcm` (raw `s16le`, 16 kHz, mono) and returns `{:ok, text}`.
  """
  @spec transcribe(binary(), [opt()]) :: {:ok, String.t()} | {:error, term()}
  def transcribe(pcm, opts) when is_binary(pcm) do
    model = Keyword.get(opts, :model, @default_model)
    token = Keyword.fetch!(opts, :token)
    timeout = Keyword.get(opts, :timeout, @default_timeout)

    case connect(websocket_url(model, opts), token, timeout) do
      {:ok, state} ->
        try do
          run(state, pcm, model, opts, timeout)
        after
          _ = Mint.HTTP.close(state.conn)
        end

      {:error, _reason} = error ->
        error
    end
  end

  # --- Options ---

  defp start_options(opts) do
    base = %{
      "enable_punctuation" => true,
      "enable_inverse_text_normalization" => true,
      "enable_emendation" => false,
      "enable_words" => false,
      "max_end_silence" => 10
    }

    Map.merge(base, Keyword.get(opts, :options, %{}))
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

  defp run(state, pcm, model, opts, timeout) do
    pace_ms = if Keyword.get(opts, :pace, true), do: @chunk_pace_ms, else: 0

    with {:ok, state} <- send_json(state, start_message(model, opts)),
         {:ok, state} <- await_started(state, timeout),
         {:ok, state} <- send_pcm(state, pcm, pace_ms),
         {:ok, state} <- send_json(state, %{"type" => "finish"}) do
      collect(state, timeout, [], nil)
    end
  end

  defp await_started(state, timeout) do
    case recv_events(state, timeout) do
      {:ok, state, events} ->
        cond do
          Enum.any?(events, &(&1 == :started)) -> {:ok, state}
          Enum.any?(events, &match?({:error, _}, &1)) -> {:error, :start_failed}
          true -> await_started(state, timeout)
        end

      {:error, _reason} = error ->
        error
    end
  end

  defp collect(state, timeout, finals, partial) do
    case recv_events(state, timeout) do
      {:ok, state, events} ->
        handle_events(state, events, timeout, finals, partial)

      {:error, _reason} = error ->
        error
    end
  end

  defp handle_events(state, [], timeout, finals, partial),
    do: collect(state, timeout, finals, partial)

  defp handle_events(state, [event | rest], timeout, finals, partial) do
    case event do
      {:output, text, true} -> handle_events(state, rest, timeout, [text | finals], partial)
      {:output, text, false} -> handle_events(state, rest, timeout, finals, text)
      :completed -> {:ok, transcript(finals, partial)}
      {:error, message} -> {:error, {:server_error, message}}
      _other -> handle_events(state, rest, timeout, finals, partial)
    end
  end

  defp transcript([], partial), do: partial || ""
  defp transcript(finals, _partial), do: finals |> Enum.reverse() |> Enum.join(" ")

  defp send_pcm(state, <<>>, _pace_ms), do: {:ok, state}

  defp send_pcm(state, pcm, pace_ms) do
    {chunk, rest} = split_at(pcm, @chunk_bytes)

    with {:ok, state} <- send_frame(state, {:binary, chunk}) do
      if pace_ms > 0, do: Process.sleep(pace_ms)
      send_pcm(state, rest, pace_ms)
    end
  end

  defp split_at(bin, n) when byte_size(bin) <= n, do: {bin, <<>>}

  defp split_at(bin, n),
    do: {binary_part(bin, 0, n), binary_part(bin, n, byte_size(bin) - n)}

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
  defp classify({:close, _code, _reason}), do: :completed

  defp classify(%{"type" => "started"}), do: :started

  defp classify(%{"type" => "output"} = message) do
    payload = Map.get(message, "payload", %{})
    {:output, Map.get(payload, "text", ""), Map.get(payload, "is_final") == true}
  end

  defp classify(%{"type" => "completed"}), do: :completed

  defp classify(%{"type" => "error"} = message) do
    message = get_in(message, ["payload", "message"]) || "unknown ASR error"
    {:error, message}
  end

  defp classify(_other), do: :ignore
end
