defmodule Exhub.RealtimeASR.Handler do
  @moduledoc """
  `POST /openai/v1/audio/transcriptions` — an OpenAI-compatible transcription
  endpoint backed by MoArk (Gitee AI).

  The generic `/openai/v1/*` reverse proxy can already forward the HTTP multipart
  `/v1/audio/transcriptions` endpoint, but MoArk's `TeleASR-MultiDialect` is only
  reachable over a WebSocket. This handler closes that gap and gives every MoArk
  audio model a single OpenAI-shaped surface:

    * `TeleASR-MultiDialect` — decode the upload to PCM and drive the WebSocket
      via `Exhub.RealtimeASR.Transcriber`.
    * `whisper-large-v3`, `whisper-large-v3-turbo`, `SenseVoiceSmall` — forward
      the multipart body to MoArk's HTTP `/v1/audio/transcriptions`.

  Request fields (multipart/form-data, OpenAI-compatible): `file` (required),
  `model`, `language`, `response_format` (`json` | `text` | `verbose_json`).
  """

  import Plug.Conn

  require Logger

  alias Exhub.RealtimeASR.Transcriber
  alias Exhub.Router.Config, as: RouterConfig

  # MoArk HTTP transcription endpoint (multipart).
  @http_url "https://api.moark.com/v1/audio/transcriptions"
  @request_timeout 120_000
  @multipart_length 200_000_000
  @default_model "whisper-large-v3-turbo"

  # Models only reachable over MoArk's WebSocket.
  @ws_models ~w(TeleASR-MultiDialect)
  # Models served by MoArk's HTTP /v1/audio/transcriptions.
  @http_models ~w(whisper-large-v3 whisper-large-v3-turbo SenseVoiceSmall)
  # OpenAI-style aliases → MoArk model names.
  @model_aliases %{
    "whisper-1" => "whisper-large-v3-turbo",
    "whisper-1-turbo" => "whisper-large-v3-turbo"
  }

  @doc "Supported models, for docs and error messages."
  @spec supported_models() :: [String.t()]
  def supported_models, do: @ws_models ++ @http_models

  @doc "Plug entry point."
  @spec call(Plug.Conn.t()) :: Plug.Conn.t()
  def call(conn) do
    conn = parse_multipart(conn)

    with {:ok, upload_path, params} <- extract(conn),
         {:ok, model} <- resolve_model(params["model"]) do
      dispatch(conn, upload_path, params, model)
    else
      {:error, message} -> send_error(conn, 400, message)
    end
  end

  # --- Request parsing ---

  # Multipart is parsed here rather than in the global parser plug so the
  # `/openai/v1/*` pass-through of other multipart bodies stays untouched.
  defp parse_multipart(conn) do
    opts = Plug.Parsers.init(parsers: [:multipart], pass: ["*/*"], length: @multipart_length)
    Plug.Parsers.call(conn, opts)
  rescue
    error ->
      Logger.warning("[RealtimeASR] multipart parse failed: #{inspect(error)}")
      conn
  end

  defp extract(conn) do
    params = conn.body_params

    case params["file"] do
      %Plug.Upload{path: path} -> {:ok, path, params}
      _ -> {:error, "`file` is required (multipart/form-data 'file' upload)"}
    end
  end

  defp resolve_model(nil), do: {:ok, @default_model}
  defp resolve_model(""), do: {:ok, @default_model}

  defp resolve_model(model) when is_binary(model) do
    resolved = Map.get(@model_aliases, model, model)

    if resolved in supported_models() do
      {:ok, resolved}
    else
      {:error, "Unsupported model: #{model}. Supported: #{Enum.join(supported_models(), ", ")}"}
    end
  end

  # --- Dispatch ---

  defp dispatch(conn, upload_path, params, model) do
    cond do
      model in @ws_models -> transcribe_ws(conn, upload_path, params, model)
      model in @http_models -> forward_http(conn, upload_path, params, model)
    end
  end

  defp transcribe_ws(conn, upload_path, params, model) do
    case RouterConfig.get_model_api_key(model) do
      "" ->
        send_error(conn, 500, "MoArk API key not configured (set :giteeai_api_key)")

      token ->
        case Transcriber.transcribe(upload_path, model, token: token) do
          {:ok, text} ->
            send_transcript(conn, text, params, model)

          {:error, reason} ->
            send_error(conn, 502, "Transcription failed: #{format_reason(reason)}")
        end
    end
  end

  defp forward_http(conn, upload_path, params, model) do
    case File.read(upload_path) do
      {:ok, binary} ->
        {body, content_type} = multipart_body(build_fields(model, binary, params))

        headers = [
          {"Authorization", "Bearer #{RouterConfig.get_model_api_key(model)}"},
          {"X-Failover-Enabled", "true"},
          {"Content-Type", content_type}
        ]

        case HTTPoison.post(
               @http_url,
               body,
               headers,
               [recv_timeout: @request_timeout, timeout: @request_timeout] ++
                 Exhub.TLSCompat.httpoison_opts(@http_url)
             ) do
          {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
            conn
            |> put_resp_content_type("application/json")
            |> send_resp(status, resp_body)

          {:error, %HTTPoison.Error{reason: reason}} ->
            send_error(conn, 502, "Upstream request failed: #{inspect(reason)}")
        end

      {:error, reason} ->
        send_error(conn, 500, "Cannot read upload: #{inspect(reason)}")
    end
  end

  defp build_fields(model, binary, params) do
    [
      {"model", model},
      {"file", {"audio", binary, "application/octet-stream"}}
    ] ++ optional_field("language", params["language"])
  end

  defp optional_field(_name, nil), do: []
  defp optional_field(_name, ""), do: []
  defp optional_field(name, value), do: [{name, value}]

  # --- Response ---

  defp send_transcript(conn, text, params, model) do
    case params["response_format"] do
      "text" ->
        conn
        |> put_resp_content_type("text/plain")
        |> send_resp(200, text)

      "verbose_json" ->
        send_json(conn, 200, %{"task" => "transcribe", "text" => text, "model" => model})

      _json ->
        send_json(conn, 200, %{"text" => text})
    end
  end

  defp send_json(conn, status, payload) do
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status, Jason.encode!(payload))
  end

  defp send_error(conn, status, message) do
    send_json(conn, status, %{
      "error" => %{"message" => message, "type" => "invalid_request_error"}
    })
  end

  defp format_reason({:ffmpeg_not_found}),
    do: "ffmpeg is not installed (required to decode the audio upload)"

  defp format_reason({:ffmpeg_failed, status, stderr}),
    do: "audio decode failed (ffmpeg exit #{status}): #{String.trim(stderr)}"

  defp format_reason({:server_error, message}), do: message
  defp format_reason(reason), do: inspect(reason)

  # --- Multipart encoding (mirrors Exhub.MCP.Tools.Listen) ---

  defp multipart_body(fields) do
    boundary = :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)

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
    {body, "multipart/form-data; boundary=#{boundary}"}
  end
end
