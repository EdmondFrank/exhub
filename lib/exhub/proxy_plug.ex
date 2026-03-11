defmodule Exhub.ProxyPlug do
  @moduledoc """
  Reverse proxy plug for forwarding requests to upstream servers.
  Handles request/response transformation and error handling.
  """

  require Logger

  @doc """
  Forward connection to upstream server with proper error handling.
  """
  def forward_upstream(conn, upstream, opts \\ []) do
    params =
      ReverseProxyPlug.init(
        upstream: upstream,
        response_mode: Keyword.get(opts, :response_mode, :stream),
        client_options: Keyword.get(opts, :client_options, []),
        preserve_host_header: Keyword.get(opts, :preserve_host_header, false)
      )

    custom_headers = Keyword.get(opts, :custom_headers, [])

    conn =
      Enum.reduce(custom_headers, conn, fn {header, value}, conn ->
        header = String.downcase(header)
        conn = Plug.Conn.delete_req_header(conn, header)
        Plug.Conn.put_req_header(conn, header, value)
      end)

    {pre_body, conn} =
      case Map.get(conn, :req_body) do
        nil -> ReverseProxyPlug.read_body(conn)
        body -> {body, conn}
      end

    body = build_request_body(conn, pre_body)

    model_name = extract_model_name(conn)
    provider = extract_provider(conn.request_path)

    # Store request body for token tracking
    request_body_for_tracking = body

    conn
    |> Map.put(:path_info, conn.path_params["path"])
    |> ReverseProxyPlug.request(body, params)
    |> case do
      {:ok, resp} when is_function(resp) or is_struct(resp, Stream) or is_list(resp) ->
        wrapped_resp = {:ok, wrap_stream(resp, model_name, provider, request_body_for_tracking)}
        log_response(wrapped_resp, conn)
        ReverseProxyPlug.response(wrapped_resp, conn, params)

      {:ok, %{body: _body} = resp} ->
        wrapped_resp = {:ok, resp}
        log_response(wrapped_resp, conn)
        track_token_usage(wrapped_resp, model_name, provider, request_body_for_tracking)
        ReverseProxyPlug.response(wrapped_resp, conn, params)

      {:error, _error} = error_resp ->
        log_response(error_resp, conn)
        ReverseProxyPlug.response(error_resp, conn, params)
    end
  end

  defp wrap_stream(stream, model_name, provider, request_body) do
    acc_ref = :erlang.unique_integer([:positive, :monotonic])
    acc_ref_bin = Integer.to_string(acc_ref)

    acc_pid = spawn(fn -> stream_accumulator(acc_ref_bin, [], request_body) end)

    wrapped_stream =
      stream
      |> Stream.transform(
        fn -> {acc_pid, acc_ref_bin, []} end,
        fn element, {pid, ref, acc} ->
          case element do
            {:chunk, chunk} ->
              send(pid, {:accumulate, ref, chunk})
              {[element], {pid, ref, [chunk | acc]}}

            other ->
              {[other], {pid, ref, acc}}
          end
        end,
        fn {pid, ref, _chunks} ->
          send(pid, {:process, ref, model_name, provider})
          :ok
        end
      )

    wrapped_stream
  end

  defp stream_accumulator(ref, chunks, request_body) do
    receive do
      {:accumulate, ^ref, chunk} ->
        stream_accumulator(ref, [chunk | chunks], request_body)

      {:process, ^ref, model_name, provider} ->
        response_body = chunks |> Enum.reverse() |> Enum.join()

        if response_body != "" or request_body != "" do
          spawn(fn ->
            try do
              Exhub.TokenUsage.Tracker.track_openai_usage(
                response_body,
                model_name,
                provider,
                request_body
              )
            rescue
              _ ->
                try do
                  Exhub.TokenUsage.Tracker.track_estimate_only(
                    model_name,
                    provider,
                    response_body,
                    request_body
                  )
                rescue
                  _ -> :ok
                end
            end
          end)
        end

        :ok

      _ ->
        stream_accumulator(ref, chunks, request_body)
    after
      300_000 ->
        :ok
    end
  end

  defp build_request_body(conn, pre_body) do
    cond do
      pre_body == "" && Plug.Conn.get_req_header(conn, "content-type") == ["application/json"] ->
        encode_json_body(conn)

      pre_body == "" &&
          Plug.Conn.get_req_header(conn, "content-type") == ["application/x-www-form-urlencoded"] ->
        Plug.Conn.Query.encode(conn.body_params)

      true ->
        pre_body
    end
  end

  defp encode_json_body(conn) do
    if List.first(conn.path_info) == "cohere" do
      Jason.encode!(Map.delete(conn.body_params, "n"))
    else
      encode_body_with_model_transforms(conn.body_params)
    end
  end

  defp encode_body_with_model_transforms(body_params) do
    case body_params do
      %{"model" => "deepseek-v3.2"} ->
        Jason.encode!(
          body_params
          |> Map.put("think", false)
          |> Map.put("max_tokens", 8192)
          |> Map.put("thinking_budget", 4096)
        )

      %{"model" => "minimax-m2.1", "messages" => messages} ->
        transformed_messages =
          Enum.map(messages, fn message ->
            if message["role"] == "system" do
              Map.put(message, "role", "user")
            else
              message
            end
          end)

        Jason.encode!(Map.put(body_params, "messages", transformed_messages))

      %{"model" => "kimi-k2.5"} ->
        body_params
        |> Map.put("temperature", 1)
        |> Jason.encode!()

      _ ->
        Jason.encode!(body_params)
    end
  end

  defp extract_model_name(conn) do
    case conn.body_params do
      %{"model" => model} when is_binary(model) -> model
      _ -> "unknown"
    end
  end

  defp extract_provider(path) do
    cond do
      String.contains?(path, "openai") -> "openai"
      String.contains?(path, "anthropic") -> "anthropic"
      String.contains?(path, "groq") -> "groq"
      String.contains?(path, "google") -> "google"
      String.contains?(path, "cohere") -> "cohere"
      String.contains?(path, "samba") -> "samba"
      true -> "unknown"
    end
  end

  defp log_response(resp, conn) do
    case resp do
      {:ok, %{status: status, body: resp_body}} when status < 200 or status >= 300 ->
        Logger.warning(
          "Upstream request failed: status=#{status}, path=#{conn.request_path}, body=#{inspect(resp_body)}"
        )

      {:ok, %{status: status}} when status < 200 or status >= 300 ->
        Logger.warning("Upstream request failed: status=#{status}, path=#{conn.request_path}")

      {:error, reason} ->
        Logger.error(
          "Upstream request error: path=#{conn.request_path}, reason=#{inspect(reason)}"
        )

      _ ->
        :ok
    end
  end

  defp track_token_usage({:ok, %{body: resp_body}}, model_name, provider, req_body)
       when is_binary(resp_body) and resp_body != "" do
    spawn(fn ->
      try do
        Exhub.TokenUsage.Tracker.track_openai_usage(resp_body, model_name, provider, req_body)
      rescue
        _ -> :ok
      end
    end)
  end

  defp track_token_usage(_, _, _, _), do: :ok
end
