defmodule Exhub.ProxyPlug do
  @moduledoc """
  Reverse proxy plug for forwarding requests to upstream servers.
  Handles request/response transformation and error handling.
  """

  require Logger

  # Models that require reasoning_content to be preserved across turns.
  @kimi_reasoning_models ["kimi-k2.5", "kimi-k2.6", "inf-kimi-k2.5", "mimo-v2.5-pro", "mimo-v2.5"]

  # Early stream termination: when an upstream SSE chunk contains one of these
  # sentinels (e.g. Gitee AI emits "<mask>DONE</mask>" once generation is
  # effectively complete), the proxy stops reading from the upstream stream
  # immediately — saving billing for tokens that would never be delivered —
  # and appends a well-formed terminator so the downstream still receives a
  # complete, parseable response. Configurable via
  # `config :exhub, :giteeai_early_done_markers, [...]`; an empty list disables.
  @early_done_config_key :giteeai_early_done_markers
  @default_early_done_markers ["<mask>DONE</mask>"]
  @openai_sse_done "data: [DONE]\n\n"
  @anthropic_sse_stop "event: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"

  @doc """
  Returns whether proxy should be used for the given provider.

  Configuration via `:exhub, :proxy_providers`:
  - `:all` — all providers use proxy (default for backward compatibility)
  - `list(String.t())` — only specified providers use proxy
  - `:none` or `[]` — no providers use proxy
  """
  def proxy_enabled_for_provider?(provider) when is_binary(provider) do
    providers = Application.get_env(:exhub, :proxy_providers, :all)

    case providers do
      :all -> true
      :none -> false
      list when is_list(list) -> provider in list
      _ -> true
    end
  end

  @doc """
  Returns the proxy URL to use for a given provider, or an empty string
  if no proxy should be used.
  """
  def proxy_for_provider(provider) when is_binary(provider) do
    if proxy_enabled_for_provider?(provider) do
      Application.get_env(:exhub, :proxy, "")
    else
      ""
    end
  end

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
        Plug.Conn.put_req_header(conn, header, to_string(value))
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

  @doc false
  def wrap_stream(stream, model_name, provider, request_body) do
    acc_ref = :erlang.unique_integer([:positive, :monotonic])
    acc_ref_bin = Integer.to_string(acc_ref)

    acc_pid = spawn(fn -> stream_accumulator(acc_ref_bin, [], request_body) end)

    wrapped_stream =
      stream
      |> Stream.transform(
        fn -> {acc_pid, acc_ref_bin, [], scanner_init()} end,
        &transform_stream_element/2,
        &flush_stream_tail/1,
        fn {pid, ref, _chunks, _scanner} ->
          send(pid, {:process, ref, model_name, provider})
          :ok
        end
      )

    wrapped_stream
  end

  # ── Early stream termination (<mask>DONE</mask>) ──────────────────────────

  # The scanner buffers the trailing partial SSE frame (bytes since the last
  # "\n\n" boundary) so a marker can be detected even when it is split across
  # upstream chunks. Only complete frames are ever emitted, so the downstream
  # never receives a truncated, unparseable JSON event.
  defp scanner_init do
    %{markers: early_done_markers(), carry: "", anthropic?: false, done: false, dropped: 0}
  end

  defp early_done_markers do
    Application.get_env(:exhub, @early_done_config_key, @default_early_done_markers) || []
  end

  defp transform_stream_element({:chunk, chunk}, {pid, ref, acc, scanner}) do
    if scanner.done do
      # We already emitted the truncated frames + terminator; stop reading the
      # upstream stream so the connection is closed as soon as possible.
      updated = %{scanner | dropped: scanner.dropped + byte_size(chunk)}

      Logger.debug(fn ->
        "Early-done marker already seen; ignoring #{byte_size(chunk)} more upstream bytes"
      end)

      {:halt, {pid, ref, acc, updated}}
    else
      case process_chunk(chunk, scanner) do
        {:emit, emitted, updated} ->
          acc = maybe_accumulate(pid, ref, acc, emitted)
          {emit_chunks(emitted), {pid, ref, acc, updated}}

        {:terminate, emitted, terminator, updated} ->
          updated = %{updated | done: true}
          acc = maybe_accumulate(pid, ref, acc, emitted)
          {emit_chunks(emitted) ++ [{:chunk, terminator}], {pid, ref, acc, updated}}
      end
    end
  end

  defp transform_stream_element(_other, {pid, ref, acc, %{done: true} = scanner}) do
    {:halt, {pid, ref, acc, scanner}}
  end

  defp transform_stream_element(other, {pid, ref, acc, scanner}) do
    {[other], {pid, ref, acc, scanner}}
  end

  defp process_chunk(chunk, %{markers: []} = scanner), do: {:emit, chunk, scanner}

  defp process_chunk(chunk, scanner) do
    combined = scanner.carry <> chunk

    case find_marker(combined, scanner.markers) do
      nil ->
        {frames, partial} = split_at_last_frame_boundary(combined)

        updated = %{
          scanner
          | carry: partial,
            anthropic?: scanner.anthropic? or anthropic_sse?(frames)
        }

        {:emit, frames, updated}

      {pos, marker} ->
        # Everything from the marker onward is dropped. Only complete SSE
        # frames (ending in "\n\n") are forwarded; the partial frame that
        # carried the marker is dropped rather than emitted half-broken.
        prefix = binary_part(combined, 0, pos)
        emitted = truncate_to_frame_boundary(prefix)
        anthropic? = scanner.anthropic? or anthropic_sse?(prefix)
        terminator = if anthropic?, do: @anthropic_sse_stop, else: @openai_sse_done

        Logger.info(
          "Early-done marker #{inspect(marker)} detected; truncating stream and " <>
            "appending #{if anthropic?, do: "Anthropic message_stop", else: "OpenAI [DONE]"} terminator"
        )

        {:terminate, emitted, terminator, %{scanner | carry: "", anthropic?: anthropic?}}
    end
  end

  # Splits at the last "\n\n" boundary: frames = everything up to and including
  # that boundary, partial = the incomplete frame after it (held for detection).
  # Falls back to holding the whole input (never emitting partial frames) unless
  # the hold grows beyond @max_partial_frame_bytes, in which case it is flushed
  # as-is to avoid unbounded memory for non-SSE streams.
  @max_partial_frame_bytes 64 * 1024

  defp split_at_last_frame_boundary(combined) do
    case :binary.matches(combined, "\n\n") do
      [] ->
        if byte_size(combined) > @max_partial_frame_bytes do
          {combined, ""}
        else
          {"", combined}
        end

      matches ->
        {pos, len} = List.last(matches)
        boundary = pos + len

        {binary_part(combined, 0, boundary),
         binary_part(combined, boundary, byte_size(combined) - boundary)}
    end
  end

  defp find_marker(combined, markers) do
    markers
    |> Enum.reduce(:none, fn marker, best ->
      case :binary.match(combined, marker) do
        {pos, _len} ->
          case best do
            :none -> {pos, marker}
            {best_pos, _} when pos < best_pos -> {pos, marker}
            _ -> best
          end

        :nomatch ->
          best
      end
    end)
    |> case do
      :none -> nil
      match -> match
    end
  end

  @doc false
  def truncate_to_frame_boundary(binary) do
    case :binary.matches(binary, "\n\n") do
      [] ->
        ""

      matches ->
        {pos, len} = List.last(matches)
        binary_part(binary, 0, pos + len)
    end
  end

  defp anthropic_sse?(binary) do
    String.contains?(binary, "event: ")
  end

  defp emit_chunks(""), do: []
  defp emit_chunks(binary), do: [{:chunk, binary}]

  defp maybe_accumulate(_pid, _ref, acc, ""), do: acc

  defp maybe_accumulate(pid, ref, acc, binary) do
    send(pid, {:accumulate, ref, binary})
    [binary | acc]
  end

  # Runs when the upstream stream ends on its own (no marker seen): emit any
  # withheld bytes so nothing is silently dropped.
  defp flush_stream_tail({pid, ref, acc, %{done: true} = scanner}) do
    if scanner.dropped > 0 do
      Logger.debug("Early-done termination: dropped #{scanner.dropped} bytes after the marker")
    end

    {:halt, {pid, ref, acc, scanner}}
  end

  defp flush_stream_tail({pid, ref, acc, %{carry: ""} = scanner}) do
    {:halt, {pid, ref, acc, scanner}}
  end

  defp flush_stream_tail({pid, ref, acc, scanner}) do
    Logger.info(
      "Early-done marker never fired; flushing #{byte_size(scanner.carry)} trailing " <>
        "bytes at stream end"
    )

    acc = maybe_accumulate(pid, ref, acc, scanner.carry)
    {[{:chunk, scanner.carry}], {pid, ref, acc, %{scanner | carry: ""}}}
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

        if model_name in @kimi_reasoning_models do
          Exhub.Router.ReasoningCache.put_from_response(response_body)
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
    # Validate temperature for requests
    conn = validate_temperature(conn)

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

  defp validate_temperature(conn) do
    case conn.body_params do
      %{"temperature" => temp} when is_number(temp) and temp >= 0 and temp <= 1 ->
        conn

      %{"temperature" => _} ->
        %{conn | body_params: Map.put(conn.body_params, "temperature", 0.7)}

      _ ->
        %{conn | body_params: Map.put(conn.body_params, "temperature", 0.7)}
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
    body_params = fill_tool_calls_content(body_params)
    body_params = normalize_developer_role(body_params)
    body_params = inject_early_done_prompt(body_params)

    # Normalize model name by stripping prefixes before sending to API
    body_params =
      case Map.get(body_params, "model") do
        nil ->
          body_params

        model ->
          normalized_model =
            model
            |> Exhub.Router.Config.normalize_model_name()
            |> Exhub.Router.Config.resolve_model_alias()

          Map.put(body_params, "model", normalized_model)
      end

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

      %{"model" => model}
      when model in ["kimi-k2.5", "kimi-k2.6", "inf-kimi-k2.5", "mimo-v2.5-pro", "mimo-v2.5"] ->
        body_params
        |> Map.put("temperature", 1)
        |> Exhub.Router.Config.transform_request_body(model)
        |> Jason.encode!()

      _ ->
        Jason.encode!(body_params)
    end
  end

  defp fill_tool_calls_content(%{"messages" => messages} = body_params) when is_list(messages) do
    transformed_messages =
      Enum.map(messages, fn message ->
        if Map.has_key?(message, "tool_calls") and
             (is_nil(message["content"]) or message["content"] == "") do
          Map.put(message, "content", "exploring")
        else
          message
        end
      end)

    Map.put(body_params, "messages", transformed_messages)
  end

  defp fill_tool_calls_content(body_params), do: body_params

  defp normalize_developer_role(%{"messages" => messages} = body_params) when is_list(messages) do
    transformed_messages =
      Enum.map(messages, fn message ->
        if is_map(message) and Map.get(message, "role") == "developer" do
          Map.put(message, "role", "system")
        else
          message
        end
      end)

    Map.put(body_params, "messages", transformed_messages)
  end

  defp normalize_developer_role(body_params), do: body_params

  # When early stream termination is enabled, the upstream model must be told to
  # emit the configured done marker once its response is final — otherwise the
  # scanner never sees it. Injection is gated on the same config the scanner
  # reads (`:giteeai_early_done_markers`) and restricted to Gitee AI models so
  # other providers routed through this plug are left untouched.
  @doc false
  def inject_early_done_prompt(body_params) do
    with markers when markers != [] <- early_done_markers(),
         %{"model" => model} when is_binary(model) <- body_params,
         :giteeai <- Exhub.LLMModels.model_provider(model),
         %{"messages" => messages} when is_list(messages) <- body_params do
      Logger.debug(
        "Injecting early-done marker prompt for giteeai model #{inspect(model)} " <>
          "(markers: #{inspect(markers)})"
      )

      Map.put(body_params, "messages", prepend_early_done_prompt(markers, messages))
    else
      _ -> body_params
    end
  end

  defp prepend_early_done_prompt(markers, messages) do
    prompt = early_done_prompt(markers)

    case messages do
      [%{"role" => "system", "content" => content} = first | rest]
      when is_binary(content) ->
        [%{first | "content" => content <> "\n\n" <> prompt} | rest]

      _ ->
        [%{"role" => "system", "content" => prompt} | messages]
    end
  end

  defp early_done_prompt(markers) do
    listed =
      markers
      |> Enum.map(&("\"" <> &1 <> "\""))
      |> Enum.join(" or ")

    "End your final answer with the exact literal marker #{listed} as the very " <>
      "last characters — no trailing text, whitespace, or newline after it. " <>
      "Never emit the marker anywhere else."
  end

  defp extract_model_name(conn) do
    case conn.body_params do
      %{"model" => model} when is_binary(model) -> model
      _ -> "unknown"
    end
  end

  def extract_provider(path) do
    cond do
      String.contains?(path, "openai") -> "openai"
      String.contains?(path, "anthropic") -> "anthropic"
      String.contains?(path, "groq") -> "groq"
      String.contains?(path, "google") -> "google"
      String.contains?(path, "cohere") -> "cohere"
      String.contains?(path, "samba") -> "samba"
      String.contains?(path, "infini") -> "infini"
      String.contains?(path, "baidu-anthropic") -> "anthropic"
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
    if model_name in @kimi_reasoning_models do
      Exhub.Router.ReasoningCache.put_from_response(resp_body)
    end

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
