defmodule Exhub.Fim.Client do
  @moduledoc """
  FIM (fill-in-the-middle) HTTP client for ExHub.

  Replaces the Emacs-side `plz` HTTP requests in `exhub-fim.el` for FIM
  providers (Codestral, DeepSeek-style OpenAI-FIM-compatible endpoints).
  Given a context map (before/after cursor plus language/tab comment), it
  builds the standard FIM prompt/suffix pair, posts it to the provider, and
  decodes the streamed SSE response into the completion text.

  The response format mirrors what the original Emacs client consumed:
  `data: {…}` lines whose JSON carries `choices[0].text` (DeepSeek-style) or
  `choices[0].delta.content` (Codestral's chat-shaped chunks).

  Remote endpoints are reached through the shared ExHub egress proxy
  (`:exhub, :proxy`, the same setting the router's proxy routes use) when one is
  configured. Local endpoints (Ollama, llama.cpp, …) are always dialed direct.
  """

  alias Exhub.TLSCompat

  # Provider defaults.
  #
  # `llms_key` is the `:exhub, :llms` map entry whose `:api_key` is used when
  # no explicit API key is supplied and no `app_env_key` is configured.
  @providers %{
    "codestral" => %{
      endpoint: "https://codestral.mistral.ai/v1/fim/completions",
      model: "codestral-latest",
      llms_key: "codestral/codestral-latest",
      app_env_key: :codestral_api_key,
      api_key_env: "CODESTRAL_API_KEY"
    },
    "openai-fim-compatible" => %{
      endpoint: "https://api.deepseek.com/beta/completions",
      model: "deepseek-chat",
      # No fixed `:llms` entry for the generic OpenAI-FIM provider; the key
      # usually comes from an env var or an explicit request option.
      llms_key: nil,
      app_env_key: :deepseek_api_key,
      api_key_env: "DEEPSEEK_API_KEY"
    }
  }

  @default_timeout_ms 60_000

  @doc "Provider names understood by `complete/3`."
  def providers, do: Map.keys(@providers)

  @doc "Resolved configuration for PROVIDER, merged with request OPTS."
  def provider_config(provider, opts \\ %{}) do
    defaults = Map.get(@providers, provider, %{})
    opts = if is_map(opts), do: opts, else: %{}

    %{
      endpoint: string_or(Map.get(opts, "endpoint"), defaults[:endpoint]),
      model: string_or(Map.get(opts, "model"), defaults[:model]),
      api_key: resolve_api_key(Map.get(opts, "api_key"), defaults),
      timeout_ms: positive_int(Map.get(opts, "timeout_ms"), @default_timeout_ms)
    }
  end

  @doc """
  Builds the FIM prompt from CONTEXT, mirroring
  `exhub-fim--default-fim-prompt-function`: language/tab comment line followed
  by the code before the cursor.
  """
  def build_prompt(context) do
    language_and_tab = Map.get(context || %{}, "language-and-tab", "")
    before_cursor = Map.get(context || %{}, "before-cursor", "")
    "#{language_and_tab}\n#{before_cursor}"
  end

  @doc "Builds the FIM suffix from CONTEXT (code after the cursor)."
  def build_suffix(context) do
    Map.get(context || %{}, "after-cursor", "")
  end

  @doc """
  Decodes a streamed SSE body into the concatenated completion text.

  Each `data: {…}` line contributes its completion text — `choices[0].text` or
  `choices[0].delta.content` (see `completion_text/1`); `data: [DONE]`,
  comments, and malformed lines are skipped (mirrors
  `exhub-fim--stream-decode` with the Emacs `*-get-text-fn` helpers).
  """
  def parse_sse(body) when is_binary(body) do
    body
    |> String.split("\n")
    |> Enum.reduce("", &accumulate_sse_line/2)
  end

  def parse_sse(_), do: ""

  @doc """
  Requests one FIM completion for PROVIDER from CONTEXT with OPTS.

  Returns `{:ok, text}` (text may be empty when the model produced no text),
  or `{:error, reason}`.
  """
  def complete(provider, context, opts \\ %{}) when is_binary(provider) do
    config = provider_config(provider, opts)

    if blank?(config[:api_key]) do
      env_name = get_in(@providers, [provider, :api_key_env]) || ""

      {:error,
       "API key not configured for provider #{provider} (set #{env_name} or pass an :api_key option)"}
    else
      do_request(config, context)
    end
  end

  defp do_request(config, context) do
    body =
      Jason.encode!(%{
        "model" => config.model,
        "prompt" => build_prompt(context),
        "suffix" => build_suffix(context),
        "stream" => true
      })

    headers = [
      {"Content-Type", "application/json"},
      {"Authorization", "Bearer #{config.api_key}"}
    ]

    # `recv_timeout` bounds each socket read, matching the previous
    # `Req.post(receive_timeout:)` behaviour on a streamed body.
    options =
      [recv_timeout: config.timeout_ms]
      |> put_proxy_option(config.endpoint)
      |> Enum.concat(TLSCompat.httpoison_opts(config.endpoint))

    case HTTPoison.post(config.endpoint, body, headers, options) do
      {:ok, %HTTPoison.Response{status_code: status, body: resp_body}}
      when status in 200..299 ->
        {:ok, parse_sse(resp_body)}

      {:ok, %HTTPoison.Response{status_code: status, body: resp_body}} ->
        {:error, "HTTP #{status}: #{truncate_body(resp_body)}"}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, inspect(reason)}
    end
  end

  defp accumulate_sse_line("data: " <> payload, acc) do
    case String.trim(payload) do
      "" ->
        acc

      "[DONE]" ->
        acc

      json ->
        case Jason.decode(json) do
          {:ok, decoded} -> acc <> completion_text(decoded)
          _ -> acc
        end
    end
  end

  defp accumulate_sse_line(_line, acc), do: acc

  # Providers stream two chunk shapes:
  #
  #   * `choices[0].delta.content` — Codestral's `/fim/completions` answers with
  #     `chat.completion.chunk` objects (old Emacs `exhub-fim--openai-get-text-fn`)
  #   * `choices[0].text` — DeepSeek-style FIM endpoints
  #     (old Emacs `exhub-fim--openai-fim-get-text-fn`)
  defp completion_text(decoded) do
    choice = decoded |> get_in(["choices", Access.at(0)]) || %{}

    case choice["text"] || get_in(choice, ["delta", "content"]) do
      text when is_binary(text) -> text
      _ -> ""
    end
  end

  @doc """
  True when ENDPOINT is served from this machine (or is unparseable), so the
  egress proxy must be skipped.
  """
  def local_endpoint?(endpoint) when is_binary(endpoint) do
    case URI.parse(endpoint).host do
      host when is_binary(host) ->
        host in ["localhost", "127.0.0.1", "0.0.0.0", "::1"] or
          String.ends_with?(host, ".localhost")

      _ ->
        true
    end
  end

  def local_endpoint?(_endpoint), do: true

  # Honour the shared ExHub egress proxy (`:exhub, :proxy`) for remote FIM
  # endpoints. hackney's `:proxy` option takes the raw URL string — the same
  # shape the router's proxy routes pass via `ProxyPlug.proxy_for_provider/1`.
  # The option is only added when a proxy applies, so machine-local endpoints
  # always stay direct.
  defp put_proxy_option(options, endpoint) do
    proxy_url = Application.get_env(:exhub, :proxy, "")

    if is_binary(proxy_url) and proxy_url != "" and not local_endpoint?(endpoint) do
      Keyword.put(options, :proxy, proxy_url)
    else
      options
    end
  end

  defp resolve_api_key(nil, defaults) do
    llms = Application.get_env(:exhub, :llms, %{})
    llm_key = defaults[:llms_key] && llms[defaults[:llms_key]]

    Enum.find_value(
      [
        llm_key && llm_key[:api_key],
        defaults[:app_env_key] && Application.get_env(:exhub, defaults[:app_env_key]),
        defaults[:api_key_env] && System.get_env(defaults[:api_key_env])
      ],
      fn
        nil -> nil
        "" -> nil
        value -> value
      end
    )
  end

  defp resolve_api_key("", _defaults), do: nil
  defp resolve_api_key(value, _defaults) when is_binary(value), do: value
  defp resolve_api_key(_, _defaults), do: nil

  defp string_or(nil, default), do: default
  defp string_or("", default), do: default
  defp string_or(value, _default) when is_binary(value), do: value
  defp string_or(_, default), do: default

  defp positive_int(value, _default) when is_integer(value) and value > 0, do: value
  defp positive_int(_, default), do: default

  defp blank?(nil), do: true
  defp blank?(""), do: true
  defp blank?(_), do: false

  defp truncate_body(body) when is_binary(body) and byte_size(body) > 200 do
    binary_part(body, 0, 200)
  end

  defp truncate_body(body) when is_binary(body), do: body
  defp truncate_body(_), do: ""
end
