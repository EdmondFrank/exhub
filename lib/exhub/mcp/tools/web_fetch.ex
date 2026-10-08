defmodule Exhub.MCP.Tools.WebFetch do
  @moduledoc """
  MCP Tool for fetching web content from URLs.

  This tool allows fetching content from a URL and returning the page content
  as simplified text after parsing HTML.

  ## Egress

  Outbound HTTP requests go direct unless Smart Decide says otherwise, the same
  rule the Desktop shell tools follow: `Exhub.MCP.WebTools.Proxy` escalates a
  failure that looks like a blocked route to one System One call, which may
  approve a single retry through a proxy it judged reachable and worth the leak
  risk. `Application.get_env(:exhub, :proxy)` is that call's first candidate,
  not an unconditional injection.
  """

  alias Anubis.Server.Response
  alias Exhub.MCP.WebTools.Proxy

  use Anubis.Server.Component, type: :tool

  def name, do: "web_fetch"

  @impl true
  def description do
    """
    Fetch content from a URL or local file. Returns the content as simplified text.

    This tool allows you to retrieve content from:
    - Web pages (http/https URLs) via HTTP requests
    - Local files (file:// URLs) by reading from filesystem

    For web content, the response is parsed and returned as clean text, stripping HTML markup.
    For local files, the content is read directly and HTML is parsed if applicable.

    Supports GET, POST, and HEAD methods for HTTP requests.
    Custom headers and request body can be provided for POST requests.

    Set render_js to true to render JavaScript-heavy pages via headless Chrome (KuriDaemon).
    This is useful for SPAs, dynamically loaded content, or pages that require JS execution.

    Plain HTTP requests start direct: only a transport-layer failure (timeout,
    refused/reset connection, DNS or TLS breakage) is escalated to Smart Decide,
    which may approve one retry through a proxy it judged reachable and safe.
    A successful structured response that went through a proxy carries a
    "proxy" field explaining the verdict.
    """
  end

  schema do
    field(:url, {:required, :string}, description: "The URL to fetch")

    field(:method, :string,
      description: "HTTP method - \"GET\", \"POST\", \"HEAD\" (default GET)"
    )

    field(:headers, :map, description: "Optional HTTP headers as key-value pairs")
    field(:body, :string, description: "Optional request body for POST requests")

    field(:render_js, :boolean,
      description: "Render JavaScript via headless Chrome before extracting text. Default false."
    )
  end

  @impl true
  def execute(params, frame) do
    url = Map.get(params, :url)
    method = Map.get(params, :method, "GET") |> String.upcase()
    headers = Map.get(params, :headers, %{}) || %{}
    body = Map.get(params, :body)
    render_js = Map.get(params, :render_js, false)

    cond do
      is_nil(url) or url == "" ->
        resp = Response.tool() |> Response.error("URL is required")
        {:reply, resp, frame}

      not valid_url?(url) ->
        resp = Response.tool() |> Response.error("Invalid URL: #{url}")
        {:reply, resp, frame}

      true ->
        do_fetch(url, method, headers, body, render_js, frame)
    end
  end

  # Private functions

  # `web_fetch` does not let hackney follow redirects: hackney 1.23.0 returns a
  # bare `:badarg` (or a `transport_scheme/1` FunctionClauseError) when it
  # follows a redirect — with or without a proxy — which HTTPoison surfaces as a
  # `CaseClauseError`. We follow the Location chain here instead, bounded by
  # this budget.
  @max_redirects 5

  defp valid_url?(url) do
    case URI.parse(url) do
      %URI{scheme: scheme, host: host} when scheme in ["http", "https"] and not is_nil(host) ->
        true

      %URI{scheme: "file", path: path} when not is_nil(path) ->
        true

      _ ->
        false
    end
  end

  defp do_fetch(url, method, headers, body, render_js, frame) do
    case URI.parse(url) do
      %URI{scheme: "file", path: path} ->
        do_fetch_file(path, frame)

      _ when render_js ->
        do_fetch_rendered(url, frame)

      _ ->
        do_fetch_http(url, method, headers, body, frame)
    end
  end

  defp do_fetch_file(path, frame) do
    case File.read(path) do
      {:ok, content} ->
        # Ensure content is valid UTF-8 string
        content_str =
          case String.valid?(content) do
            true -> content
            false -> Base.encode64(content)
          end

        # Try to parse as HTML if it looks like HTML (case-insensitive check)
        parsed_content =
          if looks_like_html?(content_str) do
            parse_html_content(content_str)
          else
            content_str
          end

        resp =
          Response.tool()
          |> Response.structured(%{
            "success" => true,
            "url" => "file://#{path}",
            "status_code" => 200,
            "content" => parsed_content
          })

        {:reply, resp, frame}

      {:error, :enoent} ->
        resp = Response.tool() |> Response.error("File not found: #{path}")
        {:reply, resp, frame}

      {:error, :eacces} ->
        resp = Response.tool() |> Response.error("Permission denied: #{path}")
        {:reply, resp, frame}

      {:error, reason} ->
        resp = Response.tool() |> Response.error("Failed to read file: #{inspect(reason)}")
        {:reply, resp, frame}
    end
  end

  defp do_fetch_rendered(url, frame) do
    case Exhub.KuriDaemon.status() do
      :healthy ->
        base = Exhub.KuriDaemon.base_url()
        encoded_url = URI.encode_www_form(url)

        with {:ok, tab_id} <- kuri_new_tab(base, encoded_url),
             {:ok, text} <- kuri_get_text(base, tab_id) do
          # Best-effort cleanup
          _ = kuri_close_tab(base, tab_id)

          resp =
            Response.tool()
            |> Response.structured(%{
              "success" => true,
              "url" => url,
              "status_code" => 200,
              "render_js" => true,
              "content" => text
            })

          {:reply, resp, frame}
        else
          {:error, reason} ->
            resp =
              Response.tool()
              |> Response.error("render_js failed: #{reason}. KuriDaemon may be unavailable.")

            {:reply, resp, frame}
        end

      status ->
        resp =
          Response.tool()
          |> Response.error(
            "render_js requires KuriDaemon to be healthy (current status: #{status}). " <>
              "Ensure kuri is installed and :kuri_enabled is true."
          )

        {:reply, resp, frame}
    end
  end

  defp kuri_new_tab(base, encoded_url) do
    url = "#{base}/tab/new?url=#{encoded_url}&wait=true"

    case kuri_http_get(url, 30_000) do
      {:ok, body} ->
        case Jason.decode(body) do
          {:ok, %{"tab_id" => tab_id}} when tab_id != "" and tab_id != "unknown" ->
            {:ok, tab_id}

          {:ok, %{"status" => "created", "tab_id" => tab_id}} ->
            {:ok, tab_id}

          {:ok, other} ->
            {:error, "unexpected /tab/new response: #{inspect(other)}"}

          {:error, _} ->
            {:error, "failed to decode /tab/new response"}
        end

      {:error, reason} ->
        {:error, "kuri /tab/new request failed: #{inspect(reason)}"}
    end
  end

  @render_js_max_wait_ms 10_000
  @render_js_poll_interval_ms 1_000

  defp kuri_get_text(base, tab_id) do
    poll_text(base, tab_id, @render_js_max_wait_ms, 0, nil)
  end

  # Poll /text until we get meaningful content or hit the max timeout.
  defp poll_text(_base, _tab_id, max_wait, elapsed, _last_text) when elapsed >= max_wait do
    {:error, "timed out waiting for page content after #{max_wait}ms"}
  end

  defp poll_text(base, tab_id, max_wait, elapsed, last_text) do
    # Wait before first poll and between polls
    Process.sleep(@render_js_poll_interval_ms)

    url = "#{base}/text?tab_id=#{URI.encode_www_form(tab_id)}"

    case kuri_http_get(url, 10_000) do
      {:ok, body} ->
        case extract_text_value(body) do
          {:ok, text} ->
            if meaningful_content?(text) do
              {:ok, text}
            else
              # Content is empty or still loading — keep polling
              poll_text(base, tab_id, max_wait, elapsed + @render_js_poll_interval_ms, text)
            end

          {:error, _} ->
            # Unexpected format — retry once more then give up
            poll_text(base, tab_id, max_wait, elapsed + @render_js_poll_interval_ms, last_text)
        end

      {:error, reason} ->
        # Transient CDP failure — retry
        if elapsed + @render_js_poll_interval_ms >= max_wait do
          {:error, "kuri /text request failed: #{inspect(reason)}"}
        else
          poll_text(base, tab_id, max_wait, elapsed + @render_js_poll_interval_ms, last_text)
        end
    end
  end

  defp extract_text_value(body) do
    case Jason.decode(body) do
      {:ok, %{"result" => %{"result" => %{"value" => text}}}} when is_binary(text) ->
        {:ok, text}

      {:ok, %{"result" => %{"value" => text}}} when is_binary(text) ->
        {:ok, text}

      {:ok, _other} ->
        {:error, :unexpected_format}

      {:error, _} ->
        # Response might be plain text
        {:ok, body}
    end
  end

  # Heuristic: content is "meaningful" if it's non-trivial and not just a loading indicator.
  defp meaningful_content?(text) when is_binary(text) do
    trimmed = String.trim(text)

    cond do
      String.length(trimmed) < 20 -> false
      loading_indicator?(trimmed) -> false
      true -> true
    end
  end

  defp meaningful_content?(_), do: false

  defp loading_indicator?(text) do
    downcased = String.downcase(text)

    Enum.any?(
      ["正在加载", "loading", "please wait", "加载中...", "initializing"],
      &String.contains?(downcased, &1)
    ) and String.length(text) < 100
  end

  defp kuri_close_tab(base, tab_id) do
    url = "#{base}/tab/close?tab_id=#{URI.encode_www_form(tab_id)}"
    kuri_http_get(url, 5_000)
  end

  defp kuri_http_get(url, timeout) do
    headers = [{~c"authorization", to_charlist("Bearer #{Exhub.KuriDaemon.api_token()}")}]

    case :httpc.request(:get, {to_charlist(url), headers}, [timeout: timeout],
           body_format: :binary
         ) do
      {:ok, {{_, 200, _}, _headers, body}} ->
        {:ok, body}

      {:ok, {{_, status, _}, _headers, body}} ->
        {:error, {:http_error, status, body}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  # Direct by default — see `Exhub.MCP.WebTools.Proxy`. A transport-shaped
  # failure is escalated to Smart Decide, which may approve exactly one retry
  # through a proxy it judged reachable and worth the leak risk. Nothing is
  # injected the decision did not approve, and an HTTP status is never a
  # proxy candidate.
  defp do_fetch_http(url, method, headers, body, frame) do
    command = Proxy.evidence_command(method, url)

    case Proxy.route(command) do
      {:proxy, proxy_url, meta} ->
        fetch_via(command, url, method, headers, body, proxy_url, meta, 1, @max_redirects, frame)

      {:direct, meta} ->
        fetch_via(command, url, method, headers, body, nil, meta, 1, @max_redirects, frame)
    end
  end

  defp fetch_via(
         command,
         url,
         method,
         headers,
         body,
         proxy_url,
         meta,
         attempt,
         redirects_left,
         frame
       ) do
    case http_request(method, url, headers, body, proxy_url) do
      {:ok,
       %HTTPoison.Response{
         status_code: status_code,
         body: response_body,
         headers: response_headers
       }}
      when status_code in 200..299 ->
        content =
          if method == "HEAD" do
            format_head_response(response_headers)
          else
            parse_html_content(response_body)
          end

        data = %{
          "success" => true,
          "url" => url,
          "status_code" => status_code,
          "content" => content
        }

        data =
          if proxy_url do
            Map.put(data, "proxy", Proxy.annotate(proxy_url, meta, attempt))
          else
            data
          end

        {:reply, Response.tool() |> Response.structured(data), frame}

      {:ok, %HTTPoison.Response{status_code: status_code, headers: response_headers}}
      when status_code in 300..399 ->
        follow_redirect(
          command,
          url,
          method,
          headers,
          body,
          proxy_url,
          meta,
          attempt,
          redirects_left,
          status_code,
          response_headers,
          frame
        )

      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        # The peer answered: the failure is application-layer, a proxy changes
        # nothing here, so no decision is consulted.
        resp =
          Response.tool()
          |> Response.error("HTTP error: status code #{status_code}")

        {:reply, resp, frame}

      {:error, {:unsupported_method, unsupported}} ->
        resp =
          Response.tool()
          |> Response.error("Unsupported HTTP method: #{unsupported}")

        {:reply, resp, frame}

      {:error, %HTTPoison.Error{reason: reason}} ->
        handle_transport_failure(
          command,
          url,
          method,
          headers,
          body,
          proxy_url,
          meta,
          attempt,
          reason,
          frame
        )

      {:error, {:exception, exception}} ->
        # Defence in depth: `http_request/5` already keeps hackney off its
        # redirect path, but a dependency crash must never surface as a bare
        # `%CaseClauseError{}` — report it like any other failure.
        resp =
          Response.tool()
          |> Response.error("HTTP request failed: #{describe_exception(exception)}")

        {:reply, resp, frame}

      {:error, reason} ->
        resp =
          Response.tool()
          |> Response.error("HTTP request failed: #{inspect(reason)}")

        {:reply, resp, frame}
    end
  end

  # Follows one redirect hackney did not follow itself (a proxied request, or a
  # non-GET/HEAD redirect hackney hands back). The Location chain is walked
  # here, up to `@max_redirects`, instead of inside hackney.
  defp follow_redirect(
         command,
         url,
         method,
         headers,
         body,
         proxy_url,
         meta,
         attempt,
         redirects_left,
         status,
         response_headers,
         frame
       ) do
    new_url = resolve_redirect(url, redirect_location(response_headers))

    cond do
      redirects_left <= 0 ->
        resp =
          Response.tool()
          |> Response.error(
            "HTTP request failed: too many redirects (more than #{@max_redirects})"
          )

        {:reply, resp, frame}

      is_nil(new_url) ->
        resp = Response.tool() |> Response.error("HTTP error: status code #{status}")
        {:reply, resp, frame}

      true ->
        {new_method, new_body} = redirected_request(status, method, body)
        new_headers = maybe_strip_credentials(headers, url, new_url)

        # A redirect can point at a bypassed host (loopback/NO_PROXY); the proxy
        # that carried the first hop must not be reused for it.
        next_proxy =
          if Proxy.bypassed?(Proxy.evidence_command(new_method, new_url)),
            do: nil,
            else: proxy_url

        fetch_via(
          command,
          new_url,
          new_method,
          new_headers,
          new_body,
          next_proxy,
          meta,
          attempt,
          redirects_left - 1,
          frame
        )
    end
  end

  @doc false
  def redirect_location(headers) when is_list(headers) do
    Enum.find_value(headers, fn {key, value} ->
      if String.downcase(to_string(key)) == "location", do: to_string(value)
    end)
  end

  def redirect_location(_headers), do: nil

  @doc false
  def resolve_redirect(base, location) when is_binary(location) do
    case URI.merge(base, location) do
      %URI{scheme: scheme, host: host} = uri
      when scheme in ["http", "https"] and is_binary(host) ->
        URI.to_string(uri)

      _ ->
        nil
    end
  rescue
    _ -> nil
  end

  def resolve_redirect(_base, _location), do: nil

  # 303 always becomes GET; a 301/302 POST becomes GET (browser behaviour);
  # 307/308 keep the method and body; HEAD stays HEAD.
  @doc false
  def redirected_request(_status, "HEAD", _body), do: {"HEAD", nil}
  def redirected_request(303, _method, _body), do: {"GET", nil}
  def redirected_request(status, "POST", _body) when status in [301, 302], do: {"GET", nil}
  def redirected_request(_status, method, body), do: {method, body}

  # Credentials must not follow a redirect to another origin.
  @doc false
  def maybe_strip_credentials(headers, from, to) when is_map(headers) do
    if same_origin?(from, to), do: headers, else: drop_sensitive_headers(headers)
  end

  def maybe_strip_credentials(headers, _from, _to), do: headers

  defp same_origin?(from, to) do
    a = URI.parse(from)
    b = URI.parse(to)
    {a.scheme, a.host, a.port} == {b.scheme, b.host, b.port}
  end

  defp drop_sensitive_headers(headers) do
    Map.reject(headers, fn {key, _value} ->
      String.downcase(to_string(key)) in ["authorization", "cookie", "proxy-authorization"]
    end)
  end

  defp describe_exception(%{__struct__: _} = exception), do: Exception.message(exception)
  defp describe_exception(exception), do: inspect(exception)

  defp handle_transport_failure(
         command,
         url,
         method,
         headers,
         body,
         proxy_url,
         meta,
         attempt,
         reason,
         frame
       ) do
    failure = Proxy.transport_failure(reason)

    cond do
      is_nil(failure) ->
        transport_error(reason, proxy_url, meta, attempt, nil, nil, frame)

      attempt >= 2 ->
        transport_error(reason, proxy_url, meta, attempt, :attempts_exhausted, meta, frame)

      not Proxy.decision_enabled?() ->
        transport_error(reason, proxy_url, meta, attempt, :decision_disabled, nil, frame)

      true ->
        decide_retry(
          command,
          url,
          method,
          headers,
          body,
          proxy_url,
          meta,
          attempt,
          reason,
          failure,
          frame
        )
    end
  end

  defp decide_retry(
         command,
         url,
         method,
         headers,
         body,
         proxy_url,
         meta,
         attempt,
         reason,
         failure,
         frame
       ) do
    case Proxy.judge(method, url, failure) do
      {:proxy, retry_url, detail} ->
        if retry_url == proxy_url do
          transport_error(reason, proxy_url, meta, attempt, :already_proxied, detail, frame)
        else
          fetch_via(
            command,
            url,
            method,
            headers,
            body,
            retry_url,
            Map.put(detail, "decision", "smart_decide"),
            attempt + 1,
            @max_redirects,
            frame
          )
        end

      {:noop, verdict, detail} ->
        # The judged fix can be the opposite of the transport in use (the proxy
        # is what is broken). That is still a Smart Decide verdict, so the
        # direct retry is made on its advice only — never as a guess.
        if proxy_url && Proxy.advises_direct?(verdict, detail) do
          fetch_via(
            command,
            url,
            method,
            headers,
            body,
            nil,
            %{"decision" => "direct", "mechanism" => "direct_no_proxy"},
            attempt + 1,
            @max_redirects,
            frame
          )
        else
          transport_error(reason, proxy_url, meta, attempt, verdict, detail, frame)
        end
    end
  end

  defp transport_error(reason, proxy_url, meta, attempt, verdict, detail, frame) do
    note =
      Proxy.failure_note(
        proxy_url: proxy_url,
        decision: Map.get(meta || %{}, "decision"),
        verdict: verdict,
        detail: detail
      )

    attempts = if attempt > 1, do: " (after #{attempt} attempts)", else: ""

    resp =
      Response.tool()
      |> Response.error("HTTP request failed: #{inspect(reason)}#{note}#{attempts}")

    {:reply, resp, frame}
  end

  @request_timeout_ms 30_000

  defp http_request(method, url, headers, body, proxy_url) do
    http_options =
      [
        hackney: [
          # hackney's redirect follower returns a bare `:badarg`, so every
          # request asks it not to follow and `follow_redirect/12` walks the
          # Location chain itself.
          follow_redirect: false,
          max_redirect: @max_redirects,
          timeout: @request_timeout_ms,
          recv_timeout: @request_timeout_ms,
          # Even a "direct" request must not silently pick up an exported
          # HTTPS_PROXY/HTTP_PROXY/ALL_PROXY: hackney reads those by default.
          # The explicit `proxy:` option — added only when the decision approves
          # one — is the sole transport this tool uses.
          no_proxy_env: true
        ]
      ]
      |> Proxy.add_proxy(proxy_url)

    http_headers = Map.to_list(headers)

    try do
      case method do
        "GET" ->
          HTTPoison.get(url, http_headers, http_options)

        "POST" ->
          HTTPoison.post(url, body || "", http_headers, http_options)

        "HEAD" ->
          HTTPoison.head(url, http_headers, http_options)

        other ->
          {:error, {:unsupported_method, other}}
      end
    rescue
      exception ->
        {:error, {:exception, exception}}
    catch
      kind, reason ->
        {:error, {:exception, {kind, reason}}}
    end
  end

  defp parse_html_content(html) when is_binary(html) do
    {:ok, document} = Floki.parse_document(html)

    # Remove script and style elements
    cleaned =
      document
      |> Floki.filter_out("script")
      |> Floki.filter_out("style")
      |> Floki.filter_out("noscript")

    # Get body content or fall back to full document
    body_content =
      case Floki.find(cleaned, "body") do
        [body | _] -> body
        [] -> cleaned
      end

    # Extract text and clean up whitespace
    text =
      body_content
      |> Floki.text(sep: " ")
      |> String.replace(~r/\s+/, " ")
      |> String.trim()

    text
  end

  defp parse_html_content(_), do: ""

  defp looks_like_html?(content) when is_binary(content) do
    downcased = String.downcase(content)

    # Must have proper HTML document structure indicators
    has_doctype_or_html =
      String.contains?(downcased, "<!doctype html") or
        String.contains?(downcased, "<html")

    # Check for body tag to confirm it's a full HTML document
    has_body = String.contains?(downcased, "<body")

    # Require both indicators for a confident HTML detection
    has_doctype_or_html and has_body
  end

  defp looks_like_html?(_), do: false

  defp format_head_response(headers) do
    headers
    |> Enum.map(fn {key, value} -> "#{key}: #{value}" end)
    |> Enum.join("\n")
  end
end
