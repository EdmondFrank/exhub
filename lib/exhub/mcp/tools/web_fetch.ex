defmodule Exhub.MCP.Tools.WebFetch do
  @moduledoc """
  MCP Tool for fetching web content from URLs.

  This tool allows fetching content from a URL and returning the page content
  as simplified text after parsing HTML.
  """

  alias Anubis.Server.Response

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
    """
  end

  schema do
    field(:url, {:required, :string}, description: "The URL to fetch")

    field(:method, :string,
      description: "HTTP method - \"GET\", \"POST\", \"HEAD\" (default GET)"
    )

    field(:headers, :map, description: "Optional HTTP headers as key-value pairs")
    field(:body, :string, description: "Optional request body for POST requests")
  end

  @impl true
  def execute(params, frame) do
    url = Map.get(params, :url)
    method = Map.get(params, :method, "GET") |> String.upcase()
    headers = Map.get(params, :headers, %{}) || %{}
    body = Map.get(params, :body)

    cond do
      is_nil(url) or url == "" ->
        resp = Response.tool() |> Response.error("URL is required")
        {:reply, resp, frame}

      not valid_url?(url) ->
        resp = Response.tool() |> Response.error("Invalid URL: #{url}")
        {:reply, resp, frame}

      true ->
        do_fetch(url, method, headers, body, frame)
    end
  end

  # Private functions

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

  defp do_fetch(url, method, headers, body, frame) do
    case URI.parse(url) do
      %URI{scheme: "file", path: path} ->
        do_fetch_file(path, frame)

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

  defp do_fetch_http(url, method, headers, body, frame) do
    proxy = Application.get_env(:exhub, :proxy, "")

    http_options =
      [
        hackney: [
          follow_redirect: true,
          max_redirect: 5,
          timeout: 30_000,
          recv_timeout: 30_000
        ]
      ]
      |> maybe_add_proxy(proxy)

    http_headers = Map.to_list(headers)

    result =
      case method do
        "GET" ->
          HTTPoison.get(url, http_headers, http_options)

        "POST" ->
          HTTPoison.post(url, body || "", http_headers, http_options)

        "HEAD" ->
          HTTPoison.head(url, http_headers, http_options)

        _ ->
          {:error, "Unsupported HTTP method: #{method}"}
      end

    case result do
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

        resp =
          Response.tool()
          |> Response.structured(%{
            "success" => true,
            "url" => url,
            "status_code" => status_code,
            "content" => content
          })

        {:reply, resp, frame}

      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        resp =
          Response.tool()
          |> Response.error("HTTP error: status code #{status_code}")

        {:reply, resp, frame}

      {:error, %HTTPoison.Error{reason: reason}} ->
        resp =
          Response.tool()
          |> Response.error("HTTP request failed: #{inspect(reason)}")

        {:reply, resp, frame}

      {:error, reason} when is_binary(reason) ->
        resp = Response.tool() |> Response.error(reason)
        {:reply, resp, frame}
    end
  end

  defp maybe_add_proxy(options, ""), do: options

  defp maybe_add_proxy(options, proxy) do
    Keyword.update(options, :hackney, [proxy: proxy], fn hackney_opts ->
      Keyword.put(hackney_opts, :proxy, proxy)
    end)
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
