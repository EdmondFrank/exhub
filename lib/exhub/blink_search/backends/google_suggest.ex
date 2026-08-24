defmodule Exhub.BlinkSearch.Backends.GoogleSuggest do
  @moduledoc """
  Google Suggest backend — fetches Google search suggestions.

  Requests honor the shared ExHub proxy (`Application.get_env(:exhub, :proxy)`)
  when configured. Also detects URLs and offers to open them directly.
  """

  use Exhub.BlinkSearch.Backend

  alias Exhub.BlinkSearch.Backend

  @impl true
  def search_match(prefix, _state) do
    words = String.split(prefix, ~r/\s+/, trim: true)

    if words == [] do
      []
    else
      # Check if prefix is a valid URL
      url_candidates =
        if valid_url?(prefix) do
          url = ensure_scheme(prefix)
          [url]
        else
          []
        end

      # Fetch Google suggestions
      suggestions = fetch_suggestions(prefix)

      url_candidates ++ suggestions
    end
  end

  @impl true
  def do_action(candidate, _state) do
    text = Backend.candidate_text(candidate)

    url =
      if valid_url?(text) do
        ensure_scheme(text)
      else
        "http://www.google.com/search?q=#{URI.encode(text)}"
      end

    Exhub.send_message(~s|(blink-search-browser-function #{Backend.elisp_quote(url)})|)
    :ok
  end

  # Private helpers

  defp fetch_suggestions(prefix) do
    query = String.replace(prefix, " ", "%20")
    url = "http://google.com/complete/search?client=chrome&q=#{query}"

    options =
      [
        headers: [
          {"user-agent",
           "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.102 Safari/537.36 Edge/18.19582"}
        ],
        receive_timeout: 5_000
      ]
      |> put_proxy_options()

    case Req.get(url, options) do
      {:ok, %{status: 200, body: body}} ->
        # Req auto-decodes JSON responses into a list; older/manual paths
        # may still yield a binary — handle both shapes.
        suggestions =
          cond do
            is_list(body) ->
              case body do
                [_query, suggestions | _] when is_list(suggestions) -> suggestions
                _ -> []
              end

            is_binary(body) ->
              case Jason.decode(body) do
                {:ok, [_query, suggestions | _]} when is_list(suggestions) -> suggestions
                _ -> []
              end

            true ->
              []
          end

        Enum.filter(suggestions, &is_binary/1)

      _ ->
        []
    end
  rescue
    _ -> []
  end

  # Route through the shared ExHub proxy (`:exhub, :proxy` URL string) when
  # configured, converting to Mint's {scheme, host, port, opts} tuple form.
  defp put_proxy_options(options) do
    case proxy_connect_options() do
      nil -> options
      proxy -> Keyword.put(options, :connect_options, proxy: proxy)
    end
  end

  defp proxy_connect_options do
    case Application.get_env(:exhub, :proxy, "") do
      url when is_binary(url) and url != "" ->
        uri = URI.parse(url)

        if is_binary(uri.host) and uri.host != "" do
          scheme = if uri.scheme == "https", do: :https, else: :http
          port = uri.port || default_port(scheme)
          {scheme, String.to_charlist(uri.host), port, []}
        else
          nil
        end

      _ ->
        nil
    end
  end

  defp default_port(:https), do: 443
  defp default_port(_), do: 80

  defp valid_url?(url) do
    # Must be a single word (no spaces)
    if String.contains?(url, " ") do
      false
    else
      Regex.match?(
        ~r/^(?:http|ftp)s?:\/\/(?:(?:[A-Z0-9](?:[A-Z0-9-]{0,61}[A-Z0-9])?\.)+(?:[A-Z]{2,6}\.?|[A-Z0-9-]{2,}\.?)|localhost|\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3})(?::\d+)?(?:\/?|[\/?]\S+)$/i,
        url
      ) or String.ends_with?(url, ".html")
    end
  end

  defp ensure_scheme(url) do
    if String.starts_with?(url, ["http://", "https://", "ftp://", "ftps://"]) do
      url
    else
      "http://" <> url
    end
  end
end
