defmodule Exhub.ProxyClient do
  @moduledoc """
  Streaming `HTTPoison` client for `ReverseProxyPlug`, used by
  `Exhub.ProxyPlug` for every proxied LLM request.

  ## Why not the vendored adapter?

  `ReverseProxyPlug.HTTPClient.Adapters.HTTPoison.request_stream/1` returns a
  bare `Stream.unfold/2` over the mailbox:

      Stream.unfold(nil, fn _ ->
        receive do
          %HTTPoison.AsyncChunk{chunk: chunk} -> {{:chunk, chunk}, nil}
          %HTTPoison.AsyncEnd{} -> nil
          ...
        end
      end)

  It discards the `%HTTPoison.AsyncResponse{id: id}` handle, so when the
  consumer **halts** — `ReverseProxyPlug.process_response/4` does exactly that
  as soon as `Plug.Conn.chunk/2` fails because the downstream client went away
  — nothing closes the upstream request. The slot is returned only when the
  owning request process dies (hackney's monitor), or never, if the socket is
  still pinned by a keep-alive chunk. For long completions that is a
  connection slot held hostage by a client that already hang up.

  This adapter keeps the handle and wraps the receive loop in
  `Stream.resource/3`, whose `after` clause always runs — on completion, on
  halt, and on a raise — and calls `:hackney.close/1`. Every stream therefore
  returns its connection to the pool deterministically.

  Streams are also pinned to the dedicated `:exhub_proxy` pool (see
  `Exhub.HTTP.Pools`) so long completions cannot starve the short-lived tool
  requests that share hackney's `:default` pool.
  """

  @behaviour ReverseProxyPlug.HTTPClient

  alias ReverseProxyPlug.HTTPClient
  alias ReverseProxyPlug.HTTPClient.{Error, Request}

  # Delay before a stalled upstream is surfaced as an error chunk. Generous:
  # reasoning models routinely pause for tens of seconds between chunks, but a
  # silent socket must not hold a slot forever.
  @recv_timeout 120_000
  @connect_timeout 8_000

  @impl true
  def request(%Request{} = req) do
    req
    |> Map.update!(:options, &pool_options/1)
    |> then(&HTTPClient.Adapters.HTTPoison.request/1)
  end

  @impl true
  def request_stream(%Request{} = req) do
    options =
      req.options
      |> pool_options()
      |> Keyword.put(:stream_to, self())
      |> Keyword.put_new(:timeout, @connect_timeout)
      |> Keyword.put_new(:recv_timeout, @recv_timeout)

    case HTTPoison.request(to_httpoison_request(req, options)) do
      {:ok, %HTTPoison.AsyncResponse{id: id}} ->
        {:ok, stream(id)}

      {:ok, %HTTPoison.Response{} = resp} ->
        # Upstream answered without streaming (e.g. an error page). Surface it
        # as a one-shot enumerable so ReverseProxyPlug can still proxy it.
        {:ok, [{:status, resp.status_code}, {:headers, resp.headers}, {:chunk, resp.body}]}

      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, %Error{reason: reason}}
    end
  end

  defp to_httpoison_request(%Request{} = req, options) do
    %HTTPoison.Request{
      method: req.method,
      url: req.url,
      headers: req.headers,
      body: req.body,
      params: req.query_params,
      options: options
    }
  end

  defp pool_options(options) do
    pool = Exhub.HTTP.Pools.proxy_pool()

    Keyword.update(options, :hackney, [pool: pool], fn hackney ->
      Keyword.put_new(hackney, :pool, pool)
    end)
  end

  defp stream(id) do
    Stream.resource(
      fn -> :streaming end,
      fn
        :done -> {:halt, :done}
        :streaming -> next_message(id)
      end,
      fn _state -> close(id) end
    )
  end

  defp next_message(id) do
    receive do
      %HTTPoison.AsyncStatus{id: ^id, code: code} ->
        {[{:status, code}], :streaming}

      %HTTPoison.AsyncHeaders{id: ^id, headers: headers} ->
        {[{:headers, headers}], :streaming}

      %HTTPoison.AsyncChunk{id: ^id, chunk: chunk} ->
        {[{:chunk, chunk}], :streaming}

      %HTTPoison.AsyncEnd{id: ^id} ->
        {:halt, :done}

      %HTTPoison.Error{id: ^id, reason: reason} ->
        {[{:error, %Error{reason: reason}}], :done}
    after
      @recv_timeout ->
        {[{:error, %Error{reason: :timeout}}], :done}
    end
  end

  defp close(id) do
    _ = :hackney.close(id)
    :ok
  catch
    _, _ -> :ok
  end
end