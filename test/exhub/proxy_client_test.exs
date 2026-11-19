defmodule Exhub.ProxyClientTest do
  @moduledoc """
  Regression tests for `Exhub.ProxyClient`: the streaming adapter must hand the
  hackney connection back to its pool even when the consumer halts early (a
  downstream client that disconnects mid-completion).

  No application boot is needed — a raw `:gen_tcp` server speaks just enough
  HTTP/1.1 chunked encoding to drive the client.
  """

  use ExUnit.Case, async: false

  alias Exhub.ProxyClient
  alias ReverseProxyPlug.HTTPClient.Request

  setup_all do
    {:ok, _apps} = Application.ensure_all_started(:hackney)
    Exhub.HTTP.Pools.setup()
    :ok
  end

  describe "pool sizing" do
    test "proxy traffic gets a dedicated pool and the shared pool is widened" do
      assert Exhub.HTTP.Pools.max_connections(Exhub.HTTP.Pools.proxy_pool()) == 200
      assert Exhub.HTTP.Pools.max_connections(:default) == 200
    end

    test "setup/0 resizes a pool hackney already auto-created" do
      :hackney_pool.set_max_connections(Exhub.HTTP.Pools.proxy_pool(), 5)
      assert Exhub.HTTP.Pools.max_connections(Exhub.HTTP.Pools.proxy_pool()) == 5

      Exhub.HTTP.Pools.setup()

      assert Exhub.HTTP.Pools.max_connections(Exhub.HTTP.Pools.proxy_pool()) == 200
    end
  end

  describe "request_stream/1" do
    test "yields status, headers and chunks, then releases the slot" do
      port = start_streaming_server(chunks: ["alpha", "beta"], delay: 0, finish: true)

      {:ok, stream} = ProxyClient.request_stream(request(port))
      elements = Enum.to_list(stream)

      assert {:status, 200} in elements
      assert Enum.any?(elements, &match?({:headers, _}, &1))
      assert Enum.any?(elements, &(&1 == {:chunk, "alpha"}))
      assert Enum.any?(elements, &(&1 == {:chunk, "beta"}))

      assert_pool_released()
    end

    test "a halted stream releases its connection slot" do
      port = start_streaming_server(chunks: List.duplicate("x", 400), delay: 5, finish: false)

      {:ok, stream} = ProxyClient.request_stream(request(port))

      # Halt while the upstream is still streaming: this is the case the
      # vendored adapter leaked (it discarded the hackney ref, so nothing
      # could close the request).
      assert [_, _] = Enum.take(stream, 2)

      assert_pool_released()
    end
  end

  # --- helpers ---

  defp request(port) do
    %Request{
      method: :get,
      url: "http://127.0.0.1:#{port}/stream",
      headers: [],
      body: "",
      options: []
    }
  end

  defp assert_pool_released(attempts \\ 80) do
    stats = :hackney_pool.get_stats(Exhub.HTTP.Pools.proxy_pool())

    cond do
      stats[:in_use_count] == 0 ->
        :ok

      attempts == 0 ->
        flunk("hackney pool slot was not released: #{inspect(stats)}")

      true ->
        Process.sleep(25)
        assert_pool_released(attempts - 1)
    end
  end

  defp start_streaming_server(opts) do
    chunks = Keyword.fetch!(opts, :chunks)
    delay = Keyword.get(opts, :delay, 0)
    finish? = Keyword.get(opts, :finish, true)

    {:ok, listen} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
    {:ok, port} = :inet.port(listen)

    server = spawn(fn -> accept_loop(listen, chunks, delay, finish?) end)

    on_exit(fn ->
      :gen_tcp.close(listen)
      Process.exit(server, :kill)
    end)

    port
  end

  defp accept_loop(listen, chunks, delay, finish?) do
    case :gen_tcp.accept(listen) do
      {:ok, socket} ->
        :ok =
          :gen_tcp.send(
            socket,
            "HTTP/1.1 200 OK\r\ncontent-type: text/event-stream\r\ntransfer-encoding: chunked\r\n\r\n"
          )

        send_chunks(socket, chunks, delay, finish?)
        accept_loop(listen, chunks, delay, finish?)

      _ ->
        :ok
    end
  end

  defp send_chunks(_socket, [], _delay, false), do: :ok

  defp send_chunks(socket, [], _delay, true) do
    _ = :gen_tcp.send(socket, "0\r\n\r\n")
    :gen_tcp.close(socket)
  end

  defp send_chunks(socket, [chunk | rest], delay, finish?) do
    if delay > 0, do: Process.sleep(delay)
    frame = "#{Integer.to_string(byte_size(chunk), 16)}\r\n#{chunk}\r\n"

    case :gen_tcp.send(socket, frame) do
      :ok -> send_chunks(socket, rest, delay, finish?)
      _ -> :ok
    end
  end
end