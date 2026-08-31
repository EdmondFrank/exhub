defmodule Exhub.SocketHandlerTest do
  @moduledoc """
  Unit tests for `Exhub.SocketHandler` websocket callbacks.

  Exercises the module functions directly without booting the ExHub
  application (`mix test --no-start`), so the Registry the handler
  dispatches into is started manually.

  The core regression covered here: a slow response handler (e.g. a long
  LLM translation/chat call) must not block the Cowboy websocket handler
  process — pings, incoming frames, and results broadcast via
  `Exhub.send_message` all share that process's mailbox.
  """

  use ExUnit.Case, async: false

  defmodule NilHandler do
    @moduledoc false
    def call(_message), do: :ok
  end

  defmodule SlowHandler do
    @moduledoc false
    def call(message) do
      Process.sleep(200)
      "handled:#{message}"
    end
  end

  setup do
    start_supervised!({Registry, keys: :unique, name: Exhub.Registry})
    Application.put_env(:exhub, :response_handler, NilHandler)
    on_exit(fn -> Application.delete_env(:exhub, :response_handler) end)
    :ok
  end

  test "websocket_handle does not block on a slow response handler" do
    Application.put_env(:exhub, :response_handler, SlowHandler)

    parent = self()

    spawn(fn ->
      # self() here is the simulated Cowboy websocket handler process.
      started = System.monotonic_time(:millisecond)
      result = Exhub.SocketHandler.websocket_handle({:text, "translate big text"}, %{})
      elapsed = System.monotonic_time(:millisecond) - started
      send(parent, {:handle_result, result, elapsed})

      # The slow handler finishes later and sends its reply back here.
      receive do
        {:websocket_response, response} -> send(parent, {:got_response, response})
      after
        2_000 -> send(parent, {:got_response, :timeout})
      end
    end)

    # Handle must return long before the 200ms handler completes.
    assert_receive {:handle_result, {:ok, %{}}, elapsed}, 500
    assert elapsed < 150

    # The reply is still delivered asynchronously, matching the old
    # synchronous semantics (binary response -> same text frame).
    assert_receive {:got_response, "handled:translate big text"}, 2_000

    # The same frame is produced by the websocket_info clause that Cowboy
    # invokes to flush the pending reply.
    state = %{}

    assert {:reply, {:text, "handled:translate big text"}, ^state} =
             Exhub.SocketHandler.websocket_info(
               {:websocket_response, "handled:translate big text"},
               state
             )
  end

  test "non-binary handler results are replied as nil like before" do
    state = %{}

    # Union handler returns :ok, matching every built-in response handler.
    assert {:reply, {:text, "nil"}, ^state} =
             Exhub.SocketHandler.websocket_info({:websocket_response, :ok}, state)

    assert {:reply, {:text, "nil"}, ^state} =
             Exhub.SocketHandler.websocket_info({:websocket_response, nil}, state)
  end

  test "emacs_response messages are still handled synchronously" do
    # Fast dispatch path: reply immediately, no task spawned.
    assert {:reply, {:text, "nil"}, %{}} =
             Exhub.SocketHandler.websocket_handle(
               {:text, ~s(["emacs_response", "42", "output"])},
               %{}
             )
  end
end
