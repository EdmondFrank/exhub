defmodule Exhub.MCP.ConcurrentToolDispatcherTest do
  # Sync: the fake SSE handler registers a process under a fixed name.
  use ExUnit.Case, async: false

  import Plug.Conn
  import Plug.Test

  alias Anubis.Server.Response
  alias Exhub.MCP.ConcurrentToolDispatcher

  defmodule EchoTool do
    @moduledoc false

    use Anubis.Server.Component, type: :tool

    alias Anubis.Server.Response

    def name, do: "echo"

    @impl true
    def description, do: "Echo the provided text back."

    schema do
      field(:text, {:required, :string}, description: "Text to echo back")
    end

    @impl true
    def execute(params, frame), do: {:reply, Response.tool() |> Response.text(params.text), frame}
  end

  defmodule EchoServer do
    @moduledoc false

    use Anubis.Server,
      name: "dispatcher-test-server",
      version: "1.0.0",
      capabilities: [:tools]

    component(EchoTool)

    @impl true
    def init(_client_info, frame), do: {:ok, frame}

    @impl true
    def handle_request(request, frame) do
      Exhub.MCP.ServerHelpers.handle_request_with_filtered_tools(__MODULE__, request, frame)
    end
  end

  setup do
    # `mix test --no-start` does not boot the app, so the supervisor the
    # dispatcher executes tools in has to be started here.
    if Process.whereis(Exhub.MCP.ToolTaskSupervisor) == nil do
      start_supervised!({Task.Supervisor, name: Exhub.MCP.ToolTaskSupervisor})
    end

    :ok
  end

  describe "maybe_handle/2 response transport" do
    test "answers tools/call on the POST connection even when a GET SSE stream is attached" do
      session_id = "session-#{System.unique_integer([:positive])}"
      fake_handler = start_fake_sse_handler(session_id)

      assert {:handled, conn} =
               ConcurrentToolDispatcher.maybe_handle(tools_call_conn(session_id),
                 server: EchoServer,
                 request_timeout: 5_000
               )

      # 202 (or an empty body) would strand the response on the GET stream.
      assert conn.status == 200

      assert conn.resp_body |> JSON.decode!() |> get_in(["result", "content"]) == [
               %{"type" => "text", "text" => "hello"}
             ]

      # Nothing is pushed to the standalone stream — the POST carried it.
      refute_received {:sse_message, _}
      assert Process.alive?(fake_handler)
    end

    test "reports invalid arguments as a JSON-RPC error on the POST connection" do
      session_id = "session-#{System.unique_integer([:positive])}"

      assert {:handled, conn} =
               ConcurrentToolDispatcher.maybe_handle(
                 tools_call_conn(session_id, %{"nope" => 1}),
                 server: EchoServer,
                 request_timeout: 5_000
               )

      assert conn.status == 200
      assert %{"error" => %{"code" => -32602}} = JSON.decode!(conn.resp_body)
    end
  end

  defp tools_call_conn(session_id, arguments \\ %{"text" => "hello"}) do
    body =
      JSON.encode!(%{
        "jsonrpc" => "2.0",
        "id" => 42,
        "method" => "tools/call",
        "params" => %{"name" => "echo", "arguments" => arguments}
      })

    conn(:post, "/dispatcher-test/mcp", body)
    |> put_req_header("content-type", "application/json")
    |> put_req_header("accept", "application/json, text/event-stream")
    |> put_req_header("mcp-session-id", session_id)
    |> Plug.Parsers.call(Plug.Parsers.init(parsers: [:json], json_decoder: Jason))
  end

  # Stands in for Anubis.Server.Transport.StreamableHTTP: answers the dispatcher's
  # `get_sse_handler` lookup and forwards any pushed message to the test process.
  defp start_fake_sse_handler(session_id) do
    parent = self()
    transport = Anubis.Server.Registry.transport_name(EchoServer, :streamable_http)

    pid =
      spawn_link(fn ->
        Process.register(self(), transport)
        fake_sse_loop(session_id, parent)
      end)

    wait_until_registered(transport, pid)
    on_exit(fn -> if Process.alive?(pid), do: Process.exit(pid, :kill) end)

    pid
  end

  defp fake_sse_loop(session_id, parent) do
    receive do
      {:"$gen_call", from, {:get_sse_handler, ^session_id}} ->
        GenServer.reply(from, self())
        fake_sse_loop(session_id, parent)

      {:"$gen_call", from, _other} ->
        GenServer.reply(from, nil)
        fake_sse_loop(session_id, parent)

      {:sse_message, message} ->
        send(parent, {:sse_message, message})
        fake_sse_loop(session_id, parent)
    end
  end

  defp wait_until_registered(transport, pid, attempts \\ 100)

  defp wait_until_registered(transport, pid, attempts) do
    cond do
      Process.whereis(transport) == pid ->
        :ok

      attempts == 0 ->
        flunk("fake SSE handler never registered as #{inspect(transport)}")

      true ->
        Process.sleep(5)
        wait_until_registered(transport, pid, attempts - 1)
    end
  end
end
