defmodule Exhub.LspBridge.ServerTest do
  use ExUnit.Case, async: false

  alias Exhub.LspBridge.{Capabilities, Config, Server}

  @tmp_dir System.tmp_dir!() |> Path.join("exhub_lsp_fake_#{:erlang.unique_integer([:positive])}")
  @script Path.join(@tmp_dir, "fake_lsp.exs")
  @server_name "fake-lsp-#{:erlang.unique_integer([:positive])}"

  # A minimal stdio JSON-RPC server written in Elixir so this test has no
  # external language-server dependency. It answers `initialize`, replies to a
  # server-initiated `workspace/configuration` request from the client, and
  # echoes a fixed hover result for any other request id.
  #
  # Run as a script file (not `-e`, and without `--no-halt`) so that closing the
  # port sends EOF and the child exits instead of orphaning a beam process.
  @fake ~S'''
  defmodule Fake do
    def read_frame do
      case IO.read(:stdio, :line) do
        :eof ->
          nil

        {:error, _} ->
          nil

        line ->
          case Integer.parse(String.replace(line, "Content-Length: ", "")) do
            {len, _} ->
              _crlf = IO.read(:stdio, :line)
              IO.read(:stdio, len)

            _ ->
              read_frame()
          end
      end
    end

    def write(body) do
      IO.write(:stdio, "Content-Length: #{byte_size(body)}\r\n\r\n" <> body)
    end

    def loop do
      case read_frame() do
        nil ->
          :ok

        body ->
          handle(body)
          loop()
      end
    end

    # Pull method/id by regex so map key order does not matter. `initialize`
    # gets a capabilities result plus a server-initiated request; any other
    # *request* (method + id) gets a fixed hover result; notifications and
    # responses are ignored.
    defp handle(body) do
      method =
        case Regex.run(~r/"method"\s*:\s*"([^"]+)"/, body) do
          [_, m] -> m
          _ -> nil
        end

      id =
        case Regex.run(~r/"id"\s*:\s*(\d+)/, body) do
          [_, i] -> i
          _ -> nil
        end

      cond do
        method == "initialize" ->
          write(~s({"jsonrpc":"2.0","id":#{id},"result":{"capabilities":{"hoverProvider":true,"textDocumentSync":{"change":2}}}}))

          # server-initiated request; the client must answer it (we ignore the reply)
          write(
            ~s({"jsonrpc":"2.0","id":99,"method":"workspace/configuration","params":{"items":[{"section":"x"}]}})
          )

        is_binary(id) and is_binary(method) ->
          write(~s({"jsonrpc":"2.0","id":#{id},"result":{"contents":"fake-hover"}}))

        true ->
          :ok
      end
    end
  end

  Fake.loop()
  '''

  setup_all do
    File.mkdir_p!(@tmp_dir)
    File.write!(@script, @fake)
    {:ok, _registry} = start_supervised({Registry, keys: :unique, name: Exhub.LspBridge.Registry})
    on_exit(fn -> File.rm_rf(@tmp_dir) end)
    :ok
  end

  test "runs the initialize handshake, derives capabilities and correlates a request" do
    elixir = System.find_executable("elixir") || flunk("elixir not on PATH")
    key = Server.key(@tmp_dir, @server_name)

    info = %Config{
      name: @server_name,
      command: elixir,
      args: [@script],
      settings: %{}
    }

    spec = %{
      id: Server,
      start: {Server, :start_link, [info, @tmp_dir, [owner: self(), key: key]]},
      restart: :temporary
    }

    {:ok, pid} = start_supervised(spec)
    assert is_pid(pid)

    # initialize result reaches us via the owner callback, already derived.
    assert_receive {:lsp_initialized, ^key, %Capabilities{} = caps}, 10_000
    assert Capabilities.supports?(caps, "hover")
    assert Capabilities.sync_kind(caps) == 2

    # A follow-up request is correlated back to this process by id.
    id = Server.request(key, "textDocument/hover", %{position: %{line: 0}}, self())
    assert is_integer(id)
    assert_receive {:lsp_response, @server_name, ^id, %{"contents" => "fake-hover"}}, 10_000
  end
end
