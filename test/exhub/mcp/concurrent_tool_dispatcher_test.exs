defmodule Exhub.MCP.ConcurrentToolDispatcherTest do
  @moduledoc """
  Regression for the `/desktop/mcp` gate.

  That route is served by `Exhub.MCP.LazyPlug` →
  `Exhub.MCP.ConcurrentToolDispatcher`, which runs a tool's
  `handler.execute/2` directly in a Task: neither
  `Exhub.MCP.DesktopServer.handle_request/2` nor
  `Exhub.MCP.Hub.BuiltInRegistry.call_tool/3` is on the path, so the dispatcher
  has to consult `Exhub.MCP.Hub.DispatchGuards` itself. Without it a plain POST
  runs a whole-home scan or a credential-shaped command unchecked.
  """

  # Not async: the allow-path test needs `Exhub.MCP.ToolTaskSupervisor`, which
  # `--no-start` runs leave unstarted, so it is started once here.
  use ExUnit.Case, async: false

  alias Exhub.MCP.ConcurrentToolDispatcher
  alias Exhub.MCP.Desktop.CostGuard
  alias Exhub.MCP.Desktop.SecretGuard

  @server Exhub.MCP.DesktopServer

  setup_all do
    if Process.whereis(Exhub.MCP.ToolTaskSupervisor) == nil do
      start_supervised!({Task.Supervisor, name: Exhub.MCP.ToolTaskSupervisor})
    end

    :ok
  end

  setup do
    CostGuard.clear_cache()
    SecretGuard.clear_cache()
    :ok
  end

  test "refuses a broad search_files instead of running it" do
    result =
      post_tools_call("search_files", %{
        "path" => "~",
        "search_type" => "content",
        "pattern" => "maxConcurrentRunsPerUser",
        "ignore_case" => true
      })

    assert %{"isError" => true} = result
    assert text(result) =~ "Refused"
    refute text(result) =~ "matches"
  end

  test "refuses a whole-home shell scan" do
    result = post_tools_call("execute_command", %{"command" => "find ~ -type f"})

    assert %{"isError" => true} = result
    assert text(result) =~ "Refused"
  end

  test "still dispatches a scoped call" do
    dir = Path.join(System.tmp_dir!(), "exhub_ctd_gate_#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, "nested"))
    File.write!(Path.join(dir, "nested/note.txt"), "a needle in a haystack\n")
    on_exit(fn -> File.rm_rf(dir) end)

    result = post_tools_call("list_directory", %{"path" => dir, "depth" => 2})

    assert %{"isError" => false} = result
    assert text(result) =~ "note.txt"
  end

  # Sends a `tools/call` POST through the dispatcher the way `LazyPlug` does and
  # returns the JSON-RPC `result` map.
  defp post_tools_call(tool_name, arguments) do
    body = %{
      "jsonrpc" => "2.0",
      "id" => 1,
      "method" => "tools/call",
      "params" => %{"name" => tool_name, "arguments" => arguments}
    }

    conn =
      Plug.Test.conn(:post, "/desktop/mcp", Jason.encode!(body))
      |> Plug.Conn.put_req_header("content-type", "application/json")
      |> Plug.Conn.put_req_header("mcp-session-id", "test-session")
      |> Map.put(:body_params, body)

    assert {:handled, conn} =
             ConcurrentToolDispatcher.maybe_handle(conn, server: @server, request_timeout: 60_000)

    assert %{"result" => result} = Jason.decode!(conn.resp_body)
    result
  end

  defp text(%{"content" => [%{"text" => text} | _]}), do: text
  defp text(other), do: inspect(other)
end
