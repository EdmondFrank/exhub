defmodule Exhub.MCP.Tools.Desktop.StartProcessTest do
  @moduledoc """
  Regression tests for `start_process` process lifecycle.

  Each test pins one bug found by probing the live server:

    * the PTY (erlexec) backend used to monitor the *transient* `tools/call`
      task and stop its child the instant that task exited, so every
      `pty: true` process died immediately and produced no output;
    * the recorded pid for a streaming process was resolved with
      `pgrep -f <command>`, which for a short command matched an unrelated
      process (`ls` reported a root daemon);
    * a failed PTY start returned `{:error, _}` into a `case` with no matching
      clause, raising `CaseClauseError` instead of returning a tool error.
  """

  use ExUnit.Case, async: false

  alias Exhub.MCP.Desktop.ProcessStore
  alias Exhub.MCP.Tools.Desktop.InteractWithProcess
  alias Exhub.MCP.Tools.Desktop.KillProcess
  alias Exhub.MCP.Tools.Desktop.ReadProcessOutput
  alias Exhub.MCP.Tools.Desktop.StartProcess

  setup do
    Application.ensure_all_started(:erlexec)
    ensure_process_store()
    :ok
  end

  defp ensure_process_store do
    case GenServer.whereis(ProcessStore) do
      nil -> {:ok, _} = GenServer.start(ProcessStore, :ok, name: ProcessStore)
      _ -> :ok
    end
  end

  defp response_text(resp) do
    resp.content
    |> Enum.find(&(Map.get(&1, "type") == "text"))
    |> Map.get("text")
  end

  defp decode_response(resp) do
    text = response_text(resp)

    try do
      Toon.decode!(text)
    rescue
      _ ->
        {:ok, map} = Jason.decode(text)
        map
    end
  end

  defp field(resp, name), do: decode_response(resp)[name]

  defp safe_kill(process_id) do
    KillProcess.execute(%{process_id: process_id}, %{})
  rescue
    _ -> :ok
  end

  defp await_status(process_id, status, timeout_ms) do
    deadline = System.monotonic_time(:millisecond) + timeout_ms
    poll_status(process_id, status, deadline)
  end

  defp poll_status(process_id, status, deadline) do
    case ProcessStore.get(process_id) do
      %{status: ^status} ->
        true

      _ ->
        if System.monotonic_time(:millisecond) > deadline do
          false
        else
          Process.sleep(50)
          poll_status(process_id, status, deadline)
        end
    end
  end

  describe "pty: true (erlexec) backend" do
    test "the child survives the process that started it" do
      parent = self()

      # Anubis runs each tools/call in a transient task, so `start_process` is
      # always called from a process that exits immediately afterwards. Emulate
      # that: start from a throwaway process and assert the child lives on.
      starter =
        spawn(fn ->
          {:reply, resp, _} =
            StartProcess.execute(
              %{command: "cat", interactive: true, pty: true, working_dir: System.tmp_dir!()},
              %{}
            )

          send(parent, {:started, field(resp, "process_id"), field(resp, "pid")})
        end)

      assert_receive {:started, process_id, pid}, 10_000
      refute Process.alive?(starter)
      on_exit(fn -> safe_kill(process_id) end)

      assert is_integer(pid)
      assert await_status(process_id, :running, 2_000)

      # The reported pid must actually exist — the bug left it dead with a stale
      # "running" status.
      {_, 0} = System.cmd("ps", ["-o", "pid=", "-p", to_string(pid)])

      {:reply, resp, _} =
        InteractWithProcess.execute(
          %{process_id: process_id, input: "hello\n", wait_seconds: 1},
          %{}
        )

      assert field(resp, "success") == true
      assert field(resp, "auto_newline") == true

      {:reply, read, _} = ReadProcessOutput.execute(%{process_id: process_id}, %{})
      assert field(read, "output") =~ "hello"
    end

    test "a failed PTY start returns an error response instead of raising" do
      # erlexec rejects an empty command (`{:error, ~c"empty command provided"}`),
      # which used to crash execute/2 with a CaseClauseError.
      {:reply, resp, _} =
        StartProcess.execute(
          %{command: "", interactive: true, pty: true, working_dir: System.tmp_dir!()},
          %{}
        )

      assert resp.isError
      assert response_text(resp) =~ "Failed to start process"
    end
  end

  describe "streaming backend" do
    test "records the real child OS pid, never a look-alike" do
      {:reply, resp, _} =
        StartProcess.execute(%{command: "sleep 30", working_dir: System.tmp_dir!()}, %{})

      refute resp.isError
      process_id = field(resp, "process_id")
      pid = field(resp, "pid")
      on_exit(fn -> safe_kill(process_id) end)

      assert is_integer(pid)

      # `ps` must show our command at that pid (the old pgrep-based lookup could
      # return an unrelated process instead).
      {out, 0} = System.cmd("ps", ["-o", "command=", "-p", to_string(pid)])
      assert out =~ "sleep 30"
    end
  end
end
