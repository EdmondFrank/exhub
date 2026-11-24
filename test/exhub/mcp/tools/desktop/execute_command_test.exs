defmodule Exhub.MCP.Tools.Desktop.ExecuteCommandTest do
  @moduledoc """
  Tests for the merged `execute_command` tool.

  It covers both behaviours that used to live in separate tools:

    * a command that finishes inside the wait window returns its stdout,
      stderr and exit code inline (the old `execute_command`);
    * a command that does not returns a `process_id` with the output captured so
      far, readable through `read_process_output` (the old `start_process`);
    * `async: false` keeps the historical blocking behaviour, including killing
      the command at `timeout_ms`;
    * the interactive/PTY backends return immediately and the child outlives the
      transient `tools/call` task that started it.
  """

  use ExUnit.Case, async: false

  alias Exhub.MCP.Desktop.ProcessStore
  alias Exhub.MCP.Tools.Desktop.ExecuteCommand
  alias Exhub.MCP.Tools.Desktop.InteractWithProcess
  alias Exhub.MCP.Tools.Desktop.KillProcess
  alias Exhub.MCP.Tools.Desktop.ReadProcessOutput

  @tmp_dir System.tmp_dir!()

  # Exile spawns a watcher under its own supervision tree, so the :exile app
  # must be running; ProcessStore names a global GenServer. We start only what
  # is needed (not the full ExHub app) so these tests still work with
  # `mix test --no-start`.
  setup do
    {:ok, _} = Application.ensure_all_started(:exile)
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

  describe "waiting for completion (default async mode)" do
    test "returns stdout and exit code on success" do
      {:reply, resp, %{}} =
        ExecuteCommand.execute(%{command: "echo hello", working_dir: @tmp_dir}, %{})

      refute resp.isError
      text = text(resp)
      assert text =~ "hello"
      assert text =~ "exit_code"
      refute text =~ "process_id"
    end

    test "captures stderr and non-zero exit code" do
      params = %{command: "echo oops 1>&2; exit 3", working_dir: @tmp_dir}

      {:reply, resp, %{}} = ExecuteCommand.execute(params, %{})

      refute resp.isError
      text = text(resp)
      assert text =~ "oops"
      assert text =~ "3"
    end

    test "answers with a process_id when the command outlives wait_ms" do
      # Emit some output, then hang well past the wait window.
      params = %{
        command: "printf 'line1\\nline2\\n'; sleep 30",
        wait_ms: 400,
        working_dir: @tmp_dir
      }

      {:reply, resp, %{}} = ExecuteCommand.execute(params, %{})

      refute resp.isError
      assert field(resp, "status") == "running"
      assert is_integer(field(resp, "pid"))
      assert field(resp, "stdout") =~ "line1"
      assert field(resp, "stdout") =~ "line2"
      assert field(resp, "hint") =~ "read_process_output"

      process_id = field(resp, "process_id")
      on_exit(fn -> safe_kill(process_id) end)

      # The same output is still readable through the process store, and the
      # process is still running.
      {:reply, read, %{}} = ReadProcessOutput.execute(%{process_id: process_id}, %{})
      assert field(read, "status") == "running"
      assert field(read, "output") =~ "line1"
    end

    test "records the real child OS pid, never a look-alike" do
      params = %{command: "sleep 30", wait_ms: 300, working_dir: @tmp_dir}

      {:reply, resp, %{}} = ExecuteCommand.execute(params, %{})

      refute resp.isError
      process_id = field(resp, "process_id")
      pid = field(resp, "pid")
      on_exit(fn -> safe_kill(process_id) end)

      assert is_integer(pid)

      # `ps` must show our command at that pid (an earlier implementation used
      # `pgrep -f <command>` and could return an unrelated process instead).
      {out, 0} = System.cmd("ps", ["-o", "command=", "-p", to_string(pid)])
      assert out =~ "sleep 30"
    end
  end

  describe "async: false (blocking mode)" do
    test "waits for completion" do
      params = %{command: "sleep 0.2; echo done", async: false, working_dir: @tmp_dir}

      {:reply, resp, %{}} = ExecuteCommand.execute(params, %{})

      refute resp.isError
      assert text(resp) =~ "done"
      assert field(resp, "exit_code") == 0
    end

    test "includes partial output when the command times out" do
      params = %{
        command: "printf 'line1\\nline2\\n'; sleep 5",
        async: false,
        timeout_ms: 400,
        working_dir: @tmp_dir
      }

      {:reply, resp, %{}} = ExecuteCommand.execute(params, %{})

      assert resp.isError
      text = text(resp)
      assert text =~ "timed out after 400ms"
      assert text =~ "timed_out"
      assert text =~ "line1"
      assert text =~ "line2"
    end

    test "an explicit timeout_ms without async keeps the blocking behaviour" do
      params = %{command: "sleep 5", timeout_ms: 300, working_dir: @tmp_dir}

      {:reply, resp, %{}} = ExecuteCommand.execute(params, %{})

      assert resp.isError
      assert text(resp) =~ "timed out after 300ms"
    end
  end

  describe "interactive mode" do
    test "returns a process_id immediately" do
      params = %{command: "cat", interactive: true, working_dir: @tmp_dir}

      {:reply, resp, %{}} = ExecuteCommand.execute(params, %{})

      refute resp.isError
      process_id = field(resp, "process_id")
      on_exit(fn -> safe_kill(process_id) end)

      assert is_integer(field(resp, "pid"))
      assert field(resp, "interactive") == true
      assert field(resp, "pty") == true
      assert field(resp, "backend") == "erlexec"
    end

    test "the child survives the process that started it" do
      parent = self()

      # Anubis runs each tools/call in a transient task, so `execute_command`
      # is always called from a process that exits immediately afterwards.
      # Emulate that: start from a throwaway process and assert the child lives.
      starter =
        spawn(fn ->
          {:reply, resp, _} =
            ExecuteCommand.execute(
              %{command: "cat", interactive: true, pty: true, working_dir: @tmp_dir},
              %{}
            )

          send(parent, {:started, field(resp, "process_id"), field(resp, "pid")})
        end)

      assert_receive {:started, process_id, pid}, 10_000
      refute Process.alive?(starter)
      on_exit(fn -> safe_kill(process_id) end)

      assert is_integer(pid)
      assert await_status(process_id, :running, 2_000)

      # The reported pid must actually exist.
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
      # which used to crash the tool with a CaseClauseError.
      {:reply, resp, _} =
        ExecuteCommand.execute(
          %{command: "", interactive: true, pty: true, working_dir: @tmp_dir},
          %{}
        )

      assert resp.isError
      assert text(resp) =~ "Failed to start process"
    end
  end

  defp text(resp), do: Enum.map_join(resp.content, "\n", & &1["text"])

  defp decode_response(resp) do
    try do
      Toon.decode!(text(resp))
    rescue
      _ ->
        {:ok, map} = Jason.decode(text(resp))
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
end
