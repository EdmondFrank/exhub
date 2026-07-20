defmodule Exhub.MCP.Tools.Desktop.PtyInlcrTest do
  @moduledoc """
  Tests that PTY processes started with `pty: true` have INLCR disabled.

  INLCR (Input Newline to Carriage Return) translates incoming `\\n` to `\\r`,
  which breaks line terminators for SSH, REPLs, and other TTY-dependent programs.
  The fix uses `{:pty, [{:inlcr, false}]}` instead of the bare `:pty` atom.
  """

  use ExUnit.Case, async: false

  alias Exhub.MCP.Tools.Desktop.StartProcess
  alias Exhub.MCP.Tools.Desktop.InteractWithProcess
  alias Exhub.MCP.Tools.Desktop.ReadProcessOutput
  alias Exhub.MCP.Tools.Desktop.KillProcess
  alias Exhub.MCP.Desktop.ProcessStore

  # ==========================================================================
  # Helpers
  # ==========================================================================

  defp ensure_erlexec do
    Application.ensure_all_started(:erlexec)
  end

  defp ensure_process_store do
    case GenServer.whereis(ProcessStore) do
      nil -> {:ok, _} = GenServer.start(ProcessStore, :ok, name: ProcessStore)
      _ -> :ok
    end
  end

  defp safe_kill(process_id) do
    try do
      frame = %{}
      KillProcess.execute(%{process_id: process_id}, frame)
    rescue
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

  defp extract_field(resp, field) do
    decode_response(resp)[field]
  end

  defp wait_for_output(process_id, pattern, timeout_ms) when is_binary(pattern) do
    deadline = System.monotonic_time(:millisecond) + timeout_ms
    poll_output(process_id, pattern, deadline)
  end

  defp poll_output(process_id, pattern, deadline) do
    if System.monotonic_time(:millisecond) > deadline do
      nil
    else
      frame = %{}

      {:reply, resp, ^frame} =
        ReadProcessOutput.execute(%{process_id: process_id, offset: 0}, frame)

      output = extract_field(resp, "output") || ""

      if String.contains?(output, pattern) do
        {:ok, output}
      else
        Process.sleep(200)
        poll_output(process_id, pattern, deadline)
      end
    end
  end

  # ==========================================================================
  # Tests
  # ==========================================================================

  describe "PTY terminal settings" do
    setup do
      ensure_erlexec()
      ensure_process_store()
      :ok
    end

    test "inlcr is disabled when pty: true" do
      frame = %{}

      # Start an interactive shell with PTY
      {:reply, resp, ^frame} =
        StartProcess.execute(
          %{command: "sh", interactive: true, pty: true, working_dir: System.tmp_dir!()},
          frame
        )

      refute resp.isError, "start_process failed: #{inspect(resp.content)}"

      process_id = extract_field(resp, "process_id")
      assert process_id, "No process_id in response"
      assert extract_field(resp, "pty") == true

      on_exit(fn -> safe_kill(process_id) end)

      # Wait for the shell to be ready, then send `stty -a`
      result = wait_for_output(process_id, "$", 10_000)
      assert result, "Timed out waiting for shell prompt"

      # Send `stty -a` to inspect terminal settings
      {:reply, _resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: process_id, input: "stty -a\n"},
          frame
        )

      # Wait for stty output
      {:ok, output} = wait_for_output(process_id, "speed", 5_000)

      # INLCR disabled shows as `-inlcr` in stty output.
      # If it were enabled, it would show `inlcr` (without the minus prefix).
      assert String.contains?(output, "-inlcr"),
             "Expected -inlcr (disabled) in stty output, got:\n#{output}"

      refute output =~ ~r/(?<!-)inlcr(?!)/,
             "inlcr appears to be enabled (without - prefix) in stty output:\n#{output}"
    end
  end
end
