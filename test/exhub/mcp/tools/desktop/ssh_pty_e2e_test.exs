defmodule Exhub.MCP.Tools.Desktop.SshPtyE2ETest do
  @moduledoc """
  End-to-end test for SSH PTY interaction via erlexec.

  This test logs into the local machine via SSH using a PTY, executes `ls`
  on the project root directory, and asserts that known files/dirs appear
  in the output.

  ## Prerequisites
  - SSH server (Remote Login) must be enabled on the machine.

  ## Running
      mix test --include e2e_ssh test/exhub/mcp/tools/desktop/ssh_pty_e2e_test.exs
  """

  use ExUnit.Case, async: false

  alias Exhub.MCP.Tools.Desktop.StartProcess
  alias Exhub.MCP.Tools.Desktop.InteractWithProcess
  alias Exhub.MCP.Tools.Desktop.ReadProcessOutput
  alias Exhub.MCP.Tools.Desktop.KillProcess
  alias Exhub.MCP.Desktop.ProcessStore

  @moduletag :e2e_ssh
  @project_dir File.cwd!()
  @ssh_user System.user_home!() |> Path.basename()
  @ssh_command "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null #{@ssh_user}@localhost"
  @marker "# exhub-test-#{System.unique_integer([:positive, :monotonic])}"

  # ---------------------------------------------------------------------------
  # Setup / Teardown
  # ---------------------------------------------------------------------------

  setup do
    # Start :erlexec application (needed for PTY support)
    Application.ensure_all_started(:erlexec)

    # Start ProcessStore GenServer (idempotent)
    start_process_store()

    # Create a temp directory for the SSH key pair (avoids polluting ~/.ssh)
    tmp_dir = Path.join(System.tmp_dir!(), "exhub_ssh_test_#{System.unique_integer([:positive, :monotonic])}")
    File.mkdir_p!(tmp_dir)

    try do
      # Generate a dedicated key pair in the temp directory
      key_path = Path.join(tmp_dir, "id_ed25519")
      {_output, 0} = System.cmd("ssh-keygen", ["-t", "ed25519", "-f", key_path, "-N", "", "-q", "-C", @marker])
      pub_key = String.trim(File.read!(key_path <> ".pub"))

      # Add the public key to authorized_keys with a unique marker comment
      authorized_keys_path = Path.join([System.user_home!(), ".ssh", "authorized_keys"])
      added? = ensure_authorized_key(authorized_keys_path, pub_key, @marker)

      # Wait for SSH server to be ready
      assert ssh_server_running?(),
             "SSH server is not running. Enable Remote Login in System Settings > Sharing."

      on_exit(fn ->
        # Remove our key from authorized_keys if we added it
        if added?, do: remove_authorized_key(authorized_keys_path, @marker)

        # Clean up the temp directory (key pair + known_hosts)
        File.rm_rf!(tmp_dir)

        # Kill any leftover managed processes (ProcessStore may have died with test process)
        try do
          for entry <- ProcessStore.list(), entry.status == :running do
            ProcessStore.kill_process(entry.id)
          end
        rescue
          _ -> :ok
        end
      end)

      {:ok, key_path: key_path}
    rescue
      e ->
        File.rm_rf!(tmp_dir)
        reraise e, __STACKTRACE__
    end
  end

  # ---------------------------------------------------------------------------
  # Tests
  # ---------------------------------------------------------------------------

  test "SSH PTY session: login, ls project dir, assert known files", %{key_path: key_path} do
    # Use the test-specific key for SSH
    ssh_cmd = @ssh_command <> " -i #{key_path}"

    # Step 1: Start SSH session with PTY
    frame = %{}

    {:reply, resp, ^frame} =
      StartProcess.execute(
        %{command: ssh_cmd, interactive: true, pty: true, working_dir: @project_dir},
        frame
      )

    refute resp.isError, "start_process failed: #{inspect(resp.content)}"
    process_id = extract_field(resp, "process_id")
    assert process_id, "No process_id in response"

    # Step 2: Wait for shell prompt (key-based auth → no password needed)
    result = wait_for_output(process_id, ~r/[\$#>%]/, 15_000)
    assert result, "Timed out waiting for SSH shell prompt"

    # Step 3: Send `ls` command to list the project directory
    {:reply, _resp, ^frame} =
      InteractWithProcess.execute(
        %{process_id: process_id, input: "ls #{@project_dir}\n"},
        frame
      )

    # Step 4: Wait for output containing known files
    {:ok, output} = wait_for_output(process_id, "mix.exs", 10_000)

    # Step 5: Assert known project files/dirs appear in the output
    assert output =~ "mix.exs", "Expected 'mix.exs' in SSH ls output"
    assert output =~ "lib", "Expected 'lib' in SSH ls output"
    assert output =~ "test", "Expected 'test' in SSH ls output"
    assert output =~ "config", "Expected 'config' in SSH ls output"

    # Step 6: Clean up — kill the SSH process
    {:reply, _kill_resp, ^frame} =
      KillProcess.execute(%{process_id: process_id}, frame)
  end

  # ---------------------------------------------------------------------------
  # Helpers
  # ---------------------------------------------------------------------------

  defp start_process_store do
    case GenServer.whereis(ProcessStore) do
      nil ->
        {:ok, _pid} = GenServer.start(ProcessStore, :ok, name: ProcessStore)
        :ok

      _pid ->
        :ok
    end
  end

  defp ensure_authorized_key(path, pub_key, marker) do
    ssh_dir = Path.dirname(path)
    File.mkdir_p!(ssh_dir)

    existing =
      case File.read(path) do
        {:ok, content} -> content
        {:error, _} -> ""
      end

    if String.contains?(existing, marker) do
      # Key with our marker already present
      false
    else
      # Append the key with a unique marker comment line for robust removal
      entry = "#{pub_key} #{marker}\n"
      new_content = existing <> entry

      # Atomic write: write to temp file then rename
      tmp_path = path <> ".tmp.#{System.unique_integer([:positive, :monotonic])}"
      File.write!(tmp_path, new_content)
      File.chmod!(tmp_path, 0o600)
      :ok = File.rename(tmp_path, path)
      true
    end
  end

  defp remove_authorized_key(path, marker) do
    case File.read(path) do
      {:ok, content} ->
        updated =
          content
          |> String.split("\n")
          |> Enum.reject(&String.contains?(&1, marker))
          |> Enum.join("\n")

        # Atomic write: write to temp file then rename
        tmp_path = path <> ".tmp.#{System.unique_integer([:positive, :monotonic])}"
        File.write!(tmp_path, updated)
        File.chmod!(tmp_path, 0o600)
        :ok = File.rename(tmp_path, path)

      {:error, _} ->
        :ok
    end
  end

  defp ssh_server_running? do
    {output, 0} =
      System.cmd(
        "ssh",
        ["-o", "BatchMode=yes", "-o", "ConnectTimeout=2", "localhost", "echo", "ok"],
        stderr_to_stdout: true
      )

    String.contains?(output, "ok") or String.contains?(output, "Permission denied") or
      String.contains?(output, "password")
  rescue
    _ -> false
  end

  defp extract_field(resp, field) do
    text = response_text(resp)

    # Try Toon decode first, then JSON
    decoded =
      try do
        Toon.decode!(text)
      rescue
        _ ->
          case Jason.decode(text) do
            {:ok, map} -> map
            _ -> %{}
          end
      end

    Map.get(decoded, field) || Map.get(decoded, to_string(field))
  end

  defp response_text({:reply, resp, _frame}), do: response_text(resp)

  defp response_text(%Anubis.Server.Response{} = resp) do
    resp.content
    |> Enum.find(&(Map.get(&1, "type") == "text"))
    |> Map.get("text")
  end

  defp wait_for_output(process_id, pattern, timeout_ms) when is_binary(pattern) do
    wait_for_output(process_id, Regex.compile!(pattern), timeout_ms)
  end

  defp wait_for_output(process_id, %Regex{} = pattern, timeout_ms) do
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

      text = response_text({:reply, resp, frame})
      output = decode_output_field(text)

      if Regex.match?(pattern, output) do
        {:ok, output}
      else
        Process.sleep(300)
        poll_output(process_id, pattern, deadline)
      end
    end
  end

  defp decode_output_field(text) do
    decoded =
      try do
        Toon.decode!(text)
      rescue
        _ ->
          case Jason.decode(text) do
            {:ok, map} -> map
            _ -> %{}
          end
      end

    Map.get(decoded, "output", "")
  end
end
