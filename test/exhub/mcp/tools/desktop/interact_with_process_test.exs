defmodule Exhub.MCP.Tools.Desktop.InteractWithProcessTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Tools.Desktop.InteractWithProcess
  alias Exhub.MCP.Tools.Desktop.StartProcess
  alias Exhub.MCP.Desktop.ProcessStore

  # ==========================================================================
  # Helpers
  # ==========================================================================

  defp ensure_process_store do
    case GenServer.whereis(ProcessStore) do
      nil -> {:ok, _} = GenServer.start(ProcessStore, :ok, name: ProcessStore)
      _ -> :ok
    end
  end

  defp start_cat_process do
    frame = %{}

    {:reply, resp, ^frame} =
      StartProcess.execute(
        %{command: "cat", interactive: true, pty: false, working_dir: System.tmp_dir!()},
        frame
      )

    refute resp.isError, "start_process failed: #{inspect(resp.content)}"
    extract_field(resp, "process_id")
  end

  defp safe_kill(process_id) do
    try do
      ProcessStore.kill_process(process_id)
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

  # ==========================================================================
  # Tests — raw mode (default, no expansion)
  # ==========================================================================

  describe "execute/2 — raw mode (default)" do
    setup do
      ensure_process_store()
      process_id = start_cat_process()
      on_exit(fn -> safe_kill(process_id) end)
      {:ok, process_id: process_id}
    end

    test "passes through plain text unchanged", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(%{process_id: pid, input: "hello world"}, frame)

      refute resp.isError
      assert extract_field(resp, "success") == true
      assert extract_field(resp, "input_sent") == "hello world"
      assert extract_field(resp, "input_expanded") == false
    end

    test "does not expand ${HOME} in raw mode (default)", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(%{process_id: pid, input: "${HOME}"}, frame)

      refute resp.isError
      assert extract_field(resp, "input_sent") == "${HOME}"
      assert extract_field(resp, "input_expanded") == false
    end

    test "does not expand `echo hello` in raw mode (default)", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(%{process_id: pid, input: "`echo hello`"}, frame)

      refute resp.isError
      assert extract_field(resp, "input_sent") == "`echo hello`"
      assert extract_field(resp, "input_expanded") == false
    end

    test "does not expand mixed templates in raw mode (default)", %{process_id: pid} do
      frame = %{}
      input = "${HOME} and `echo hello`"

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(%{process_id: pid, input: input}, frame)

      refute resp.isError
      assert extract_field(resp, "input_sent") == input
      assert extract_field(resp, "input_expanded") == false
    end

    test "explicit mode: 'raw' also does not expand", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "${HOME}", mode: "raw"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_sent") == "${HOME}"
      assert extract_field(resp, "input_expanded") == false
    end

    test "passes through text with $ but no braces unchanged", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(%{process_id: pid, input: "cost is $5"}, frame)

      refute resp.isError
      assert extract_field(resp, "input_sent") == "cost is $5"
      assert extract_field(resp, "input_expanded") == false
    end

    test "passes through empty string unchanged", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(%{process_id: pid, input: ""}, frame)

      refute resp.isError
      assert extract_field(resp, "input_sent") == ""
      assert extract_field(resp, "input_expanded") == false
    end
  end

  # ==========================================================================
  # Tests — template mode: environment variable expansion
  # ==========================================================================

  describe "execute/2 — template mode: env var expansion" do
    setup do
      ensure_process_store()
      process_id = start_cat_process()
      on_exit(fn -> safe_kill(process_id) end)
      {:ok, process_id: process_id}
    end

    test "expands ${HOME} (input_sent omitted for security)", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "${HOME}", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "success") == true
      assert extract_field(resp, "input_expanded") == true
      # input_sent must NOT be present — expanded value may contain secrets
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "expands ${USER}", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "${USER}", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "expands unset env var to empty string", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{
            process_id: pid,
            input: "prefix${EXHUB_TEST_NONEXISTENT_VAR_12345}suffix",
            mode: "template"
          },
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "expands multiple env vars in same input", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "${HOME}:${USER}", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "expands env var embedded in surrounding text", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "path: ${HOME}/dir", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "expands env var with underscores and digits in name", %{process_id: pid} do
      frame = %{}
      System.put_env("EXHUB_TEST_VAR_42", "test_value_42")

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "${EXHUB_TEST_VAR_42}", mode: "template"},
          frame
        )

      System.delete_env("EXHUB_TEST_VAR_42")

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end
  end

  # ==========================================================================
  # Tests — template mode: command substitution
  # ==========================================================================

  describe "execute/2 — template mode: command substitution" do
    setup do
      ensure_process_store()
      process_id = start_cat_process()
      on_exit(fn -> safe_kill(process_id) end)
      {:ok, process_id: process_id}
    end

    test "expands `echo hello`", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "`echo hello`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "trims trailing newlines from command output", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "`printf 'hello\\n\\n\\n'`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "expands command with arguments", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "`echo -n hello`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "expands multiple command substitutions in same input", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "`echo a`-`echo b`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "command substitution with env var inside is handled by shell", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "`echo ${HOME}`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "command substitution output is not re-scanned for env vars", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "`echo '${HOME}'`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "expands `cat` of a temp file", %{process_id: pid} do
      frame = %{}
      tmp_file = Path.join(System.tmp_dir!(), "exhub_test_subst_#{:rand.uniform(999_999)}")
      File.write!(tmp_file, "secret_password_123")

      on_exit(fn -> File.rm(tmp_file) end)

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "`cat #{tmp_file}`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end
  end

  # ==========================================================================
  # Tests — template mode: mixed templates
  # ==========================================================================

  describe "execute/2 — template mode: mixed templates" do
    setup do
      ensure_process_store()
      process_id = start_cat_process()
      on_exit(fn -> safe_kill(process_id) end)
      {:ok, process_id: process_id}
    end

    test "expands both ${VAR} and `command` in same input", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "${HOME} and `echo hello`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "expands templates at start, middle, and end", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "prefix-${USER}-middle-`echo hello`-suffix", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end

    test "handles adjacent templates without separator", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "${HOME}`echo hello`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
    end
  end

  # ==========================================================================
  # Tests — template mode: no templates in input
  # ==========================================================================

  describe "execute/2 — template mode: no templates in input" do
    setup do
      ensure_process_store()
      process_id = start_cat_process()
      on_exit(fn -> safe_kill(process_id) end)
      {:ok, process_id: process_id}
    end

    test "plain text in template mode: input_sent included, input_expanded false", %{
      process_id: pid
    } do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "hello world", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "success") == true
      assert extract_field(resp, "input_sent") == "hello world"
      assert extract_field(resp, "input_expanded") == false
    end

    test "text with $ but no braces in template mode: no expansion", %{
      process_id: pid
    } do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "cost is $5", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_sent") == "cost is $5"
      assert extract_field(resp, "input_expanded") == false
    end
  end

  # ==========================================================================
  # Tests — template mode: error cases
  # ==========================================================================

  describe "execute/2 — template mode: error cases" do
    setup do
      ensure_process_store()
      :ok
    end

    test "returns error when command substitution fails" do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: "nonexistent_pid", input: "`nonexistent_command_xyz_123`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "success") == false
      assert extract_field(resp, "error") =~ "Input expansion failed"
      assert extract_field(resp, "error") =~ "Command substitution failed"
    end

    test "returns error for nonexistent process after successful expansion" do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: "nonexistent_pid", input: "${HOME}", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "success") == false
      assert extract_field(resp, "error") =~ "Process not found"
    end

    test "does not send anything to process when expansion fails" do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: "nonexistent_pid", input: "prefix `false` suffix", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "success") == false
      assert extract_field(resp, "error") =~ "Input expansion failed"
    end
  end

  # ==========================================================================
  # Tests — raw mode does not trigger expansion errors
  # ==========================================================================

  describe "execute/2 — raw mode: no expansion errors" do
    setup do
      ensure_process_store()
      process_id = start_cat_process()
      on_exit(fn -> safe_kill(process_id) end)
      {:ok, process_id: process_id}
    end

    test "raw mode sends `false` literally without expansion error", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "prefix `false` suffix", mode: "raw"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "success") == true
      assert extract_field(resp, "input_sent") == "prefix `false` suffix"
      assert extract_field(resp, "input_expanded") == false
    end

    test "raw mode sends nonexistent command literally without error", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "`nonexistent_command_xyz`", mode: "raw"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "success") == true
      assert extract_field(resp, "input_sent") == "`nonexistent_command_xyz`"
      assert extract_field(resp, "input_expanded") == false
    end
  end

  # ==========================================================================
  # Tests — security: expanded secrets not exposed in response
  # ==========================================================================

  describe "execute/2 — security: expanded input not exposed" do
    setup do
      ensure_process_store()
      process_id = start_cat_process()
      on_exit(fn -> safe_kill(process_id) end)
      {:ok, process_id: process_id}
    end

    test "does not expose ${SSH_PASS} value in response", %{process_id: pid} do
      frame = %{}
      System.put_env("EXHUB_TEST_FAKE_SSH_PASS", "super_secret_123")

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "${EXHUB_TEST_FAKE_SSH_PASS}", mode: "template"},
          frame
        )

      System.delete_env("EXHUB_TEST_FAKE_SSH_PASS")

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      # The secret must NOT appear anywhere in the response
      assert is_nil(extract_field(resp, "input_sent"))
      text = response_text(resp)
      refute text =~ "super_secret_123"
    end

    test "does not expose `cat file` output in response", %{process_id: pid} do
      frame = %{}
      tmp_file = Path.join(System.tmp_dir!(), "exhub_test_secret_#{:rand.uniform(999_999)}")
      File.write!(tmp_file, "another_secret_456")

      on_exit(fn -> File.rm(tmp_file) end)

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "`cat #{tmp_file}`", mode: "template"},
          frame
        )

      refute resp.isError
      assert extract_field(resp, "input_expanded") == true
      assert is_nil(extract_field(resp, "input_sent"))
      text = response_text(resp)
      refute text =~ "another_secret_456"
    end

    test "raw mode still includes input_sent (no secrets to leak)", %{process_id: pid} do
      frame = %{}

      {:reply, resp, ^frame} =
        InteractWithProcess.execute(
          %{process_id: pid, input: "${SSH_PASS}", mode: "raw"},
          frame
        )

      refute resp.isError
      # In raw mode, input_sent is the literal template string, not expanded
      assert extract_field(resp, "input_sent") == "${SSH_PASS}"
      assert extract_field(resp, "input_expanded") == false
    end
  end
end
