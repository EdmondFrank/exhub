defmodule Exhub.MCP.Tools.Desktop.ExecuteCommandTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Tools.Desktop.ExecuteCommand

  @tmp_dir System.tmp_dir!()

  # Exile spawns a watcher under its own supervision tree, so the :exile app
  # must be running. We start only :exile (not the full ExHub app) so these
  # tests still work with `mix test --no-start`.
  setup_all do
    {:ok, _} = Application.ensure_all_started(:exile)
    :ok
  end

  describe "execute/2" do
    test "returns stdout and exit code on success" do
      params = %{command: "echo hello", timeout_ms: 5_000, working_dir: @tmp_dir}
      frame = %{}

      {:reply, resp, ^frame} = ExecuteCommand.execute(params, frame)

      refute resp.isError
      text = text(resp)
      assert text =~ "hello"
      assert text =~ "exit_code"
    end

    test "captures stderr and non-zero exit code" do
      params = %{
        command: "echo oops 1>&2; exit 3",
        timeout_ms: 5_000,
        working_dir: @tmp_dir
      }

      {:reply, resp, %{}} = ExecuteCommand.execute(params, %{})

      refute resp.isError
      text = text(resp)
      assert text =~ "oops"
      assert text =~ "3"
    end

    test "includes partial output when the command times out" do
      # Emit some output, then hang well past the timeout.
      params = %{
        command: "printf 'line1\\nline2\\n'; sleep 5",
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
  end

  defp text(resp), do: Enum.map_join(resp.content, "\n", & &1["text"])
end
