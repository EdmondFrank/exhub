defmodule Exhub.MCP.Tools.Desktop.ReadProcessOutputTest do
  use ExUnit.Case, async: false

  alias Exhub.MCP.Desktop.ProcessStore
  alias Exhub.MCP.Tools.Desktop.KillProcess
  alias Exhub.MCP.Tools.Desktop.ReadProcessOutput
  alias Exhub.MCP.Tools.Desktop.StartProcess

  setup do
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

  defp start_lines_process do
    {:reply, resp, _} =
      StartProcess.execute(
        %{command: "printf 'l%02d\\n' 1 2 3 4 5 6 7 8 9 10", working_dir: System.tmp_dir!()},
        %{}
      )

    field(resp, "process_id")
  end

  describe "has_more" do
    test "counts only the lines that remain past the offset" do
      process_id = start_lines_process()
      on_exit(fn -> safe_kill(process_id) end)
      assert await_status(process_id, :completed, 5_000)

      # "l01".."l10" plus the trailing empty line = 11 lines buffered.
      {:reply, read, _} =
        ReadProcessOutput.execute(%{process_id: process_id, offset: 5, length: 3}, %{})

      assert field(read, "lines_returned") == 3
      assert field(read, "output") == "l06\nl07\nl08"
      # 6 lines remain (l06..l10 + trailing ""), so more are available. Reading
      # `offset + length` against an already offset-sliced buffer reported false.
      assert field(read, "has_more") == true

      {:reply, tail, _} =
        ReadProcessOutput.execute(%{process_id: process_id, offset: 5, length: 50}, %{})

      assert field(tail, "has_more") == false
    end

    test "from offset 0 keeps the previous behaviour" do
      process_id = start_lines_process()
      on_exit(fn -> safe_kill(process_id) end)
      assert await_status(process_id, :completed, 5_000)

      {:reply, read, _} =
        ReadProcessOutput.execute(%{process_id: process_id, offset: 0, length: 3}, %{})

      assert field(read, "has_more") == true
    end
  end
end
