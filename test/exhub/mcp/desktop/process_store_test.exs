defmodule Exhub.MCP.Desktop.ProcessStoreTest do
  @moduledoc """
  Covers the store's bounded output buffer and orphan protection.

  Both matter because `execute_command` now returns while a command is still
  running: a runaway command (`find /`, `yes`) must not be able to grow one
  buffer without bound — every `append_output` cast copies the whole binary, so
  an unbounded buffer makes the GenServer fall behind and every other desktop
  tool call queue behind it — and a request that dies mid-registration must not
  leave a phantom `:running` entry.
  """

  use ExUnit.Case, async: false

  alias Exhub.MCP.Desktop.ProcessStore

  setup do
    ensure_store()
    :ok
  end

  defp ensure_store do
    case GenServer.whereis(ProcessStore) do
      nil -> {:ok, _} = GenServer.start(ProcessStore, :ok, name: ProcessStore)
      _ -> :ok
    end
  end

  defp unique_id, do: "proc_test_#{System.unique_integer([:positive, :monotonic])}"

  describe "bounded output buffer" do
    test "drops the oldest whole lines once the cap is exceeded" do
      id = unique_id()
      {:ok, _} = ProcessStore.register(id, %{command: "yes"})
      on_exit(fn -> ProcessStore.remove(id) end)

      line = String.duplicate("x", 49_999) <> "\n"
      # 30 lines of 50 KB = 1.5 MB, comfortably past the 1 MB cap.
      Enum.each(1..30, fn _ -> ProcessStore.append_output(id, line) end)

      {:ok, result} = ProcessStore.get_output(id)

      assert result.truncated
      assert result.dropped_bytes > 0
      assert result.dropped_lines > 0
      assert byte_size(result.output) <= ProcessStore.max_output_bytes()
      assert String.starts_with?(result.output, "x")
      assert String.ends_with?(result.output, "\n")

      # Whole lines only: nothing retained is a partial line.
      result.output
      |> String.split("\n", trim: true)
      |> Enum.each(fn kept -> assert kept == String.duplicate("x", 49_999) end)
    end

    test "leaves a buffer under the cap untouched" do
      id = unique_id()
      {:ok, _} = ProcessStore.register(id, %{command: "echo"})
      on_exit(fn -> ProcessStore.remove(id) end)

      ProcessStore.append_output(id, "hello\n")

      {:ok, result} = ProcessStore.get_output(id)

      refute result.truncated
      assert result.dropped_bytes == 0
      assert result.dropped_lines == 0
      assert result.output == "hello\n"
    end

    test "trims a single line longer than the cap byte-wise" do
      id = unique_id()
      {:ok, _} = ProcessStore.register(id, %{command: "yes"})
      on_exit(fn -> ProcessStore.remove(id) end)

      ProcessStore.append_output(id, String.duplicate("y", ProcessStore.max_output_bytes() + 10))

      {:ok, result} = ProcessStore.get_output(id)

      assert result.truncated
      assert byte_size(result.output) <= ProcessStore.max_output_bytes()
      assert result.dropped_bytes == 10
    end
  end

  describe "registration" do
    test "refuses a caller that is already dead" do
      dead = spawn(fn -> :ok end)
      ref = Process.monitor(dead)
      assert_receive {:DOWN, ^ref, :process, ^dead, _}
      id = unique_id()

      assert {:reply, {:error, :caller_gone}, _state} =
               ProcessStore.handle_call(
                 {:register, id, %{command: "sleep 1"}},
                 {dead, make_ref()},
                 %{}
               )

      assert ProcessStore.get(id) == nil
    end
  end
end
