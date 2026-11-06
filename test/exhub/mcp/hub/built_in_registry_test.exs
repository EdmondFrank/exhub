defmodule Exhub.MCP.Hub.BuiltInRegistryTest do
  use ExUnit.Case, async: true

  alias Exhub.MCP.Hub.BuiltInRegistry

  setup do
    Exhub.MCP.Desktop.CostGuard.clear_cache()
    Exhub.MCP.Desktop.SecretGuard.clear_cache()
    :ok
  end

  describe "call_tool/3 pre-flight gates" do
    test "refuses a broad desktop scan instead of running it" do
      assert {:ok, result} =
               BuiltInRegistry.call_tool("desktop", "search_files", %{
                 "path" => "~",
                 "search_type" => "content",
                 "pattern" => "maxConcurrentRunsPerUser",
                 "ignore_case" => true
               })

      assert result["isError"] == true
      assert [%{"type" => "text", "text" => text} | _] = result["content"]
      assert text =~ "Refused"
      refute text =~ "count:"
    end

    test "still dispatches an allowed call" do
      assert {:ok, result} =
               BuiltInRegistry.call_tool("desktop", "read_file", %{
                 "path" => "/etc/hosts",
                 "length" => 3
               })

      assert result["isError"] == false
      assert [%{"text" => text} | _] = result["content"]
      assert text != ""
    end

    test "still dispatches a scoped call" do
      dir =
        Path.join(System.tmp_dir!(), "exhub_registry_gate_#{System.unique_integer([:positive])}")

      File.mkdir_p!(Path.join(dir, "nested"))
      File.write!(Path.join(dir, "nested/note.txt"), "a needle in a haystack\n")
      on_exit(fn -> File.rm_rf(dir) end)

      assert {:ok, result} =
               BuiltInRegistry.call_tool("desktop", "list_directory", %{
                 "path" => dir,
                 "depth" => 2
               })

      refute result["isError"]
      assert [%{"text" => text} | _] = result["content"]
      assert text =~ "note.txt"
    end
  end
end
