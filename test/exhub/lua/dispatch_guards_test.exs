defmodule Exhub.Lua.DispatchGuardsTest do
  @moduledoc """
  Regression for the `code_mode` bypass: the Lua sandbox dispatches through
  `Exhub.MCP.Hub.BuiltInRegistry.call_tool/3`, which executes a tool's
  `handler.execute/2` directly and therefore never runs the server's
  `handle_request/2` — where `Exhub.MCP.Desktop.CostGuard` used to be the only
  gate. A whole-home `search_files` written as a Lua snippet is now refused
  before it scans anything.
  """

  use ExUnit.Case, async: true

  alias Exhub.Lua.Engine
  alias Exhub.MCP.Hub.BuiltInRegistry

  setup do
    Exhub.MCP.Desktop.CostGuard.clear_cache()
    Exhub.MCP.Desktop.SecretGuard.clear_cache()
    :ok
  end

  defp run(code, opts \\ []) do
    Engine.run(
      code,
      BuiltInRegistry.list_tools("desktop"),
      Keyword.merge([call_fun: &BuiltInRegistry.call_tool/3, timeout_ms: 60_000], opts)
    )
  end

  test "a broad search_files through parallel_all is refused" do
    code = """
    local r = parallel_all({
      {server = "desktop", tool = "search_files", args = {path = "~", search_type = "content", pattern = "maxConcurrentRunsPerUser", ignore_case = true}},
    })
    return r
    """

    assert {:ok, text} = run(code)
    assert text =~ "Refused"
    assert text =~ "\"ok\":false"
    refute text =~ "maxConcurrentRunsPerUser\",\"matches"
  end

  test "a broad search_files through the nested bridge raises a ToolFailure" do
    code = """
    return desktop.search_files({path = "~", search_type = "content", pattern = "x"})
    """

    assert {:error, message} = run(code)
    assert message =~ "ToolFailure: desktop.search_files failed"
    assert message =~ "Refused"
  end

  test "a whole-home shell scan is refused" do
    code = """
    return desktop.execute_command({command = "find ~ -type f"})
    """

    assert {:error, message} = run(code)
    assert message =~ "Refused"
  end

  test "a scoped call still works" do
    dir = Path.join(System.tmp_dir!(), "exhub_lua_gate_#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(dir, "nested"))
    File.write!(Path.join(dir, "nested/note.txt"), "a needle here\n")
    on_exit(fn -> File.rm_rf(dir) end)

    code = """
    local r = desktop.list_directory({path = "#{dir}", depth = 2})
    return r.content[1].text
    """

    assert {:ok, text} = run(code)
    assert text =~ "note.txt"
    refute text =~ "Refused"
  end
end
