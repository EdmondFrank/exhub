defmodule Exhub.MCP.Hub.DispatchGuardsTest do
  # Not async: the override tests write the module's application env.
  use ExUnit.Case, async: false

  alias Exhub.MCP.Desktop.CostGuard
  alias Exhub.MCP.Desktop.SecretGuard
  alias Exhub.MCP.Hub.DispatchGuards

  setup do
    CostGuard.clear_cache()
    SecretGuard.clear_cache()
    previous = Application.get_env(:exhub, DispatchGuards)

    on_exit(fn ->
      if previous == nil do
        Application.delete_env(:exhub, DispatchGuards)
      else
        Application.put_env(:exhub, DispatchGuards, previous)
      end
    end)
  end

  describe "servers/0" do
    test "defaults to the desktop server, secret gate first" do
      assert DispatchGuards.servers() == %{"desktop" => [SecretGuard, CostGuard]}
    end

    test "config replaces the mapping" do
      Application.put_env(:exhub, DispatchGuards, servers: %{"lua" => [CostGuard]})

      assert DispatchGuards.servers() == %{"lua" => [CostGuard]}
      assert :ok = DispatchGuards.check("desktop", "search_files", %{"path" => "~"})
      assert {:refuse, _} = DispatchGuards.check("lua", "search_files", %{"path" => "~"})
    end
  end

  describe "check/3" do
    test "refuses a whole-home scan on the desktop server" do
      assert {:refuse, message} =
               DispatchGuards.check("desktop", "search_files", %{
                 "path" => "~",
                 "search_type" => "content",
                 "pattern" => "maxConcurrentRunsPerUser",
                 "ignore_case" => true
               })

      assert message =~ "scan the entire home directory"
    end

    test "refuses a whole-disk shell scan on the desktop server" do
      assert {:refuse, _} =
               DispatchGuards.check("desktop", "execute_command", %{"command" => "find ~ -type f"})
    end

    test "lets a scoped invocation through" do
      assert :ok = DispatchGuards.check("desktop", "search_files", %{"path" => "~/projects/repo"})
      assert :ok = DispatchGuards.check("desktop", "read_file", %{"path" => "/etc/hosts"})
    end

    test "does not gate servers outside the mapping" do
      assert :ok = DispatchGuards.check("brain", "search_files", %{"path" => "~"})
    end

    test "a non-binary server or tool name fails open" do
      assert :ok = DispatchGuards.check(nil, "search_files", %{"path" => "~"})
      assert :ok = DispatchGuards.check("desktop", nil, %{"path" => "~"})
    end

    test "accepts the server module instead of the route name" do
      assert {:refuse, _} =
               DispatchGuards.check(Exhub.MCP.DesktopServer, "search_files", %{"path" => "~"})

      assert :ok =
               DispatchGuards.check(Exhub.MCP.DesktopServer, "read_file", %{
                 "path" => "/etc/hosts"
               })
    end

    test "fails open for a server module the registry or the mapping lacks" do
      assert :ok = DispatchGuards.check(Exhub.MCP.TimeServer, "search_files", %{"path" => "~"})

      assert :ok =
               DispatchGuards.check(Exhub.MCP.NotARealServer, "search_files", %{"path" => "~"})
    end

    test "an empty mapping disables funnel gating" do
      Application.put_env(:exhub, DispatchGuards, servers: %{})

      assert :ok = DispatchGuards.check("desktop", "search_files", %{"path" => "~"})
    end
  end

  describe "refuse_result/1" do
    test "wraps the message as an MCP tool error" do
      result = DispatchGuards.refuse_result("Refused: too broad")

      assert result["isError"] == true
      assert [%{"type" => "text", "text" => text} | _] = result["content"]
      assert text =~ "Refused: too broad"
    end
  end
end
